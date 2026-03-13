"""CQL2 filter parsing for STAC API queries.

Supports both CQL2-JSON and CQL2-text formats as used by STAC Browser.

CQL2-JSON example::

    {"op": "and", "args": [
        {"op": "=", "args": [{"property": "variable"}, "ssh"]},
        {"op": "=", "args": [{"property": "experiment"}, "basic-001"]}
    ]}

CQL2-text example::

    variable = 'ssh' AND experiment = 'basic-001'

Both formats are parsed into a normalized dict structure that can be
translated to SQL WHERE clauses by the storage layer.
"""

from __future__ import annotations

import json
import re
from typing import Any


# Operator mapping from CQL2 op names to SQL operators
_OP_MAP: dict[str, str] = {
    "eq": "=", "=": "=",
    "neq": "!=", "!=": "!=", "<>": "!=",
    "lt": "<", "<": "<",
    "lte": "<=", "<=": "<=",
    "gt": ">", ">": ">",
    "gte": ">=", ">=": ">=",
    "like": "LIKE",
}

# Operator inversion for reversed operand order (property on right side)
_OP_INVERSE: dict[str, str] = {
    "<": ">", "<=": ">=", ">": "<", ">=": "<=",
    "=": "=", "!=": "!=", "LIKE": "LIKE",
}

# Logical negation of operators (for NOT wrapper)
_OP_NEGATE: dict[str, str] = {
    "=": "!=", "!=": "=",
    "<": ">=", "<=": ">", ">": "<=", ">=": "<",
}

# Well-known property names that are always lowercased
_KNOWN_PROPERTIES = frozenset({
    "variable", "experiment", "model", "component", "collection", "stream"
})

# Regex for parsing CQL2-text expressions
_TEXT_PATTERN = re.compile(
    r"\b(\w+)\s*(<=|>=|<>|!=|<|>|=)\s*(?:'([^']*)'|\"([^\"]*)\"|(\S+?)(?:\s|$|AND|OR))",
    re.IGNORECASE,
)


def _unwrap_literal(val: Any) -> Any:
    """Unwrap CQL2 literal objects into plain Python values.

    CQL2-JSON represents temporal literals as {"timestamp": "..."} or
    {"date": "..."} dicts rather than bare strings.
    """
    if isinstance(val, dict):
        if "timestamp" in val:
            return val["timestamp"]
        if "date" in val:
            return val["date"]
    return val


def parse_cql2_json(expr: dict | None, negate: bool = False) -> dict:
    """Parse a CQL2-JSON filter expression into a filter_props dict.

    Handles AND combinations, OR combinations, comparison operators,
    and NOT wrappers.

    Args:
        expr: CQL2-JSON expression dict
        negate: Whether to negate all operators (for NOT wrapper)

    Returns:
        Dict mapping field name to value where value is:
        - (sql_op, value) for a single condition
        - [(sql_op, v1), (sql_op, v2), ...] for AND duplicates
        - [v1, v2, ...] (plain values) for OR equality -> IN clause
    """
    if not expr:
        return {}

    op = str(expr.get("op", "")).lower()
    args = expr.get("args", [])

    if op == "and":
        result: dict = {}
        for arg in args:
            for k, v in parse_cql2_json(arg, negate=negate).items():
                if k in result:
                    existing = result[k]
                    result[k] = (existing if isinstance(existing, list) else [existing]) + [v]
                else:
                    result[k] = v
        return result

    if op == "or":
        or_result: dict = {}
        for arg in args:
            for k, v in parse_cql2_json(arg, negate=negate).items():
                # Extract plain value from equality tuple for OR -> IN clause
                plain = v[1] if isinstance(v, tuple) and v[0] == "=" else v
                if k in or_result:
                    existing = or_result[k]
                    or_result[k] = (existing if isinstance(existing, list) else [existing]) + [plain]
                else:
                    or_result[k] = [plain]
        return or_result

    if op == "not" and len(args) == 1:
        return parse_cql2_json(args[0], negate=not negate)

    sql_op = _OP_MAP.get(op)
    if sql_op and len(args) == 2:
        left, right = args
        if negate:
            sql_op = _OP_NEGATE.get(sql_op, sql_op)
        if isinstance(left, dict) and "property" in left:
            return {left["property"]: (sql_op, _unwrap_literal(right))}
        if isinstance(right, dict) and "property" in right:
            inv = _OP_INVERSE.get(sql_op, sql_op)
            return {right["property"]: (inv, _unwrap_literal(left))}

    # Spatial ops or unknown - return empty
    return {}


def parse_cql2_text(expr: str, negate: bool = False) -> dict:
    """Parse a CQL2-text filter expression into a filter_props dict.

    Handles simple equality and comparison expressions from STAC Browser::

        variable = 'ssh'
        experiment = 'basic-001' AND variable = 'sst'
        variable = 'ssh' OR variable = 'sst'
        NOT (variable = 'sst')

    Args:
        expr: CQL2-text expression string
        negate: Whether to negate all operators

    Returns:
        Same structure as parse_cql2_json()
    """
    stripped = expr.strip()

    # Detect top-level NOT (...) wrapper
    not_match = re.match(r'^NOT\s*\((.+)\)\s*$', stripped, re.IGNORECASE | re.DOTALL)
    if not_match:
        return parse_cql2_text(not_match.group(1), negate=not negate)

    # Detect OR (STAC Browser "Match any filters")
    or_parts = re.split(r'\bOR\b', stripped, flags=re.IGNORECASE)
    if len(or_parts) > 1:
        or_result: dict = {}
        for part in or_parts:
            for m in _TEXT_PATTERN.finditer(part):
                prop = m.group(1)
                if prop.upper() in ("AND", "OR", "NOT"):
                    continue
                value = m.group(3) or m.group(4) or (m.group(5) or "").strip()
                key = prop.lower() if prop.lower() in _KNOWN_PROPERTIES else prop
                if key in or_result:
                    existing = or_result[key]
                    or_result[key] = (existing if isinstance(existing, list) else [existing]) + [value]
                else:
                    or_result[key] = [value]
        return or_result

    # AND expression or single condition
    result: dict = {}
    for m in _TEXT_PATTERN.finditer(expr):
        prop = m.group(1)
        if prop.upper() in ("AND", "OR", "NOT"):
            continue
        op = m.group(2)
        value = m.group(3) or m.group(4) or (m.group(5) or "").strip()
        key = prop.lower() if prop.lower() in _KNOWN_PROPERTIES else prop
        sql_op = _OP_MAP.get(op.lower(), op)
        if negate:
            sql_op = _OP_NEGATE.get(sql_op, sql_op)
        cond = (sql_op, value)
        if key in result:
            existing = result[key]
            result[key] = (existing if isinstance(existing, list) else [existing]) + [cond]
        else:
            result[key] = cond
    return result


def parse_filter(raw: str | None, lang: str | None = None) -> dict:
    """Parse a CQL2 filter expression, auto-detecting format.

    Args:
        raw: Raw filter string (JSON or text)
        lang: Filter language hint ("cql2-json" or "cql2-text")

    Returns:
        Normalized filter_props dict
    """
    if not raw:
        return {}
    try:
        if lang and "json" in lang.lower():
            return parse_cql2_json(json.loads(raw))
        # Try JSON first, fall back to text
        if raw.strip().startswith("{"):
            try:
                return parse_cql2_json(json.loads(raw))
            except json.JSONDecodeError:
                pass
        return parse_cql2_text(raw)
    except Exception:
        return {}


def parse_datetime(datetime_str: str | None) -> dict:
    """Parse STAC datetime parameter into filter conditions.

    Handles:
    - Single timestamp: "2000-01-01T00:00:00Z"
    - Open-ended range: "2000-01-01T00:00:00Z/.." or "../2005-12-31"
    - Closed range: "2000-01-01/2005-12-31"

    Returns:
        Dict with datetime/datetime_end keys and (op, value) tuples
    """
    if not datetime_str:
        return {}
    if "/" in datetime_str:
        parts = datetime_str.split("/", 1)
        start, end = parts[0], parts[1]
        filt: dict = {}
        if start and start != "..":
            filt["datetime"] = (">=", start)
        if end and end != "..":
            filt["datetime_end"] = ("<=", end)
        return filt
    return {"datetime": ("=", datetime_str)}
