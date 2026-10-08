"""One generic engine reading declarative filename templates (``templates.yaml``).

Confirmed live: a real on-disk filename convention (ECHAM's
``<expid>_YYYYMM.DD_<stream>``, FESOM's ``<stream>.fesom.<year>.nc`` /
``fesom.<year>.<stream>.restart``) is a property of how each model writes its
own output -- not something specific to one component's Python code. A single
template-compiling engine, driven by a plain per-component data table, covers
every format (GRIB, NetCDF, ...) the same way; there is nothing GRIB- or
NetCDF- or FESOM-specific about *matching a filename shape*. Only the table
itself (``templates.yaml``) is per-component; this module carries no
component-specific logic at all.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from datetime import datetime
from functools import lru_cache
from importlib import resources
from typing import Optional

from upath import UPath

from esm_catalog.scan.path_facets import hookimpl

_PLACEHOLDER_RE = re.compile(r"\{\{\s*(\w+)\s*\}\}")

_STRFTIME_FIELD_RE = {
    "%Y": r"\d{4}",
    "%y": r"\d{2}",
    "%m": r"\d{2}",
    "%d": r"\d{2}",
    "%H": r"\d{2}",
    "%M": r"\d{2}",
    "%S": r"\d{2}",
    "%j": r"\d{1,3}",
}
"""strftime code -> the regex matching exactly what it prints. Extend as a
real template needs a field not listed here -- deliberately not exhaustive."""


@dataclass(frozen=True)
class _Template:
    component: str
    date_format: str
    regex: "re.Pattern"


def _date_field_regex(date_format: str) -> str:
    """*date_format* (a plain strftime string, e.g. ``"%Y%m.%d"``) as a regex
    matching exactly what it would print -- literal characters escaped,
    each recognised ``%X`` code replaced by its fixed-width digit class."""
    pattern = re.escape(date_format)
    for code, field_regex in _STRFTIME_FIELD_RE.items():
        # re.escape leaves '%' and letters alone (not regex-special), so the
        # escaped string still contains the literal "%Y" etc. to replace.
        pattern = pattern.replace(code, field_regex)
    return pattern


def _compile(component: str, pattern: str, date_format: str) -> _Template:
    """Compile one declarative *pattern* into a matchable :class:`_Template`.

    ``{{ expid }}`` -> greedy ``.+`` (disambiguated by backtracking against
    the far more specific date/stream fields around it -- the same
    resolution a hand-written regex anchoring on the filename's tail would
    need). ``{{ stream }}`` -> everything up to the next ``.``/``/`` --
    confirmed live: a real stream name can itself contain an underscore
    (FESOM's ``a_ice``), so only the filename/path structural separators are
    excluded, not ``_``. ``{{ date }}`` -> derived from *date_format*.
    """
    date_regex = _date_field_regex(date_format)
    pieces: list[str] = []
    last = 0
    for match in _PLACEHOLDER_RE.finditer(pattern):
        pieces.append(re.escape(pattern[last : match.start()]))
        name = match.group(1)
        if name == "expid":
            pieces.append(r"(?P<expid>.+)")
        elif name == "stream":
            pieces.append(r"(?P<stream>[^./]+)")
        elif name == "date":
            pieces.append(f"(?P<date>{date_regex})")
        else:
            raise ValueError(f"templates.yaml: unknown placeholder {{{{ {name} }}}}")
        last = match.end()
    pieces.append(re.escape(pattern[last:]))
    regex = re.compile("^" + "".join(pieces) + "$")
    return _Template(component=component, date_format=date_format, regex=regex)


@lru_cache(maxsize=1)
def _load_templates() -> dict[str, list[_Template]]:
    """Parse ``templates.yaml`` once per process; cached, since it never
    changes at runtime."""
    import yaml

    text = (
        resources.files("esm_catalog.scan.path_facets")
        .joinpath("templates.yaml")
        .read_text()
    )
    doc = yaml.safe_load(text) or {}
    return {
        component: [
            _compile(component, entry["pattern"], entry["date_format"])
            for entry in entries
        ]
        for component, entries in doc.items()
    }


@hookimpl
def extract_path_facets(
    path: UPath, component: str, stream: Optional[str]
) -> Optional[tuple[str, datetime]]:
    for template in _load_templates().get(component, []):
        match = template.regex.match(path.name)
        if match is None:
            continue
        groups = match.groupdict()
        resolved_stream = groups.get("stream")
        if resolved_stream is None:
            continue
        if stream is not None and stream != resolved_stream:
            continue
        try:
            start = datetime.strptime(groups["date"], template.date_format)
        except ValueError:  # noqa: BLE001 -- an out-of-range date is not a match
            continue
        return resolved_stream, start
    return None
