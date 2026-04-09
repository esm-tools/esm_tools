"""OGC API - Features Part 3 Queryables endpoint.

Provides the /queryables endpoint required for STAC Browser's CQL2 filter
builder. Property enums are populated from live catalog data via DISTINCT
queries on the items and collections tables.

This module uses a FULLY DYNAMIC approach:
1. Discover all property keys present in the items table
2. For each property, query distinct values
3. Build JSON Schema with appropriate types and enums

No properties are hardcoded - everything is discovered from the database.
This means any metadata stored in STAC items (NetCDF global attributes,
namelist parameters, HPC info, paleo data, etc.) automatically becomes
queryable without code changes.

Example response::

    {
        "$schema": "https://json-schema.org/draft/2019-09/schema",
        "$id": "http://localhost:8000/queryables",
        "type": "object",
        "title": "Queryable properties for ESM-Tools STAC Catalog",
        "properties": {
            "datetime": {"title": "Datetime", "type": "string", "format": "date-time"},
            "collection": {"title": "Collection", "type": "string", "enum": [...]},
            "file:FESOM_model": {"title": "FESOM_model", "type": "string", "enum": ["FESOM2"]},
            ...
        }
    }
"""

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import TYPE_CHECKING

from loguru import logger

from esm_catalog.api.validation import is_safe_property_name

if TYPE_CHECKING:
    from starlette.requests import Request

    from esm_catalog.api.pool import CatalogPool
    from esm_catalog.storage.duckdb import CatalogDB


class QueryablesError(Exception):
    """Error during queryables computation."""

    pass


# Special properties that need custom schema (not auto-discovered)
_SPECIAL_PROPERTIES: dict[str, dict] = {
    "datetime": {
        "title": "Datetime",
        "type": "string",
        "format": "date-time",
    },
}

# Collection-level properties to expose as queryables (for collection search)
_COLLECTION_PROPERTIES: list[str] = [
    "model",       # Model component (ECHAM, FESOM, etc.)
    "experiment",  # Experiment ID
]

# Properties to exclude from auto-discovery (too complex or not useful for filtering)
_EXCLUDED_PROPERTIES: frozenset[str] = frozenset({
    "cube:dimensions",
    "cube:variables",
    "cf:parameter",
    "start_datetime",
    "end_datetime",
})

# Maximum number of enum values to include (avoid huge dropdowns)
_MAX_ENUM_VALUES = 100

# Maximum number of properties to discover (performance limit)
_MAX_PROPERTIES = 200

# GHG (greenhouse gas) parameters with unit conversion support
# Maps parameter name patterns to their unit information
# canonical_unit: the unit values are stored in (decimal = volume mixing ratio)
# supported_units: units users can search with, with conversion factors to canonical
_GHG_PARAMETERS: dict[str, dict] = {
    "co2vmr": {
        "title": "CO2 Concentration",
        "canonical_unit": "decimal",
        "supported_units": {
            "decimal": 1.0,        # No conversion
            "ppm": 1e-6,           # Parts per million
            "ppb": 1e-9,           # Parts per billion
        },
        "default_display_unit": "ppm",
    },
    "ch4vmr": {
        "title": "CH4 Concentration",
        "canonical_unit": "decimal",
        "supported_units": {
            "decimal": 1.0,
            "ppm": 1e-6,
            "ppb": 1e-9,
        },
        "default_display_unit": "ppb",  # CH4 typically in ppb
    },
    "n2ovmr": {
        "title": "N2O Concentration",
        "canonical_unit": "decimal",
        "supported_units": {
            "decimal": 1.0,
            "ppm": 1e-6,
            "ppb": 1e-9,
        },
        "default_display_unit": "ppb",  # N2O typically in ppb
    },
    # Add more GHG parameters as needed
    "co2": {
        "title": "CO2 Concentration",
        "canonical_unit": "decimal",
        "supported_units": {
            "decimal": 1.0,
            "ppm": 1e-6,
            "ppb": 1e-9,
        },
        "default_display_unit": "ppm",
    },
    "ch4": {
        "title": "CH4 Concentration",
        "canonical_unit": "decimal",
        "supported_units": {
            "decimal": 1.0,
            "ppm": 1e-6,
            "ppb": 1e-9,
        },
        "default_display_unit": "ppb",
    },
    "n2o": {
        "title": "N2O Concentration",
        "canonical_unit": "decimal",
        "supported_units": {
            "decimal": 1.0,
            "ppm": 1e-6,
            "ppb": 1e-9,
        },
        "default_display_unit": "ppb",
    },
}


def get_ghg_info(param_name: str) -> dict | None:
    """Get GHG info for a parameter name.

    Checks if the last part of the property name (after the last colon or dot)
    matches a known GHG parameter.

    Args:
        param_name: Full property name (e.g., "nml:radctl.co2vmr")

    Returns:
        GHG info dict if matched, None otherwise.
    """
    import re as _re
    # Split on both ":" and "." to handle both old and new formats
    parts = _re.split(r"[.:]", param_name)
    key = parts[-1].lower()

    return _GHG_PARAMETERS.get(key)


def convert_ghg_value(value: float, from_unit: str, param_name: str) -> float:
    """Convert a GHG value from user unit to canonical (decimal) unit.

    Args:
        value: The numeric value to convert.
        from_unit: The unit the value is in ("ppm", "ppb", or "decimal").
        param_name: The parameter name (to look up conversion info).

    Returns:
        Value converted to canonical decimal unit.
    """
    ghg_info = get_ghg_info(param_name)
    if ghg_info is None:
        return value  # Not a GHG parameter, no conversion

    factor = ghg_info["supported_units"].get(from_unit.lower(), 1.0)
    return value * factor


def _sort_paleo_display(display: str) -> float:
    """Sort key for paleo display strings (oldest first).

    Converts "66.0 Ma", "21.0 ka", "500 BCE", "1850 CE" to comparable floats.
    """
    display = display.strip()

    ma_match = re.match(r"([\d.]+)\s*Ma", display, re.IGNORECASE)
    if ma_match:
        return -float(ma_match.group(1)) * 1_000_000

    ka_match = re.match(r"([\d.]+)\s*ka", display, re.IGNORECASE)
    if ka_match:
        return -float(ka_match.group(1)) * 1_000

    bce_match = re.match(r"(\d+)\s*BCE", display, re.IGNORECASE)
    if bce_match:
        return -int(bce_match.group(1))

    ce_match = re.match(r"(\d+)\s*CE", display, re.IGNORECASE)
    if ce_match:
        return int(ce_match.group(1))

    return 0


def _discover_property_keys(db: "CatalogDB") -> set[str]:
    """Discover all unique property keys from items table.

    Only returns property names that pass validation (safe for SQL queries).
    Samples a subset of items to avoid expensive full-table scans.
    """
    try:
        # Get all top-level keys from properties JSON
        # DuckDB's json_keys returns a list directly, not a JSON string
        # Use USING SAMPLE to avoid scanning entire table on large catalogs
        rows = db.db.execute("""
            SELECT DISTINCT keys FROM (
                SELECT json_keys(json_extract(data, '$.properties')) AS keys
                FROM items
                USING SAMPLE 500
            ) WHERE keys IS NOT NULL
        """).fetchall()

        all_keys: set[str] = set()
        for row in rows:
            keys = row[0]
            if keys:
                # DuckDB returns this as a Python list already
                if isinstance(keys, list):
                    all_keys.update(keys)
                elif isinstance(keys, str):
                    # Fallback: try parsing as JSON
                    try:
                        parsed = json.loads(keys)
                        if isinstance(parsed, list):
                            all_keys.update(parsed)
                    except (json.JSONDecodeError, TypeError):
                        pass

        # Filter to only safe property names (security: prevents SQL injection)
        safe_keys = {k for k in all_keys if is_safe_property_name(k)}
        if len(safe_keys) < len(all_keys):
            logger.debug(
                "Filtered {} unsafe property names from queryables",
                len(all_keys) - len(safe_keys),
            )
        return safe_keys
    except Exception as e:
        logger.warning("Failed to discover property keys: {}", e)
        return set()


def _get_distinct_values(db: "CatalogDB", prop_name: str, limit: int = 100) -> list:
    """Get distinct values for a property from items table.

    Returns list of values with their native Python types.
    """
    # Validate property name before SQL interpolation (security)
    if not is_safe_property_name(prop_name):
        logger.warning(
            "Skipping property with invalid name for SQL: {!r}", prop_name
        )
        return []

    try:
        # Property name is validated, safe for interpolation
        # Double-quote the property name to handle any special JSON path chars
        # Use subquery with SAMPLE to avoid expensive full-table scans
        rows = db.db.execute(f"""
            SELECT DISTINCT v FROM (
                SELECT json_extract(data, '$.properties."{prop_name}"') AS v
                FROM items
                USING SAMPLE 1000
            )
            WHERE v IS NOT NULL
            ORDER BY v
            LIMIT {limit + 1}
        """).fetchall()

        values = []
        for row in rows:
            if row[0] is not None:
                raw = row[0]
                # json_extract returns JSON-encoded string, parse it
                if isinstance(raw, str):
                    try:
                        val = json.loads(raw)
                        values.append(val)
                    except (json.JSONDecodeError, TypeError):
                        # Not valid JSON, use as-is
                        values.append(raw)
                else:
                    # Already a Python type
                    values.append(raw)
        return values
    except Exception as e:
        logger.debug("Failed to get values for {}: {}", prop_name, e)
        return []


def _get_collection_distinct_values(db: "CatalogDB") -> list[str]:
    """Get distinct collection IDs."""
    try:
        rows = db.db.execute("""
            SELECT DISTINCT json_extract_string(data, '$.collection') AS v
            FROM items
            WHERE v IS NOT NULL
            ORDER BY v
            LIMIT 1000
        """).fetchall()
        return [r[0] for r in rows if r[0]]
    except Exception as e:
        logger.debug("Failed to get collection values: {}", e)
        return []


def _infer_json_schema_type(values: list) -> tuple[str, list | None]:
    """Infer JSON Schema type and optional enum from sample values.

    Returns (type, enum_or_none).
    """
    if not values:
        return "string", None

    # Check types
    types = set(type(v).__name__ for v in values)

    if types == {"bool"}:
        return "boolean", None
    if types <= {"int", "float"}:
        if all(isinstance(v, int) for v in values):
            return "integer", None
        return "number", None
    if types == {"list"}:
        return "array", None
    if types == {"dict"}:
        return "object", None

    # Default to string
    str_values = [str(v) for v in values]

    # Return enum if few enough unique values
    if len(str_values) <= _MAX_ENUM_VALUES:
        return "string", str_values

    return "string", None


def _make_title(prop_name: str) -> str:
    """Generate human-readable title from property name."""
    # Remove common prefixes
    name = prop_name
    for prefix in ("file:", "hpc:", "paleo:", "nml:"):
        if name.startswith(prefix):
            name = name[len(prefix):]
            break

    # Replace dot sub-separator (used in nml: keys) with space for readability
    name = name.replace(".", " ")
    # Convert FESOM_model -> FESOM model, etc.
    name = name.replace("_", " ")

    # Capitalize first letter
    if name:
        name = name[0].upper() + name[1:]

    return name


def _discover_namelist_groups(db: "CatalogDB") -> set[str]:
    """Discover namelist groups from collections."""
    try:
        rows = db.db.execute("""
            SELECT json_extract_string(data, '$."nml:groups"') AS v
            FROM collections WHERE v IS NOT NULL
        """).fetchall()
        groups: set[str] = set()
        for row in rows:
            if row[0]:
                try:
                    arr = json.loads(row[0])
                    if isinstance(arr, list):
                        groups.update(arr)
                except (json.JSONDecodeError, TypeError):
                    pass
        return groups
    except Exception as e:
        logger.debug("Failed to discover namelist groups: {}", e)
        return set()


def _discover_namelist_parameters(db: "CatalogDB") -> dict[str, set]:
    """Discover namelist parameters and their values from collections."""
    try:
        rows = db.db.execute("""
            SELECT json_extract_string(data, '$."nml:parameters"') AS v
            FROM collections WHERE v IS NOT NULL
        """).fetchall()

        params: dict[str, set] = {}
        for row in rows:
            if row[0]:
                try:
                    obj = json.loads(row[0])
                    if isinstance(obj, dict):
                        for key, value in obj.items():
                            if key not in params:
                                params[key] = set()
                            if isinstance(value, (str, int, float, bool)):
                                params[key].add(value)
                except (json.JSONDecodeError, TypeError):
                    pass
        return params
    except Exception as e:
        logger.debug("Failed to discover namelist parameters: {}", e)
        return {}


def _discover_collection_properties(db: "CatalogDB") -> dict[str, set]:
    """Discover collection-level properties (model, experiment, etc.)."""
    props: dict[str, set] = {}
    for prop in _COLLECTION_PROPERTIES:
        try:
            rows = db.db.execute(f"""
                SELECT DISTINCT json_extract_string(data, '$."{prop}"') AS v
                FROM collections WHERE v IS NOT NULL
                ORDER BY v
                LIMIT 100
            """).fetchall()
            values = {r[0] for r in rows if r[0]}
            if values:
                props[prop] = values
        except Exception as e:
            logger.debug("Failed to discover collection property {}: {}", prop, e)
    return props


def get_queryables(
    request: "Request",
    catalog_paths: list[str | Path],
    pool: "CatalogPool",
) -> dict:
    """Return the /queryables JSON Schema response.

    Fully dynamic discovery of queryable properties:
    1. Discovers all property keys from items table
    2. Queries distinct values for each property
    3. Infers JSON Schema types
    4. Builds complete schema with enums where appropriate

    Args:
        request: HTTP request (for base URL extraction).
        catalog_paths: List of catalog paths to query.
        pool: Connection pool for catalog access.

    Returns:
        JSON Schema dict describing queryable properties.
    """
    base_url = str(request.base_url).rstrip("/")

    # Collect all property keys and values across catalogs
    all_keys: set[str] = set()
    property_values: dict[str, list] = {}
    collection_ids: set[str] = set()
    nml_groups: set[str] = set()
    nml_params: dict[str, set] = {}
    collection_props: dict[str, set] = {}  # model, experiment, etc.

    for path in catalog_paths:
        db = pool.get(path)
        if db is None:
            logger.debug("Skipping unavailable catalog: {}", path)
            continue

        # Discover property keys
        keys = _discover_property_keys(db)
        all_keys.update(keys)

        # Get collection IDs
        collection_ids.update(_get_collection_distinct_values(db))

        # Discover namelist data from collections
        nml_groups.update(_discover_namelist_groups(db))
        for key, values in _discover_namelist_parameters(db).items():
            if key not in nml_params:
                nml_params[key] = set()
            nml_params[key].update(values)

        # Discover collection-level properties (model, experiment)
        for key, values in _discover_collection_properties(db).items():
            if key not in collection_props:
                collection_props[key] = set()
            collection_props[key].update(values)

    # Limit number of properties for performance
    all_keys = set(list(all_keys)[:_MAX_PROPERTIES])

    # Remove excluded properties
    all_keys -= _EXCLUDED_PROPERTIES

    # Get distinct values for each property (across all catalogs)
    for prop_name in all_keys:
        values: list = []
        for path in catalog_paths:
            db = pool.get(path)
            if db:
                values.extend(_get_distinct_values(db, prop_name))
        # Deduplicate while preserving order-ish
        seen: set = set()
        unique_values = []
        for v in values:
            key = str(v)
            if key not in seen:
                seen.add(key)
                unique_values.append(v)
        property_values[prop_name] = unique_values[:_MAX_ENUM_VALUES + 1]

    # Build properties dict
    properties: dict = {}

    # Add special properties first
    for name, schema in _SPECIAL_PROPERTIES.items():
        properties[name] = schema.copy()

    # Add collection property
    if collection_ids:
        properties["collection"] = {
            "title": "Collection",
            "type": "string",
            "enum": sorted(collection_ids),
        }

    # Add collection-level properties (model, experiment)
    for prop_name, values in sorted(collection_props.items()):
        title_map = {
            "model": "Model Component",
            "experiment": "Experiment ID",
        }
        properties[prop_name] = {
            "title": title_map.get(prop_name, _make_title(prop_name)),
            "type": "string",
            "enum": sorted(values),
        }

    # Add discovered item properties
    for prop_name in sorted(all_keys):
        values = property_values.get(prop_name, [])
        schema_type, enum_values = _infer_json_schema_type(values)

        prop_schema: dict = {
            "title": _make_title(prop_name),
            "type": schema_type,
        }

        # Add enum if we have values and not too many
        if enum_values and len(enum_values) <= _MAX_ENUM_VALUES:
            # Special sorting for paleo:display
            if prop_name == "paleo:display":
                prop_schema["enum"] = sorted(enum_values, key=_sort_paleo_display)
            else:
                prop_schema["enum"] = sorted(enum_values)

        properties[prop_name] = prop_schema

    # Add namelist groups if present
    if nml_groups:
        properties["nml:group"] = {
            "title": "Namelist Group",
            "type": "string",
            "enum": sorted(nml_groups),
        }

    # Add namelist parameters from collection-level discovery.
    # Use "nml:group.param" (dot sub-separator) for the key so STAC Browser
    # splits on ":" into exactly two segments (namespace + property).
    #
    # Skip a collection-level entry if an item-level key already covers the
    # same parameter. Items store nml params as nml:{comp}:{group}:{key}
    # (e.g. nml:FESOM:forcing_nml:atmbndy). These 4-segment keys produce the
    # organized "FESOM / forcing_nml" accordion display in STAC Browser.
    # Adding the collection-level nml:forcing_nml.atmbndy duplicate on top
    # would create a redundant "General" group alongside the nice accordion.
    for key, values in sorted(nml_params.items()):
        prop_name = f"nml:{key.replace(':', '.')}"
        if prop_name in properties:
            continue  # Already added from items

        # key format from collections: "group:param" (e.g. "forcing_nml:atmbndy")
        # Item-level keys end with ":group:param" (e.g. "nml:FESOM:forcing_nml:atmbndy")
        item_suffix = f":{key}"  # e.g. ":forcing_nml:atmbndy"
        if any(
            k.startswith("nml:") and k.count(":") >= 2 and k.endswith(item_suffix)
            for k in properties
        ):
            continue  # Already covered by item-level key; avoid duplicate group

        sample = next(iter(values), None)

        # Check if this is a GHG parameter
        ghg_info = get_ghg_info(prop_name)

        if isinstance(sample, bool):
            properties[prop_name] = {
                "title": f"Namelist: {key}",
                "type": "boolean",
            }
        elif isinstance(sample, int):
            prop_schema = {
                "title": f"Namelist: {key}",
                "type": "integer",
            }
            # Add GHG unit info if applicable
            if ghg_info:
                prop_schema["title"] = ghg_info["title"]
                prop_schema["x-ghg-units"] = {
                    "canonical": ghg_info["canonical_unit"],
                    "supported": list(ghg_info["supported_units"].keys()),
                    "default": ghg_info["default_display_unit"],
                }
            properties[prop_name] = prop_schema
        elif isinstance(sample, float):
            prop_schema = {
                "title": f"Namelist: {key}",
                "type": "number",
            }
            # Add GHG unit info if applicable
            if ghg_info:
                prop_schema["title"] = ghg_info["title"]
                prop_schema["x-ghg-units"] = {
                    "canonical": ghg_info["canonical_unit"],
                    "supported": list(ghg_info["supported_units"].keys()),
                    "default": ghg_info["default_display_unit"],
                }
            properties[prop_name] = prop_schema
        else:
            prop_schema = {
                "title": f"Namelist: {key}",
                "type": "string",
            }
            if len(values) <= _MAX_ENUM_VALUES:
                prop_schema["enum"] = sorted(str(v) for v in values)
            properties[prop_name] = prop_schema

    return {
        "$schema": "https://json-schema.org/draft/2019-09/schema",
        "$id": f"{base_url}/queryables",
        "type": "object",
        "title": "Queryable properties for ESM-Tools STAC Catalog",
        "properties": properties,
        "additionalProperties": True,
    }


def _get_collection_distinct_values_filtered(
    db: "CatalogDB", prop_name: str, collection_id: str, limit: int = 100
) -> list:
    """Get distinct values for a property, filtered by collection."""
    if not is_safe_property_name(prop_name):
        return []

    try:
        rows = db.db.execute(f"""
            SELECT DISTINCT v FROM (
                SELECT json_extract(data, '$.properties."{prop_name}"') AS v
                FROM items
                WHERE json_extract_string(data, '$.collection') = ?
            )
            WHERE v IS NOT NULL
            ORDER BY v
            LIMIT {limit + 1}
        """, [collection_id]).fetchall()

        values = []
        for row in rows:
            if row[0] is not None:
                raw = row[0]
                if isinstance(raw, str):
                    try:
                        val = json.loads(raw)
                        values.append(val)
                    except (json.JSONDecodeError, TypeError):
                        values.append(raw)
                else:
                    values.append(raw)
        return values
    except Exception as e:
        logger.debug("Failed to get values for {} in {}: {}", prop_name, collection_id, e)
        return []


def get_collection_queryables(
    request: "Request",
    collection_id: str,
    catalog_paths: list[str | Path],
    pool: "CatalogPool",
) -> dict | None:
    """Return queryables scoped to a specific collection.

    Similar to get_queryables() but enum values are filtered to only
    include values present in items belonging to the specified collection.

    Args:
        request: HTTP request (for base URL extraction).
        collection_id: The collection to scope queryables to.
        catalog_paths: List of catalog paths to query.
        pool: Connection pool for catalog access.

    Returns:
        JSON Schema dict, or None if collection not found.
    """
    base_url = str(request.base_url).rstrip("/")

    # Check if collection exists and discover properties
    collection_found = False
    all_keys: set[str] = set()
    property_values: dict[str, list] = {}

    for path in catalog_paths:
        db = pool.get(path)
        if db is None:
            continue

        # Check if collection exists in this catalog (check both collections table and items)
        try:
            # First check collections table directly
            row = db.db.execute(
                "SELECT 1 FROM collections WHERE id = ?",
                [collection_id],
            ).fetchone()
            if row:
                collection_found = True
            else:
                # Fall back to checking if any items reference this collection
                row = db.db.execute(
                    "SELECT COUNT(*) FROM items WHERE json_extract_string(data, '$.collection') = ?",
                    [collection_id],
                ).fetchone()
                if row and row[0] > 0:
                    collection_found = True
        except Exception:
            pass

        # Discover property keys from this collection's items
        try:
            rows = db.db.execute("""
                SELECT DISTINCT json_keys(json_extract(data, '$.properties')) AS keys
                FROM items
                WHERE json_extract_string(data, '$.collection') = ?
                LIMIT 500
            """, [collection_id]).fetchall()

            for row in rows:
                keys = row[0]
                if keys:
                    if isinstance(keys, list):
                        all_keys.update(keys)
                    elif isinstance(keys, str):
                        try:
                            parsed = json.loads(keys)
                            if isinstance(parsed, list):
                                all_keys.update(parsed)
                        except (json.JSONDecodeError, TypeError):
                            pass
        except Exception as e:
            logger.debug("Failed to discover keys for {}: {}", collection_id, e)

    if not collection_found:
        return None

    # Filter to safe property names and limit count
    all_keys = {k for k in all_keys if is_safe_property_name(k)}
    all_keys = set(list(all_keys)[:_MAX_PROPERTIES])
    all_keys -= _EXCLUDED_PROPERTIES

    # Get distinct values for each property within this collection
    for prop_name in all_keys:
        values: list = []
        for path in catalog_paths:
            db = pool.get(path)
            if db:
                values.extend(_get_collection_distinct_values_filtered(
                    db, prop_name, collection_id
                ))
        # Deduplicate
        seen: set = set()
        unique_values = []
        for v in values:
            key = str(v)
            if key not in seen:
                seen.add(key)
                unique_values.append(v)
        property_values[prop_name] = unique_values[:_MAX_ENUM_VALUES + 1]

    # Build properties dict
    properties: dict = {}

    # Add special properties
    for name, schema in _SPECIAL_PROPERTIES.items():
        properties[name] = schema.copy()

    # Add discovered properties
    for prop_name in sorted(all_keys):
        values = property_values.get(prop_name, [])
        schema_type, enum_values = _infer_json_schema_type(values)

        prop_schema: dict = {
            "title": _make_title(prop_name),
            "type": schema_type,
        }

        if enum_values and len(enum_values) <= _MAX_ENUM_VALUES:
            if prop_name == "paleo:display":
                prop_schema["enum"] = sorted(enum_values, key=_sort_paleo_display)
            else:
                prop_schema["enum"] = sorted(enum_values)

        properties[prop_name] = prop_schema

    return {
        "$schema": "https://json-schema.org/draft/2019-09/schema",
        "$id": f"{base_url}/collections/{collection_id}/queryables",
        "type": "object",
        "title": f"Queryable properties for collection {collection_id!r}",
        "properties": properties,
        "additionalProperties": True,
    }
