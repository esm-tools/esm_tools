"""Build a STAC Item dict from scan metadata and collection context."""

from __future__ import annotations

import hashlib
from datetime import timezone
from pathlib import Path, PurePosixPath
from typing import TYPE_CHECKING, Union

if TYPE_CHECKING:
    from upath import UPath


def make_item(
    path: "Union[Path, UPath, str]",
    metadata: dict,
    ctx,
) -> dict:
    """Construct a STAC Item dict.

    Args:
        path:     Path to the source file (local Path, UPath, or URI string).
        metadata: Dict returned by scan_netcdf() or scan_grib().
        ctx:      CollectionContext with experiment_id, component, collection_id.

    Returns:
        A STAC-conformant Item dict (GeoJSON Feature).
    """
    if isinstance(path, str):
        from esm_catalog.uri import parse_uri
        path = parse_uri(path)

    return {
        "type": "Feature",
        "stac_version": "1.0.0",
        "stac_extensions": [],
        "id": _make_id(metadata.get("variable", "unknown"), ctx.component, metadata.get("datetime_str", "000000"), path),
        "geometry": metadata.get("geometry"),
        "bbox": metadata.get("bbox"),
        "properties": _build_properties(metadata, ctx),
        "assets": _build_assets(path, metadata),
        "links": [
            {"rel": "collection", "href": f"#{ctx.collection_id}", "type": "application/json"}
        ],
        "collection": ctx.collection_id,
    }


def _make_id(
    variable: str, component: str, dt_str: str, path: "Union[Path, UPath]"
) -> str:
    """Return a stable unique item ID: {variable}.{component}.{datetime}.{hash}."""
    path_hash = hashlib.md5(str(path).encode()).hexdigest()[:6]
    return f"{variable}.{component}.{dt_str}.{path_hash}"


def _build_properties(metadata: dict, ctx) -> dict:
    """Assemble the STAC item properties dict from metadata and context."""
    _, _, item_datetime, start_datetime, end_datetime = _build_datetime(metadata)

    properties: dict = {
        "datetime": item_datetime,
        "variable": metadata.get("variable", "unknown"),
        "experiment": ctx.experiment_id,
        "component": ctx.component,
        "format": metadata.get("format", "unknown"),
    }
    if start_datetime:
        properties["start_datetime"] = start_datetime
    if end_datetime:
        properties["end_datetime"] = end_datetime
    if metadata.get("output_frequency"):
        properties["output_frequency"] = metadata["output_frequency"]

    all_var_names = [
        v["name"] for v in metadata.get("variables", [])
        if v.get("name") and v["name"] != "unknown"
    ]
    if len(all_var_names) > 1:
        properties["variables"] = all_var_names

    return properties


def _build_datetime(metadata: dict) -> tuple:
    """Parse and normalise datetime fields from metadata.

    Returns:
        (dt_start, dt_end, item_datetime, start_datetime, end_datetime)
        where item_datetime is set for single-time files and
        start_datetime/end_datetime for multi-time files.
    """
    dt_start = metadata.get("datetime_start")
    dt_end = metadata.get("datetime_end")

    if dt_start and dt_start.tzinfo is None:
        dt_start = dt_start.replace(tzinfo=timezone.utc)
    if dt_end and dt_end.tzinfo is None:
        dt_end = dt_end.replace(tzinfo=timezone.utc)

    if dt_start == dt_end or dt_end is None:
        return dt_start, dt_end, dt_start.isoformat() if dt_start else None, None, None
    return (
        dt_start, dt_end, None,
        dt_start.isoformat() if dt_start else None,
        dt_end.isoformat() if dt_end else None,
    )


def _build_assets(path: "Union[Path, UPath]", metadata: dict) -> dict:
    """Build the STAC assets dict with a single 'data' asset for the source file."""
    fmt = metadata.get("format", "")
    media_type = "application/x-grib2" if fmt == "grib" else "application/x-netcdf"
    return {
        "data": {
            "href": _to_href(path),
            "type": media_type,
            "title": PurePosixPath(str(path)).name,
            "roles": ["data"],
        }
    }


def _to_href(path: "Union[Path, UPath]") -> str:
    """Convert path to a STAC-compatible href (file:// or protocol URI)."""
    if hasattr(path, "protocol") and path.protocol and path.protocol != "file":
        from esm_catalog.uri import to_uri
        return to_uri(path)
    return f"file://{Path(path).resolve()}"
