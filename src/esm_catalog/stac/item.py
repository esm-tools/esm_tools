"""Build a STAC Item dict from scan metadata and collection context."""

import hashlib
from datetime import timezone
from pathlib import Path

from esm_catalog.stac.extensions.contacts import add_contacts_extension
from esm_catalog.stac.extensions.datacube import add_datacube_extension
from esm_catalog.stac.extensions.registry import EXTENSION_URLS


def make_item(path: Path, metadata: dict, ctx, config: dict | None = None) -> dict:
    """Construct a STAC Item dict.

    Args:
        path:     Path to the source file (used for href and file metadata).
        metadata: Dict returned by scan_netcdf() or scan_grib().
        ctx:      CollectionContext with experiment_id, component, collection_id.
        config:   Optional ESM-Tools config dict (for contacts extension).

    Returns:
        A STAC-conformant Item dict (GeoJSON Feature).
    """
    path = Path(path)
    variable = metadata.get("variable", "unknown")
    dt_str = metadata.get("datetime_str", "000000")
    item_id = _make_id(variable, ctx.component, dt_str, path)

    dt_start = metadata.get("datetime_start")
    dt_end = metadata.get("datetime_end")

    if dt_start and dt_start.tzinfo is None:
        dt_start = dt_start.replace(tzinfo=timezone.utc)
    if dt_end and dt_end.tzinfo is None:
        dt_end = dt_end.replace(tzinfo=timezone.utc)

    # Single-time files: datetime = start; multi-time: use interval
    if dt_start == dt_end or dt_end is None:
        item_datetime = dt_start.isoformat() if dt_start else None
        start_datetime = None
        end_datetime = None
    else:
        item_datetime = None
        start_datetime = dt_start.isoformat() if dt_start else None
        end_datetime = dt_end.isoformat() if dt_end else None

    # Build properties
    properties: dict = {
        "datetime": item_datetime,
        "variable": variable,
        "experiment": ctx.experiment_id,
        "component": ctx.component,
        "file_size": metadata.get("file_size"),
        "format": metadata.get("format", "unknown"),
    }
    if start_datetime:
        properties["start_datetime"] = start_datetime
    if end_datetime:
        properties["end_datetime"] = end_datetime
    if metadata.get("conventions"):
        properties["conventions"] = metadata["conventions"]

    # Determine asset media type
    fmt = metadata.get("format", "")
    if fmt == "grib":
        media_type = "application/x-grib2"
    else:
        media_type = "application/x-netcdf"

    item: dict = {
        "type": "Feature",
        "stac_version": "1.0.0",
        "stac_extensions": [EXTENSION_URLS["cf"]],
        "id": item_id,
        "geometry": metadata.get("geometry"),
        "bbox": metadata.get("bbox"),
        "properties": properties,
        "assets": {
            "data": {
                "href": str(path.resolve()),
                "type": media_type,
                "title": path.name,
                "roles": ["data"],
            }
        },
        "links": [
            {
                "rel": "collection",
                "href": f"#{ctx.collection_id}",
                "type": "application/json",
            }
        ],
        "collection": ctx.collection_id,
    }

    # Add CF parameters from scan metadata
    cf_params = metadata.get("cf_parameters", [])
    if cf_params:
        item["properties"]["cf:parameter"] = cf_params

    # Apply datacube extension
    item = add_datacube_extension(item, metadata)

    # Apply contacts extension (optional, requires config)
    item = add_contacts_extension(item, config)

    return item


def _make_id(variable: str, component: str, dt_str: str, path: Path) -> str:
    """Build a stable, unique item ID.

    Format: {variable}.{component}.{datetime_str}[.{hash}]

    The optional hash suffix deduplicates items that share the same
    variable/component/datetime but differ in path (e.g. ensemble members).
    """
    base = f"{variable}.{component}.{dt_str}"
    # Append short path hash only when needed for uniqueness
    path_hash = hashlib.md5(str(path).encode()).hexdigest()[:6]
    return f"{base}.{path_hash}"
