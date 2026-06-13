"""Build a STAC Item dict from scan metadata and collection context."""

from __future__ import annotations

import hashlib
from datetime import timezone
from pathlib import Path, PurePosixPath
from typing import TYPE_CHECKING, Union

from loguru import logger

from esm_catalog.stac.extensions.contacts import add_contacts_extension
from esm_catalog.stac.extensions.datacube import add_datacube_extension
from esm_catalog.stac.extensions.hpc import add_hpc_extension
from esm_catalog.stac.extensions.namelist import add_namelist_item_extension
from esm_catalog.stac.extensions.paleo import add_paleo_extension
from esm_catalog.stac.extensions.registry import EXTENSION_URLS

if TYPE_CHECKING:
    from upath import UPath


def make_item(
    path: "Union[Path, UPath, str]",
    metadata: dict,
    ctx,
    config: dict | None = None,
) -> dict:
    """Construct a STAC Item dict.

    Args:
        path:     Path to the source file (local Path, UPath, or URI string).
        metadata: Dict returned by scan_netcdf() or scan_grib().
        ctx:      CollectionContext with experiment_id, component, collection_id.
        config:   Optional ESM-Tools config dict (for contacts extension).

    Returns:
        A STAC-conformant Item dict (GeoJSON Feature).
    """
    # Handle different path types
    if isinstance(path, str):
        from esm_catalog.uri import parse_uri
        path = parse_uri(path)

    variable = metadata.get("variable", "unknown")
    stream = metadata.get("stream")  # GRIB stream type (echam, accw, co2)
    dt_str = metadata.get("datetime_str", "000000")
    # For GRIB files, use stream type for item ID; otherwise use variable
    id_prefix = stream if stream else variable
    item_id = _make_id(id_prefix, ctx.component, dt_str, path)

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
        "file:size": metadata.get("file_size"),  # STAC File extension
        "format": metadata.get("format", "unknown"),
        "keywords": [ctx.collection_id],  # Shows as badge in STAC Browser
    }
    if stream:
        properties["stream"] = stream
    if start_datetime:
        properties["start_datetime"] = start_datetime
    if end_datetime:
        properties["end_datetime"] = end_datetime
    if metadata.get("conventions"):
        properties["conventions"] = metadata["conventions"]
    if metadata.get("output_frequency"):
        properties["output_frequency"] = metadata["output_frequency"]

    # Include all global attributes from the file (model version, mesh, schemes, etc.)
    # These become searchable via the queryables endpoint
    global_attrs = metadata.get("global_attributes", {})
    for key, value in global_attrs.items():
        # Prefix with "file:" to avoid collisions with STAC standard properties
        properties[f"file:{key}"] = value

    # All variable names for multi-variable files (GRIB _echam/_accw/_co2).
    # Stored as a JSON array so users can filter items by any contained variable,
    # not just the primary one.  Excluded from single-variable NetCDF files where
    # it would duplicate `variable`.
    all_var_names = [
        v["name"] for v in metadata.get("variables", [])
        if v.get("name") and v["name"] != "unknown"
    ]
    if len(all_var_names) > 1:
        properties["variables"] = all_var_names

    # Determine asset media type
    fmt = metadata.get("format", "")
    if fmt == "grib":
        media_type = "application/x-grib2"
    else:
        media_type = "application/x-netcdf"

    item: dict = {
        "type": "Feature",
        "stac_version": "1.0.0",
        "stac_extensions": [EXTENSION_URLS["cf"], EXTENSION_URLS["file"]],
        "id": item_id,
        "geometry": metadata.get("geometry"),
        "bbox": metadata.get("bbox"),
        "properties": properties,
        "assets": {
            "data": {
                "href": _to_href(path),
                "type": media_type,
                "title": _get_filename(path),
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

    # Apply HPC storage extension (facility, storage tier, state)
    item = add_hpc_extension(item, path)

    # Apply paleo extension (geological time for paleoclimate simulations)
    item = add_paleo_extension(item, config)

    # Apply namelist extension (simulation parameters for combined queries)
    item = add_namelist_item_extension(item, ctx)

    # Apply contacts extension (optional, requires config)
    item = add_contacts_extension(item, config)

    # Derive experiment_type from start year and experiment name
    item = _add_experiment_type(item, ctx.experiment_id)

    return item


def _make_id(
    variable: str, component: str, dt_str: str, path: "Union[Path, UPath]"
) -> str:
    """Build a stable, unique item ID.

    Format: {variable}.{component}.{datetime_str}[.{hash}]

    The optional hash suffix deduplicates items that share the same
    variable/component/datetime but differ in path (e.g. ensemble members).
    """
    base = f"{variable}.{component}.{dt_str}"
    # Append short path hash only when needed for uniqueness
    path_hash = hashlib.md5(str(path).encode()).hexdigest()[:6]
    return f"{base}.{path_hash}"


def _to_href(path: "Union[Path, UPath]") -> str:
    """Convert path to a STAC-compatible href string.

    For local files: file:///absolute/path/to/file.nc
    For remote files: ssh://host/path/to/file.nc (preserves protocol)
    """
    if hasattr(path, "protocol") and path.protocol and path.protocol != "file":
        # Remote path - reconstruct full URI with hostname
        from esm_catalog.uri import to_uri
        return to_uri(path)
    else:
        # Local path - resolve and convert to file:// URI
        resolved = Path(path).resolve()
        return f"file://{resolved}"


def _get_filename(path: "Union[Path, UPath]") -> str:
    """Get the filename from a path (works for both Path and UPath)."""
    return PurePosixPath(str(path)).name


def _add_experiment_type(item: dict, experiment_id: str) -> dict:
    """Derive experiment_type and paleo:years_bp and add them to item properties.

    This runs AFTER add_namelist_item_extension so nml:echam:runctl:dt_start
    is available.  Both properties use the same start-year logic:

    experiment_type classification:
    - year < 1800  → "paleo"    (deep-time or pre-industrial paleo)
    - 1800–1950    → "control"  (pre-industrial control / spinup)
    - year > 1950  → "historical"

    paleo:years_bp = 1950 - start_year  (years before present, present = 1950 CE)
    Examples: 1850 → 100 BP, -21000 → 22950 BP, 1960 → -10 BP

    Start year priority:
    1. nml:echam:runctl:dt_start (most authoritative — experiment config)
    2. Item start_datetime / datetime (GRIB/NetCDF time coordinate)
    """
    from esm_catalog.stac.extensions.paleo import _parse_year_from_iso

    props = item["properties"]
    start_year = None

    # 1. From namelist dt_start (already added by add_namelist_item_extension)
    dt_start_nml = props.get("nml:echam:runctl:dt_start")
    if isinstance(dt_start_nml, list) and dt_start_nml:
        try:
            start_year = int(dt_start_nml[0])
        except (TypeError, ValueError):
            pass

    # 2. Fallback: item datetime (set before namelist extension runs)
    if start_year is None:
        dt_str = props.get("start_datetime") or props.get("datetime")
        if dt_str:
            start_year = _parse_year_from_iso(dt_str)

    # Classify experiment type and compute years BP
    if start_year is not None:
        if start_year < 1800:
            exp_type = "paleo"
        elif start_year <= 1950:
            exp_type = "control"
        else:
            exp_type = "historical"
        props["paleo:years_bp"] = 1950 - start_year
    else:
        exp_type = "control"

    props["experiment_type"] = exp_type
    return item
