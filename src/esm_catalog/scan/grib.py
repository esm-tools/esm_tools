"""Scan a GRIB file and return a metadata dict for STAC Item construction.

Note: GRIB scanning currently requires local file access due to eccodes limitations.
Remote files will be cached locally before scanning.
"""

from __future__ import annotations

import re
import warnings
from collections import defaultdict
from datetime import datetime, timezone
from pathlib import Path
from typing import TYPE_CHECKING, Union

from loguru import logger

if TYPE_CHECKING:
    from upath import UPath


# -----------------------------------------------------------------------------
# .codes file support
# -----------------------------------------------------------------------------

def _parse_codes_file(codes_path: Path) -> dict[int, dict]:
    """Parse an ECHAM-style .codes file.

    Format: paramId levels shortName offset scale long_name [units]

    Example line:
        130    1 st      0.      1. surface temperature [K]

    Returns:
        Dictionary mapping paramId -> {shortName, levels, longName, units}
    """
    params = {}
    try:
        with open(codes_path) as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith('#'):
                    continue
                parts = line.split()
                if len(parts) < 6:
                    continue
                try:
                    param_id = int(parts[0])
                    levels = int(parts[1])
                    short_name = parts[2]
                    long_name_with_units = " ".join(parts[5:])
                    match = re.match(r"(.+?)\s*\[(.+?)\]$", long_name_with_units)
                    if match:
                        long_name = match.group(1).strip()
                        units = match.group(2)
                    else:
                        long_name = long_name_with_units
                        units = ""
                    params[param_id] = {
                        "shortName": short_name,
                        "levels": levels,
                        "longName": long_name,
                        "units": units,
                    }
                except (ValueError, IndexError):
                    continue
        logger.debug("Loaded {} parameter definitions from {}", len(params), codes_path.name)
    except Exception as e:
        logger.warning("Failed to parse codes file {}: {}", codes_path, e)
    return params


def _extract_stream_type(path: Path) -> str:
    """Extract ECHAM stream type from filename.

    Examples:
        basic-001_185001.01_echam -> echam
        basic-001_185001.01_co2 -> co2
        basic-001_185001.01_accw -> accw
        basic-001_185002.01_echam_18500201-18500228 -> echam

    Note: ECHAM filenames contain dots that aren't extensions (e.g., .01),
    so we use .name not .stem to avoid truncation.
    """
    name = path.name
    # Strip date range suffix if present
    name = re.sub(r"_\d{6,8}-\d{6,8}$", "", name)
    # Get the last part after underscore (stream type)
    parts = name.rsplit("_", 1)
    if len(parts) == 2:
        return parts[1]
    return name


def _find_codes_file(grib_path: Path) -> Path | None:
    """Find the associated .codes file for a GRIB file.

    Search order:
    1. {grib_filename}.codes in same directory
    2. {grib_filename_without_date_range}.codes (strips _YYYYMMDD-YYYYMMDD suffix)
    3. echam6.codes, echam.codes in same directory
    4. Any *.codes file in same directory
    """
    # Try exact match first
    codes_path = grib_path.parent / (grib_path.name + ".codes")
    if codes_path.exists():
        return codes_path

    # Try stripping date range suffix: _YYYYMMDD-YYYYMMDD or _YYYYMM-YYYYMM
    base_name = re.sub(r"_\d{6,8}-\d{6,8}$", "", grib_path.name)
    if base_name != grib_path.name:
        codes_path = grib_path.parent / (base_name + ".codes")
        if codes_path.exists():
            return codes_path

    # Try generic ECHAM codes files
    for name in ("echam6.codes", "echam.codes"):
        codes_path = grib_path.parent / name
        if codes_path.exists():
            return codes_path

    # Fall back to any .codes file in directory
    codes_files = list(grib_path.parent.glob("*.codes"))
    if codes_files:
        return codes_files[0]
    return None


# -----------------------------------------------------------------------------
# GRIB structure discovery (eccodes)
# -----------------------------------------------------------------------------

def _scan_grib_structure(file_path: Path) -> dict:
    """Scan all GRIB messages to discover distinct (gridType, levelType) hypercubes.

    Returns:
        {(gridType, levelType): {paramId: {shortName, name, dataDate, dataTime,
                                           gridDimensions, indicatorOfParameter}}}
    """
    import eccodes

    structure = defaultdict(dict)
    with open(file_path, "rb") as f:
        while True:
            gid = eccodes.codes_grib_new_from_file(f)
            if gid is None:
                break
            try:
                grid_type  = eccodes.codes_get(gid, "gridType")
                level_type = eccodes.codes_get(gid, "typeOfLevel")
                param_id   = eccodes.codes_get(gid, "paramId")
                short_name = eccodes.codes_get(gid, "shortName")
                name       = eccodes.codes_get(gid, "name")
                # Get indicatorOfParameter as fallback for paramId=0 cases
                try:
                    indicator = eccodes.codes_get(gid, "indicatorOfParameter")
                except Exception:
                    indicator = None
                try:
                    data_date = eccodes.codes_get(gid, "dataDate")
                    data_time = eccodes.codes_get(gid, "dataTime")
                except Exception:
                    data_date = data_time = None
                try:
                    ni = eccodes.codes_get(gid, "Ni")
                    nj = eccodes.codes_get(gid, "Nj")
                except Exception:
                    ni = nj = None
                structure[(grid_type, level_type)][param_id] = {
                    "shortName": short_name,
                    "name": name,
                    "dataDate": data_date,
                    "dataTime": data_time,
                    "gridDimensions": (ni, nj) if ni and nj else None,
                    "indicatorOfParameter": indicator,
                }
            finally:
                eccodes.codes_release(gid)

    logger.debug(
        "{}: {} messages, {} hypercubes",
        file_path.name,
        sum(len(v) for v in structure.values()),
        len(structure),
    )
    return dict(structure)


# -----------------------------------------------------------------------------
# Per-hypercube xarray opening
# -----------------------------------------------------------------------------

def _open_grib_datasets(
    file_path: Path,
    structure: dict,
) -> dict[tuple[str, str], object]:
    """Open each (gridType, levelType) hypercube as a separate xarray Dataset.

    Returns:
        {(gridType, levelType): xr.Dataset}

    Note: Caller is responsible for closing all returned datasets.
    """
    import xarray as xr

    datasets = {}
    try:
        for (grid_type, level_type) in structure:
            filter_keys = {"gridType": grid_type, "typeOfLevel": level_type}
            try:
                with warnings.catch_warnings():
                    warnings.filterwarnings("ignore", message=".*ecCodes provides no.*")
                    warnings.filterwarnings("ignore", category=UserWarning)
                    ds = xr.open_dataset(
                        str(file_path),
                        engine="cfgrib",
                        backend_kwargs={
                            "filter_by_keys": filter_keys,
                            "errors": "ignore",
                            "indexpath": "",
                            "decode_times": False,
                        },
                    )
                datasets[(grid_type, level_type)] = ds
                logger.debug(
                    "  ({}, {}): {} variables",
                    grid_type, level_type, list(ds.data_vars),
                )
            except Exception as e:
                logger.debug("  Skipping ({}, {}): {}", grid_type, level_type, e)
    except BaseException:
        # Clean up any opened datasets on unexpected exception (e.g., KeyboardInterrupt)
        for ds in datasets.values():
            ds.close()
        raise

    return datasets


# -----------------------------------------------------------------------------
# Variable metadata extraction
# -----------------------------------------------------------------------------

def _extract_variables(
    datasets: dict,
    codes_table: dict[int, dict] | None,
    structure: dict | None = None,
) -> list[dict]:
    """Collect variable metadata across all hypercube datasets.

    Enriches from the .codes table when available; falls back to GRIB attrs.
    Uses indicatorOfParameter from structure when paramId is 0.
    Adds grid_type, level_type, and shape per variable.
    """
    result = []
    for (grid_type, level_type), ds in datasets.items():
        for var_name, da in ds.data_vars.items():
            param_id = da.attrs.get("GRIB_paramId")

            # Try to find code_info from codes_table
            code_info = None
            if codes_table:
                if param_id and param_id in codes_table:
                    code_info = codes_table[param_id]
                elif param_id == 0 and structure:
                    # Fallback: use indicatorOfParameter when paramId is 0
                    hypercube = structure.get((grid_type, level_type), {})
                    msg_info = hypercube.get(0, {})
                    indicator = msg_info.get("indicatorOfParameter")
                    if indicator and indicator in codes_table:
                        code_info = codes_table[indicator]

            if code_info:
                entry = {
                    "name":            code_info["shortName"],
                    "long_name":       code_info["longName"],
                    "units":           code_info["units"],
                    "original_name":   var_name,
                    "metadata_source": "codes_table",
                }
            else:
                entry = {
                    "name":            da.attrs.get("GRIB_shortName", var_name),
                    "long_name":       da.attrs.get("long_name", da.attrs.get("GRIB_name", "")),
                    "units":           da.attrs.get("units", da.attrs.get("GRIB_units", "")),
                    "original_name":   var_name,
                    "metadata_source": "grib_attrs",
                }

            # GRIB provenance fields
            for attr in ("GRIB_paramId", "GRIB_typeOfLevel", "GRIB_shortName"):
                if attr in da.attrs:
                    entry[attr] = da.attrs[attr]

            # CF standard_name from GRIB attrs or short-name map
            if "standard_name" in da.attrs:
                entry["standard_name"] = da.attrs["standard_name"]
            elif not entry.get("standard_name"):
                cf = _grib_to_cf(entry["name"])
                if cf:
                    entry["standard_name"] = cf

            entry["grid_type"]   = grid_type
            entry["level_type"]  = level_type
            entry["dimensions"]  = list(da.dims)
            entry["shape"]       = list(da.shape)
            result.append(entry)

    return result


# -----------------------------------------------------------------------------
# Bbox and datetime helpers
# -----------------------------------------------------------------------------

def _update_bbox(ds, current_bbox: list) -> list:
    """Expand current_bbox to include geographic extent of ds.

    Normalises longitudes from 0–360 (ECHAM convention) to −180–180
    (GeoJSON/STAC convention) before computing the extent.
    """
    import numpy as np

    lat = lon = None
    for name in ("latitude", "lat", "y"):
        if name in ds.coords:
            lat = ds.coords[name].values
            break
    for name in ("longitude", "lon", "x"):
        if name in ds.coords:
            lon = ds.coords[name].values
            break

    if lat is None or lon is None:
        return current_bbox

    try:
        lon = lon.copy()
        if np.nanmax(lon) > 180:
            lon = np.where(lon > 180, lon - 360, lon)
        lat_min, lat_max = float(np.nanmin(lat)), float(np.nanmax(lat))
        lon_min, lon_max = float(np.nanmin(lon)), float(np.nanmax(lon))
        return [
            min(current_bbox[0], lon_min),
            min(current_bbox[1], lat_min),
            max(current_bbox[2], lon_max),
            max(current_bbox[3], lat_max),
        ]
    except Exception:
        return current_bbox


def _extract_datetimes(ds) -> list[datetime]:
    """Extract valid times from a cfgrib dataset.

    Handles two representations produced by cfgrib:
    - numpy.datetime64  (decode_times=True, default)
    - float64 / int64   (decode_times=False) — seconds since Unix epoch
    """
    import numpy as np

    for coord_name in ("valid_time", "time", "step"):
        if coord_name not in ds.coords:
            continue
        times = []
        for v in np.atleast_1d(ds.coords[coord_name].values).flat:
            try:
                if np.issubdtype(type(v), np.datetime64):
                    ts = (v - np.datetime64("1970-01-01T00:00:00")) / np.timedelta64(1, "s")
                    times.append(datetime.fromtimestamp(float(ts), tz=timezone.utc))
                else:
                    times.append(datetime.fromtimestamp(float(v), tz=timezone.utc))
            except Exception:
                pass
        if times:
            return times
    return []


def _cf_parameters(variables: list[dict]) -> list[dict]:
    params = []
    for v in variables:
        if "standard_name" in v:
            p = {"name": v["standard_name"], "variable": v["name"]}
            if v.get("units"):
                p["unit"] = v["units"]
            params.append(p)
    return params


def _bbox_to_polygon(bbox: list) -> dict:
    lon_min, lat_min, lon_max, lat_max = bbox
    return {
        "type": "Polygon",
        "coordinates": [[
            [lon_min, lat_min], [lon_max, lat_min],
            [lon_max, lat_max], [lon_min, lat_max],
            [lon_min, lat_min],
        ]],
    }


# Minimal GRIB shortName → CF standard_name mapping
_GRIB_CF_MAP = {
    "t":   "air_temperature",
    "2t":  "air_temperature",
    "sp":  "surface_air_pressure",
    "msl": "air_pressure_at_mean_sea_level",
    "u":   "eastward_wind",
    "v":   "northward_wind",
    "10u": "eastward_wind",
    "10v": "northward_wind",
    "q":   "specific_humidity",
    "tp":  "precipitation_amount",
    "lsp": "large_scale_precipitation_amount",
    "cp":  "convective_precipitation_amount",
    "sst": "sea_surface_temperature",
    "ssr": "surface_net_shortwave_flux_in_air",
    "str": "surface_net_longwave_flux_in_air",
    "ssh": "sea_surface_height_above_geoid",
    "zos": "sea_surface_height_above_geoid",
}


def _grib_to_cf(short_name: str) -> str | None:
    return _GRIB_CF_MAP.get(short_name.lower())


# -----------------------------------------------------------------------------
# Public entry point
# -----------------------------------------------------------------------------

def scan_grib(path: "Union[Path, UPath, str]") -> dict:
    """Scan *path* with eccodes + cfgrib and return a STAC-ready metadata dict.

    Strategy:
    1. Scan all GRIB messages with eccodes to discover (gridType, levelType)
       hypercubes — avoids cfgrib silently dropping hypercubes.
    2. Open each hypercube as a separate xarray Dataset via filter_by_keys.
    3. Merge variables, bbox, and datetimes across all hypercubes.
    4. Enrich variable names/units from the companion .codes file if present.

    Note: eccodes requires local file access. Remote files are not yet supported
    for GRIB scanning and will raise an error.

    Returns a dict with the same schema as scan_netcdf.
    """
    # Handle string paths
    if isinstance(path, str):
        from esm_catalog.scan.upath import parse_uri
        path = parse_uri(path)

    # Check if remote - eccodes doesn't support remote files
    if hasattr(path, "protocol") and path.protocol and path.protocol != "file":
        raise ValueError(
            f"GRIB scanning requires local file access. Remote path not supported: {path}\n"
            "Download the file locally first, or use NetCDF format for remote scanning."
        )

    path = Path(path)
    logger.debug("Scanning GRIB: {}", path)

    codes_file  = _find_codes_file(path)
    codes_table = _parse_codes_file(codes_file) if codes_file else None

    structure = _scan_grib_structure(path)
    if not structure:
        raise ValueError(f"No GRIB messages found in: {path}")

    datasets = _open_grib_datasets(path, structure)
    if not datasets:
        raise ValueError(f"Could not open any hypercube from: {path}")

    all_variables: list[dict] = _extract_variables(datasets, codes_table, structure)
    bbox = [-180.0, -90.0, 180.0, 90.0]
    all_datetimes: list[datetime] = []
    try:
        for ds in datasets.values():
            bbox = _update_bbox(ds, bbox)
            all_datetimes.extend(_extract_datetimes(ds))
    finally:
        # Ensure all datasets are closed even if exception occurs
        for ds in datasets.values():
            ds.close()

    dt_start = min(all_datetimes) if all_datetimes else None
    dt_end   = max(all_datetimes) if all_datetimes else None

    # Extract stream type from filename (echam, accw, co2, etc.)
    # Pattern: expid_YYYYMM.NN_STREAM or expid_YYYYMM.NN_STREAM_DATERANGE
    stream_type = _extract_stream_type(path)

    # Primary variable: first extracted variable name, or stream type as fallback
    primary_var = all_variables[0]["name"] if all_variables else stream_type

    return {
        "variable":       primary_var,
        "stream":         stream_type,  # ECHAM output stream (echam, accw, co2)
        "variables":      all_variables,
        "cf_parameters":  _cf_parameters(all_variables),
        "dimensions":     {},   # GRIB dims are implicit per-hypercube
        "bbox":           bbox,
        "geometry":       _bbox_to_polygon(bbox),
        "datetime_start": dt_start,
        "datetime_end":   dt_end,
        "datetime_str":   dt_start.strftime("%Y%m") if dt_start else "000000",
        "file_size":      path.stat().st_size,
        "conventions":    "CF-1.6",
        "format":         "grib",
    }
