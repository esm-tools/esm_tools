"""Scan a GRIB file and return a metadata dict for STAC Item construction."""

import os
import re
import warnings
from collections import defaultdict
from datetime import datetime, timezone
from pathlib import Path

from loguru import logger


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


def _find_codes_file(grib_path: Path) -> Path | None:
    """Find the associated .codes file for a GRIB file.

    Search order:
    1. {grib_filename}.codes in same directory
    2. echam6.codes, echam.codes in same directory
    3. Any *.codes file in same directory
    """
    codes_path = grib_path.parent / (grib_path.name + ".codes")
    if codes_path.exists():
        return codes_path
    for name in ("echam6.codes", "echam.codes"):
        codes_path = grib_path.parent / name
        if codes_path.exists():
            return codes_path
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
                                           gridDimensions}}}
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
    """
    import xarray as xr

    datasets = {}
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

    return datasets


# -----------------------------------------------------------------------------
# Variable metadata extraction
# -----------------------------------------------------------------------------

def _extract_variables(
    datasets: dict,
    codes_table: dict[int, dict] | None,
) -> list[dict]:
    """Collect variable metadata across all hypercube datasets.

    Enriches from the .codes table when available; falls back to GRIB attrs.
    Adds grid_type, level_type, and shape per variable.
    """
    result = []
    for (grid_type, level_type), ds in datasets.items():
        for var_name, da in ds.data_vars.items():
            param_id = da.attrs.get("GRIB_paramId")

            if codes_table and param_id and param_id in codes_table:
                code_info = codes_table[param_id]
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
# Dimension extraction
# -----------------------------------------------------------------------------

def _extract_dimensions_grib(datasets: dict) -> dict:
    """Build a ``cube:dimensions`` dict from all open GRIB hypercube datasets.

    Iterates over every (gridType, levelType) → xr.Dataset pair and collects
    unique dimension names with their type, axis, and extent.  Multiple
    hypercubes may share dimensions (e.g. *latitude*/*longitude* appear in
    both the surface and hybrid-level cubes); duplicates are skipped after the
    first occurrence.
    """
    import numpy as np

    dims: dict = {}

    for ds in datasets.values():
        for name, size in ds.sizes.items():
            if name in dims:
                continue  # Already handled from a previous hypercube

            coord = ds.coords.get(name)
            entry: dict = {}

            if name in ("time", "valid_time") or "time" in name.lower():
                entry["type"] = "temporal"
                times = _extract_datetimes(ds)
                if times:
                    entry["extent"] = [min(times).isoformat(), max(times).isoformat()]
                else:
                    entry["extent"] = [None, None]

            elif name in ("latitude", "lat", "y"):
                entry["type"] = "spatial"
                entry["axis"] = "y"
                if coord is not None:
                    vals = coord.values
                    entry["extent"] = [float(np.nanmin(vals)), float(np.nanmax(vals))]

            elif name in ("longitude", "lon", "x"):
                entry["type"] = "spatial"
                entry["axis"] = "x"
                if coord is not None:
                    vals = coord.values.copy()
                    if np.nanmax(vals) > 180:
                        vals = np.where(vals > 180, vals - 360, vals)
                    entry["extent"] = [float(np.nanmin(vals)), float(np.nanmax(vals))]

            elif name in ("hybrid", "level", "lev", "depth",
                          "heightAboveGround", "isobaricInhPa"):
                entry["type"] = "spatial"
                entry["axis"] = "z"
                if coord is not None:
                    vals = coord.values
                    try:
                        entry["extent"] = [float(np.nanmin(vals)), float(np.nanmax(vals))]
                    except Exception:
                        entry["extent"] = [0, size - 1]
                else:
                    entry["extent"] = [0, size - 1]

            elif name == "values":
                # Reduced Gaussian / spectral grid — spatial but no single axis
                entry["type"] = "spatial"
                entry["extent"] = [0, size - 1]

            else:
                entry["type"] = "ordinal"
                if coord is not None:
                    vals = coord.values
                    try:
                        entry["extent"] = [float(np.nanmin(vals)), float(np.nanmax(vals))]
                    except Exception:
                        entry["extent"] = [0, size - 1]
                else:
                    entry["extent"] = [0, size - 1]

            dims[name] = entry

    return dims


# -----------------------------------------------------------------------------
# Public entry point
# -----------------------------------------------------------------------------

def scan_grib(path: Path) -> dict:
    """Scan *path* with eccodes + cfgrib and return a STAC-ready metadata dict.

    Strategy:
    1. Scan all GRIB messages with eccodes to discover (gridType, levelType)
       hypercubes — avoids cfgrib silently dropping hypercubes.
    2. Open each hypercube as a separate xarray Dataset via filter_by_keys.
    3. Merge variables, bbox, and datetimes across all hypercubes.
    4. Enrich variable names/units from the companion .codes file if present.

    Returns a dict with the same schema as scan_netcdf.
    """
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

    all_variables: list[dict] = _extract_variables(datasets, codes_table)
    bbox = [-180.0, -90.0, 180.0, 90.0]
    all_datetimes: list[datetime] = []
    dimensions = _extract_dimensions_grib(datasets)
    for ds in datasets.values():
        bbox = _update_bbox(ds, bbox)
        all_datetimes.extend(_extract_datetimes(ds))
        ds.close()

    dt_start = min(all_datetimes) if all_datetimes else None
    dt_end   = max(all_datetimes) if all_datetimes else None
    primary_var = all_variables[0]["name"] if all_variables else "unknown"

    return {
        "variable":       primary_var,
        "variables":      all_variables,
        "cf_parameters":  _cf_parameters(all_variables),
        "dimensions":     dimensions,
        "bbox":           bbox,
        "geometry":       _bbox_to_polygon(bbox),
        "datetime_start": dt_start,
        "datetime_end":   dt_end,
        "datetime_str":   dt_start.strftime("%Y%m") if dt_start else "000000",
        "file_size":      os.path.getsize(path),
        "conventions":    "CF-1.6",
        "format":         "grib",
    }
