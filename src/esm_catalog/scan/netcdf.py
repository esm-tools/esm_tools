"""Scan a NetCDF file and return a metadata dict for STAC Item construction."""

import os
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import xarray as xr
from loguru import logger


def scan_netcdf(path: Path) -> dict:
    """Open *path* with xarray and extract STAC-relevant metadata.

    Returns a dict with keys:
        variable, variables, cf_parameters, dimensions, bbox, geometry,
        datetime_start, datetime_end, datetime_str, file_size, conventions
    """
    path = Path(path)
    logger.debug("Scanning NetCDF: {}", path)

    ds = xr.open_dataset(str(path), decode_times=True)

    variables = _extract_variables(ds)
    dimensions = _extract_dimensions(ds)
    bbox, geometry = _extract_bbox(ds)
    dt_start, dt_end = _extract_time_range(ds)

    # Primary variable: first data variable (non-coordinate)
    primary_var = next(iter(ds.data_vars), "unknown")

    ds.close()

    return {
        "variable": primary_var,
        "variables": variables,
        "cf_parameters": _cf_parameters(variables),
        "dimensions": dimensions,
        "bbox": bbox,
        "geometry": geometry,
        "datetime_start": dt_start,
        "datetime_end": dt_end,
        "datetime_str": _datetime_str(path, dt_start),
        "file_size": os.path.getsize(path),
        "conventions": ds.attrs.get("Conventions", ""),
        "format": "netcdf",
    }


def _extract_variables(ds: xr.Dataset) -> list[dict]:
    """Return list of variable metadata dicts for all data variables."""
    result = []
    for name, da in ds.data_vars.items():
        entry = {"name": name}
        for attr in ("standard_name", "long_name", "units", "description"):
            if attr in da.attrs:
                entry[attr] = da.attrs[attr]
        entry["dimensions"] = list(da.dims)
        result.append(entry)
    return result


def _extract_dimensions(ds: xr.Dataset) -> dict:
    """Build cube:dimensions dict for the datacube extension."""
    dims = {}
    for name, size in ds.sizes.items():
        entry: dict = {"type": "spatial", "extent": [None, None]}
        coord = ds.coords.get(name)
        if coord is not None:
            vals = coord.values
            entry["extent"] = [_to_python(vals.min()), _to_python(vals.max())]
            units = coord.attrs.get("units", "")
            if units:
                entry["unit"] = units

        # Classify dimension type
        if name in ("time",) or "time" in name.lower():
            entry["type"] = "temporal"
            entry["extent"] = _time_extent_iso(coord)
        elif name in ("lat", "latitude", "nav_lat", "y"):
            entry["type"] = "spatial"
            entry["axis"] = "y"
        elif name in ("lon", "longitude", "nav_lon", "x"):
            entry["type"] = "spatial"
            entry["axis"] = "x"
        elif name in ("lev", "level", "depth", "z"):
            entry["type"] = "spatial"
            entry["axis"] = "z"

        dims[name] = entry
    return dims


def _extract_bbox(ds: xr.Dataset) -> tuple[list, dict]:
    """Return (bbox, GeoJSON geometry) from coordinate variables.

    Falls back to global extent if no geographic coordinates found.
    """
    # Try common latitude/longitude coordinate names
    lat_names = ("lat", "latitude", "nav_lat", "y", "XLAT")
    lon_names = ("lon", "longitude", "nav_lon", "x", "XLONG")

    lat = None
    lon = None
    for name in lat_names:
        if name in ds.coords:
            lat = ds.coords[name].values
            break
    for name in lon_names:
        if name in ds.coords:
            lon = ds.coords[name].values
            break

    if lat is None or lon is None:
        return _global_bbox()

    try:
        lat_min, lat_max = float(np.nanmin(lat)), float(np.nanmax(lat))
        lon_min, lon_max = float(np.nanmin(lon)), float(np.nanmax(lon))
        if not (-90 <= lat_min <= lat_max <= 90 and -180 <= lon_min <= lon_max <= 180):
            return _global_bbox()
        bbox = [lon_min, lat_min, lon_max, lat_max]
        geometry = _bbox_to_polygon(bbox)
        return bbox, geometry
    except Exception:
        return _global_bbox()


def _global_bbox():
    bbox = [-180.0, -90.0, 180.0, 90.0]
    return bbox, _bbox_to_polygon(bbox)


def _bbox_to_polygon(bbox: list) -> dict:
    lon_min, lat_min, lon_max, lat_max = bbox
    return {
        "type": "Polygon",
        "coordinates": [[
            [lon_min, lat_min],
            [lon_max, lat_min],
            [lon_max, lat_max],
            [lon_min, lat_max],
            [lon_min, lat_min],
        ]],
    }


def _extract_time_range(ds: xr.Dataset) -> tuple[datetime | None, datetime | None]:
    """Return (start, end) datetimes from the time coordinate."""
    time_coord = ds.coords.get("time")
    if time_coord is None:
        return None, None
    try:
        import cftime
        times = time_coord.values
        if len(times) == 0:
            return None, None
        t0 = times[0]
        t1 = times[-1]
        if hasattr(t0, "year"):
            # cftime object
            t0 = datetime(t0.year, t0.month, t0.day, t0.hour, t0.minute, t0.second,
                           tzinfo=timezone.utc)
            t1 = datetime(t1.year, t1.month, t1.day, t1.hour, t1.minute, t1.second,
                           tzinfo=timezone.utc)
        else:
            # numpy datetime64
            t0 = _np64_to_datetime(t0)
            t1 = _np64_to_datetime(t1)
        return t0, t1
    except Exception as e:
        logger.warning("Could not extract time range: {}", e)
        return None, None


def _np64_to_datetime(np_dt) -> datetime:
    ts = (np_dt - np.datetime64("1970-01-01T00:00:00")) / np.timedelta64(1, "s")
    return datetime.fromtimestamp(float(ts), tz=timezone.utc)


def _time_extent_iso(coord) -> list:
    if coord is None:
        return [None, None]
    try:
        vals = coord.values
        t0 = vals[0]
        t1 = vals[-1]
        if hasattr(t0, "isoformat"):
            return [t0.isoformat(), t1.isoformat()]
        t0_dt = _np64_to_datetime(t0)
        t1_dt = _np64_to_datetime(t1)
        return [t0_dt.isoformat(), t1_dt.isoformat()]
    except Exception:
        return [None, None]


def _cf_parameters(variables: list[dict]) -> list[dict]:
    params = []
    for v in variables:
        if "standard_name" in v:
            p = {"name": v["standard_name"], "variable": v["name"]}
            if "units" in v:
                p["unit"] = v["units"]
            params.append(p)
    return params


def _datetime_str(path: Path, dt: datetime | None) -> str:
    """Return a compact datetime string for use in item IDs."""
    if dt is not None:
        return dt.strftime("%Y%m")
    # Fallback: extract YYYYMM from filename
    import re
    m = re.search(r"\b(\d{6})\b", path.stem)
    if m:
        return m.group(1)
    m = re.search(r"\b(\d{4})\b", path.stem)
    if m:
        return m.group(1)
    return "000000"


def _to_python(val):
    """Convert numpy scalar to Python native type."""
    if hasattr(val, "item"):
        return val.item()
    return val
