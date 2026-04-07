"""Scan a NetCDF file and return a metadata dict for STAC Item construction.

Supports both local paths and remote URIs via fsspec/UPath.

Fast path (local files): uses netCDF4.Dataset directly for header-only reads.
Fallback (remote or on error): uses xr.open_dataset.
"""

from __future__ import annotations

import os
from datetime import datetime, timezone
from pathlib import Path, PurePosixPath
from typing import TYPE_CHECKING, Union

import numpy as np
from loguru import logger

if TYPE_CHECKING:
    import xarray as xr
    from upath import UPath

# Default bbox -- almost all ESM output is global.
# Override via ESM_CATALOG_SCAN_EXTENT="-180,-90,180,90"
_DEFAULT_EXTENT = os.environ.get("ESM_CATALOG_SCAN_EXTENT", "-180,-90,180,90")


def _parse_default_extent() -> tuple[list, dict]:
    parts = [float(x) for x in _DEFAULT_EXTENT.split(",")]
    bbox = parts[:4]
    return bbox, _bbox_to_polygon(bbox)


def _is_remote(path) -> bool:
    return hasattr(path, "protocol") and path.protocol and path.protocol != "file"


# ------------------------------------------------------------------
# Fast path: netCDF4.Dataset (header only, no xarray)
# ------------------------------------------------------------------

def _scan_netcdf_fast(path: Path) -> dict | None:
    """Extract metadata using netCDF4 directly. No xarray, no coord data reads.

    Returns None on any failure (caller falls back to xarray).
    """
    try:
        import netCDF4
    except ImportError:
        return None

    try:
        ds = netCDF4.Dataset(str(path), "r")
    except Exception:
        return None

    try:
        # Variables
        variables = []
        coord_names = set()
        for dim_name in ds.dimensions:
            if dim_name in ds.variables:
                coord_names.add(dim_name)

        for name, var in ds.variables.items():
            if name in coord_names:
                continue
            entry = {"name": name}
            for attr in ("standard_name", "long_name", "units", "description"):
                if hasattr(var, attr):
                    entry[attr] = getattr(var, attr)
            entry["dimensions"] = list(var.dimensions)
            variables.append(entry)

        # Dimensions
        dimensions = {}
        for name, dim in ds.dimensions.items():
            entry = {"type": "spatial", "extent": [None, None]}
            if name in ("time",) or "time" in name.lower():
                entry["type"] = "temporal"
            elif name in ("lat", "latitude", "nav_lat", "y"):
                entry["type"] = "spatial"
                entry["axis"] = "y"
            elif name in ("lon", "longitude", "nav_lon", "x"):
                entry["type"] = "spatial"
                entry["axis"] = "x"
            elif name in ("lev", "level", "depth", "z"):
                entry["type"] = "spatial"
                entry["axis"] = "z"
            dimensions[name] = entry

        # Time range: read only first and last value
        dt_start, dt_end = _extract_time_range_nc4(ds)

        # Bbox: use default (global) extent -- skip coordinate reads
        bbox, geometry = _parse_default_extent()

        # Global attributes
        global_attrs = {}
        for attr_name in ds.ncattrs():
            try:
                val = ds.getncattr(attr_name)
                if isinstance(val, str) and len(val) > 1000:
                    continue
                if isinstance(val, np.ndarray):
                    if val.size > 10:
                        continue
                    val = val.tolist()
                if hasattr(val, "item"):
                    val = val.item()
                global_attrs[attr_name] = val
            except Exception:
                continue

        conventions = global_attrs.pop("Conventions", "")

        return {
            "variables": variables,
            "cf_parameters": _cf_parameters(variables),
            "dimensions": dimensions,
            "bbox": bbox,
            "geometry": geometry,
            "datetime_start": dt_start,
            "datetime_end": dt_end,
            "datetime_str": _datetime_str(path, dt_start),
            "file_size": 0,  # captured by worker via os.stat
            "conventions": conventions,
            "format": "netcdf",
            "global_attributes": global_attrs,
        }
    except Exception as e:
        logger.debug("Fast NetCDF scan failed for {}: {}, falling back to xarray", path, e)
        return None
    finally:
        ds.close()


def _extract_time_range_nc4(ds) -> tuple[datetime | None, datetime | None]:
    """Read time[0] and time[-1] from a netCDF4.Dataset. Two values only."""
    try:
        if "time" not in ds.variables:
            return None, None

        time_var = ds.variables["time"]
        n = len(time_var)
        if n == 0:
            return None, None

        import cftime

        units = getattr(time_var, "units", "")
        calendar = getattr(time_var, "calendar", "standard")
        if not units:
            return None, None

        t0_raw = float(time_var[0])
        t1_raw = float(time_var[-1])

        decoded = cftime.num2date([t0_raw, t1_raw], units=units, calendar=calendar)
        t0, t1 = decoded[0], decoded[1]

        if hasattr(t0, "year"):
            t0 = datetime(t0.year, t0.month, t0.day, t0.hour, t0.minute, t0.second,
                          tzinfo=timezone.utc)
            t1 = datetime(t1.year, t1.month, t1.day, t1.hour, t1.minute, t1.second,
                          tzinfo=timezone.utc)

        return t0, t1
    except Exception:
        return None, None


# ------------------------------------------------------------------
# Full path: xr.open_dataset (remote files, fallback)
# ------------------------------------------------------------------

def scan_netcdf(path: "Union[Path, UPath, str]", timeout: int = 120) -> dict:
    """Open *path* and extract STAC-relevant metadata.

    For local files, tries the fast netCDF4.Dataset path first (header-only,
    no xarray overhead). Falls back to xr.open_dataset for remote files or
    on any error.
    """
    if isinstance(path, str):
        from esm_catalog.scan.upath import parse_uri
        path = parse_uri(path)

    logger.debug("Scanning NetCDF: {}", path)

    # Fast path for local files
    if not _is_remote(path):
        result = _scan_netcdf_fast(Path(path))
        if result is not None:
            logger.debug("Fast path OK: {}", path)
            return result
        logger.debug("Fast path failed, falling back to xarray: {}", path)

    # Full xarray path (remote or fast-path failure)
    import xarray as xr

    if _is_remote(path):
        from esm_catalog.scan.upath import to_uri
        uri = to_uri(path)
        logger.debug("Opening remote file: {}", uri)
        storage_options = {"timeout": timeout}
        open_kwargs = dict(
            decode_times=True, engine="h5netcdf",
            backend_kwargs={"storage_options": storage_options}
        )
        open_path = uri
    else:
        open_kwargs = dict(decode_times=True)
        open_path = str(path)

    try:
        ds = xr.open_dataset(open_path, **open_kwargs)
    except ValueError as e:
        if "unable to decode time" in str(e) or "Failed to decode" in str(e):
            logger.debug("Time decode failed for {}, retrying without decode_times", path)
            open_kwargs["decode_times"] = False
            ds = xr.open_dataset(open_path, **open_kwargs)
        else:
            raise

    with ds:
        variables = _extract_variables(ds)
        dimensions = _extract_dimensions(ds)
        bbox, geometry = _parse_default_extent()
        dt_start, dt_end = _extract_time_range(ds)
        global_attrs = _extract_global_attributes(ds)

    return {
        "variables": variables,
        "cf_parameters": _cf_parameters(variables),
        "dimensions": dimensions,
        "bbox": bbox,
        "geometry": geometry,
        "datetime_start": dt_start,
        "datetime_end": dt_end,
        "datetime_str": _datetime_str(path, dt_start),
        "file_size": _get_file_size(path),
        "conventions": global_attrs.pop("Conventions", ""),
        "format": "netcdf",
        "global_attributes": global_attrs,
    }


def _get_file_size(path: "Union[Path, UPath]") -> int:
    """Get file size, works for both local Path and remote UPath."""
    try:
        return path.stat().st_size
    except Exception:
        return 0


def _extract_global_attributes(ds: xr.Dataset) -> dict:
    """Extract all global attributes from the dataset.

    Filters out attributes that are too large (> 1000 chars) or non-scalar.
    Converts numpy types to Python native types.
    """
    attrs = {}
    for key, value in ds.attrs.items():
        # Skip very long values (e.g., large arrays or binary data)
        if isinstance(value, str) and len(value) > 1000:
            continue
        # Skip non-scalar numpy arrays
        if isinstance(value, np.ndarray):
            if value.size > 10:
                continue
            value = value.tolist()
        # Convert numpy scalars to Python types
        if hasattr(value, "item"):
            value = value.item()
        attrs[key] = value
    return attrs


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
        t0, t1 = times[0], times[-1]

        # Integer-encoded time: xarray could not decode (non-standard calendar or
        # missing units).  Decode manually via cftime using the coordinate attributes.
        if np.issubdtype(times.dtype, np.integer):
            units = time_coord.attrs.get("units", "")
            calendar = time_coord.attrs.get("calendar", "standard")
            if not units:
                return None, None
            decoded = cftime.num2date([t0, t1], units=units, calendar=calendar)
            t0, t1 = decoded[0], decoded[1]

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
        import cftime
        vals = coord.values
        t0, t1 = vals[0], vals[-1]

        # Integer-encoded time: decode via CF units/calendar attributes
        if np.issubdtype(vals.dtype, np.integer):
            units = coord.attrs.get("units", "")
            calendar = coord.attrs.get("calendar", "standard")
            if not units:
                return [None, None]
            decoded = cftime.num2date([t0, t1], units=units, calendar=calendar)
            t0, t1 = decoded[0], decoded[1]

        if hasattr(t0, "year"):
            # cftime object — convert to ISO string via datetime
            t0_dt = datetime(t0.year, t0.month, t0.day, t0.hour, t0.minute, t0.second,
                             tzinfo=timezone.utc)
            t1_dt = datetime(t1.year, t1.month, t1.day, t1.hour, t1.minute, t1.second,
                             tzinfo=timezone.utc)
            return [t0_dt.isoformat(), t1_dt.isoformat()]
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


def _datetime_str(path: "Union[Path, UPath]", dt: datetime | None) -> str:
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
