"""
Data loading module for climate datasets.

Handles opening NetCDF and GRIB files with xarray, supporting lazy loading
via dask for efficient handling of large climate datasets.
"""

from pathlib import Path
from typing import Any
from urllib.parse import urlparse

import numpy as np
import xarray as xr
from loguru import logger


def _to_native(obj: Any) -> Any:
    """Recursively convert numpy types to Python native for JSON serialization."""
    if isinstance(obj, (np.integer,)):
        return int(obj)
    if isinstance(obj, (np.floating,)):
        return float(obj)
    if isinstance(obj, np.ndarray):
        return obj.tolist()
    if isinstance(obj, dict):
        return {k: _to_native(v) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [_to_native(v) for v in obj]
    return obj


def _strip_file_uri(href: str) -> str:
    """
    Convert file:// URIs to plain filesystem paths.

    Parameters
    ----------
    href : str
        The href which may be a file:// URI or plain path.

    Returns
    -------
    str
        Plain filesystem path.
    """
    parsed = urlparse(href)
    if parsed.scheme == "file":
        return parsed.path
    return href


def _detect_format(path: str) -> str:
    """
    Auto-detect file format by extension and magic bytes.

    Parameters
    ----------
    path : str
        Path to the data file.

    Returns
    -------
    str
        Format identifier: 'netcdf' or 'grib'.

    Raises
    ------
    ValueError
        If the format cannot be determined.
    """
    path_obj = Path(path)
    suffix = path_obj.suffix.lower()

    # Check by extension first
    if suffix in (".nc", ".nc4", ".netcdf", ".ncdf"):
        return "netcdf"
    if suffix in (".grib", ".grib2", ".grb", ".grb2"):
        return "grib"

    # Fall back to magic bytes detection
    try:
        with open(path, "rb") as f:
            header = f.read(8)

        # NetCDF magic bytes: 'CDF\x01' or 'CDF\x02' (classic) or '\x89HDF' (HDF5/NetCDF4)
        if header[:3] == b"CDF" or header[:4] == b"\x89HDF":
            return "netcdf"

        # GRIB magic bytes: 'GRIB'
        if header[:4] == b"GRIB":
            return "grib"

    except (OSError, IOError) as e:
        logger.warning(f"Could not read file header for format detection: {e}")

    raise ValueError(
        f"Cannot determine format for {path}. "
        "Use explicit engine parameter or ensure file has correct extension."
    )


def open_data(href: str, engine: str | None = None, **kwargs: Any) -> xr.Dataset:
    """
    Open a NetCDF or GRIB file with xarray using dask lazy loading.

    Parameters
    ----------
    href : str
        Path or file:// URI to the data file.
    engine : str, optional
        Explicit xarray engine to use ('netcdf4', 'h5netcdf', 'cfgrib').
        If not provided, auto-detection is attempted.
    **kwargs : Any
        Additional keyword arguments passed to xr.open_dataset.

    Returns
    -------
    xr.Dataset
        Lazily-loaded xarray Dataset with dask chunking.

    Raises
    ------
    FileNotFoundError
        If the file does not exist.
    ValueError
        If the file format cannot be determined.
    """
    path = _strip_file_uri(href)
    path_obj = Path(path)

    if not path_obj.exists():
        raise FileNotFoundError(f"Data file not found: {path}")

    logger.debug(f"Opening data file: {path}")

    # Set default chunking for lazy loading
    if "chunks" not in kwargs:
        kwargs["chunks"] = "auto"

    # Determine engine if not specified
    if engine is None:
        fmt = _detect_format(path)
        engine = "cfgrib" if fmt == "grib" else "netcdf4"
        logger.debug(f"Auto-detected format: {fmt}, using engine: {engine}")

    try:
        ds = xr.open_dataset(path, engine=engine, **kwargs)
        logger.info(f"Successfully opened dataset with {len(ds.data_vars)} variables")
        return ds
    except Exception as e:
        logger.error(f"Failed to open dataset: {e}")
        raise


def get_data_metadata(ds: xr.Dataset) -> dict[str, Any]:
    """
    Extract metadata from an xarray Dataset.

    Parameters
    ----------
    ds : xr.Dataset
        The xarray Dataset to analyze.

    Returns
    -------
    dict
        Metadata dictionary containing:
        - variables: list of variable names with their attributes
        - dimensions: dict of dimension names to sizes
        - coordinates: dict of coordinate names with their ranges
        - global_attrs: dataset-level attributes
    """
    metadata: dict[str, Any] = {
        "variables": [],
        "dimensions": {k: int(v) for k, v in ds.dims.items()},
        "coordinates": {},
        "global_attrs": _to_native(dict(ds.attrs)),
    }

    # Extract variable information
    for var_name, var_data in ds.data_vars.items():
        var_info = {
            "name": var_name,
            "dims": list(var_data.dims),
            "shape": [int(s) for s in var_data.shape],
            "dtype": str(var_data.dtype),
            "attrs": _to_native(dict(var_data.attrs)),
        }

        # Add standard_name and long_name if available
        if "standard_name" in var_data.attrs:
            var_info["standard_name"] = var_data.attrs["standard_name"]
        if "long_name" in var_data.attrs:
            var_info["long_name"] = var_data.attrs["long_name"]
        if "units" in var_data.attrs:
            var_info["units"] = var_data.attrs["units"]

        metadata["variables"].append(var_info)

    # Extract coordinate ranges
    for coord_name, coord_data in ds.coords.items():
        try:
            coord_values = coord_data.values
            coord_info: dict[str, Any] = {
                "dims": list(coord_data.dims),
                "size": int(coord_data.size),
                "dtype": str(coord_data.dtype),
            }

            # Add min/max for numeric coordinates
            if coord_data.dtype.kind in ("i", "u", "f"):  # integer, unsigned, float
                coord_info["min"] = float(coord_values.min())
                coord_info["max"] = float(coord_values.max())

            # Handle datetime coordinates
            if coord_data.dtype.kind == "M":  # datetime64
                coord_info["min"] = str(coord_values.min())
                coord_info["max"] = str(coord_values.max())

            # Add units if available
            if "units" in coord_data.attrs:
                coord_info["units"] = coord_data.attrs["units"]

            metadata["coordinates"][coord_name] = coord_info
        except Exception as e:
            logger.warning(f"Could not extract range for coordinate {coord_name}: {e}")
            metadata["coordinates"][coord_name] = {
                "dims": list(coord_data.dims),
                "size": int(coord_data.size),
                "error": str(e),
            }

    return metadata
