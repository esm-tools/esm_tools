"""Open a NetCDF file with xarray, local or remote.

Local paths use xarray's default engine; remote ``UPath``\\s are opened through
their own fsspec filesystem and handed to ``h5netcdf`` as a file object, because
``h5netcdf`` cannot consume a remote path directly. A dataset that fails time
decoding is reopened with ``decode_times=False`` for hand-decoding downstream.
"""

from __future__ import annotations

import xarray as xr
from loguru import logger
from upath import UPath

from esm_catalog.scan.reader import UnsupportedContentError


def _open_dataset(path: UPath) -> xr.Dataset:
    """Open *path* with xarray, retrying without time decoding on a cftime failure.

    Local paths open straight from the ``UPath`` with xarray's default engine.
    Remote paths (``memory://``, ``ssh://``, ``scoutfs://``, ``s3://``, ...) are
    opened through the ``UPath``'s own fsspec filesystem -- which already carries
    its storage options / credentials -- and handed to ``h5netcdf`` as a file
    object, because ``h5netcdf`` cannot consume a remote path directly. A dataset
    with a non-standard calendar or missing time units raises on decode; we then
    reopen with ``decode_times=False`` and decode the time coordinate by hand
    downstream. Each attempt opens a fresh handle, since a consumed remote stream
    cannot be re-read.
    """
    is_remote = bool(path.protocol) and path.protocol != "file"
    logger.debug("Opening NetCDF {} (remote={})", path, is_remote)

    def _open(**open_kwargs) -> xr.Dataset:
        if not is_remote:
            return xr.open_dataset(path, **open_kwargs)
        store = path.fs.open(path.path, "rb")
        try:
            return xr.open_dataset(store, engine="h5netcdf", **open_kwargs)
        except OSError as error:
            if _is_signature_error(error):
                raise UnsupportedContentError(
                    f"{path.name}: NetCDF-3 classic is not readable over a remote "
                    "stream; scan it on the cluster (local read) instead"
                ) from error
            raise

    try:
        return _open(decode_times=True)
    except (ValueError, OSError) as error:
        if _is_time_decode_error(error):
            logger.debug("Time decode failed for {}; retrying undecoded", path)
            return _open(decode_times=False)
        raise


def _is_time_decode_error(error: Exception) -> bool:
    """Return whether *error* is xarray failing to decode the time coordinate."""
    message = str(error)
    return "unable to decode time" in message or "Failed to decode" in message


def _is_signature_error(error: Exception) -> bool:
    """Return whether *error* is h5netcdf rejecting a non-HDF5 (e.g. NetCDF-3) file."""
    return "file signature not found" in str(error).lower()
