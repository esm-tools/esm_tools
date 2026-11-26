"""OIFS enrichment for the NetCDF reader (a pluggable model extension).

OIFS run with ``with_xios: true`` (the AWI-CM3/AWI-ESM3 default) writes NetCDF
through XIOS rather than native GRIB. XIOS files carry two time axes,
``time_counter`` and ``time_centered``, plus ``*_bounds`` variables.
``cf_xarray`` finds two time-like candidates and refuses to pick one
(``dataset.cf["time"]`` raises ``KeyError``), so the base reader's
``_time_coord`` cannot resolve an axis either -- confirmed live against a
real AWI-CM3 v3.2 run (FESOM2 + OIFS/XIOS): the file loses its time range and
frequency, landing in the time-invariant (``fx``) shard. This enricher picks
``time_counter`` deliberately (not ``time_centered`` -- a bin-center
convention, not the file's own step axis), restores the time range and
frequency from the bounds, and drops the ``*_bounds`` pseudo-variables.
"""

from __future__ import annotations

from typing import Optional

import numpy as np

from esm_catalog.scan.enrichers import hookimpl
from esm_catalog.scan.format import FileFormat
from esm_catalog.scan.readers.netcdf.frequency import _cf_frequency
from esm_catalog.scan.readers.netcdf.timeaxis import _time_bounds

_AXES = ("time_counter", "time_centered")


def _pick_axis(ds):
    for name in _AXES:
        if name in ds.coords and ds[name].ndim == 1:
            return name
    return None


def _is_bounds(name: str) -> bool:
    return name.endswith("_bounds") or name.endswith("_bnds")


def _frequency(ds, axis: str) -> Optional[str]:
    """Step length from the axis bounds (exact), else from point spacing."""
    coord = ds[axis]
    bname = coord.attrs.get("bounds") or f"{axis}_bounds"
    try:
        if bname in ds and ds[bname].ndim == 2 and ds[bname].shape[0] >= 1:
            b = ds[bname].isel({ds[bname].dims[0]: 0}).values
            days = float((b[1] - b[0]) / np.timedelta64(1, "D")) if np.issubdtype(
                b.dtype, np.datetime64) else None
            if days is None:
                import cftime  # noqa: F401 -- values may be cftime objects

                days = (b[1] - b[0]).total_seconds() / 86400.0
            if 27.5 <= days <= 31.5:
                return "mon"
            if 359.5 <= days <= 366.5:
                return "yr"
            if abs(days - 1.0) < 1e-6:
                return "day"
            if days > 0:
                return _cf_frequency(f"{round(days * 24 * 3600)}s")
    except Exception:  # noqa: BLE001
        pass
    import xarray as xr

    if coord.size >= 2:
        try:
            f = xr.infer_freq(coord)
            return _cf_frequency(f) if f else None
        except Exception:  # noqa: BLE001
            return None
    return None


@hookimpl
def enrich_metadata(path, file_format, metadata, datasets):
    if file_format != FileFormat.netcdf or not datasets:
        return
    ds = datasets[0]
    if "datetime_start" not in metadata:
        axis = _pick_axis(ds)
        if axis is not None:
            start, end = _time_bounds(ds[axis])
            if start is not None:
                metadata["datetime_start"] = start
                metadata["datetime_end"] = end
                metadata["datetime_str"] = start.strftime("%Y%m")
                freq = _frequency(ds, axis)
                if freq and "frequency" not in metadata:
                    metadata["frequency"] = freq
    variables = metadata.get("variables")
    if variables:
        kept = [v for v in variables if not _is_bounds(str(v.get("name", "")))]
        metadata["variables"] = kept
        if metadata.get("variable") and _is_bounds(str(metadata["variable"])) and kept:
            metadata["variable"] = kept[0]["name"]
