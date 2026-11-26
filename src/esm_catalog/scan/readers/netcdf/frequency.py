"""Infer a CF-style frequency code (CMIP ``frequency``) for a NetCDF dataset.

The signal is taken, in order, from the global ``frequency`` attribute, then from
the spacing of the CF time axis via :func:`xarray.infer_freq`. A non-uniform axis
yields ``None`` -- we do not guess -- and so does a time-invariant file. The
filename is never consulted; a stamp in a filename is not the data.
"""

from __future__ import annotations

from typing import Optional

import pandas as pd
import xarray as xr
from pandas.tseries import offsets as pd_offsets

from esm_catalog.scan.readers.netcdf.timeaxis import _time_coord


def _infer_frequency(dataset: xr.Dataset) -> Optional[str]:
    """Infer a CF-style frequency code (CMIP ``frequency``) for *dataset*.

    Returns ``None`` when there is no time coordinate (a time-invariant ``fx``
    file), when the axis has fewer than two steps, or when the spacing is not
    regular enough for :func:`xarray.infer_freq` to name it.
    """
    attr = dataset.attrs.get("frequency")
    if isinstance(attr, str) and attr:
        return attr.lower()

    coord = _time_coord(dataset)
    if coord is None or coord.size < 2:
        return None

    try:
        pandas_freq = xr.infer_freq(coord)
    except (ValueError, TypeError):
        return None
    if pandas_freq is None:
        return None
    return _cf_frequency(pandas_freq)


def _cf_frequency(pandas_freq: str) -> Optional[str]:
    """Map a pandas offset alias (``6h``, ``D``, ``MS``, ``YS`` ...) to a CMIP code.

    Calendar offsets are matched by type rather than alias string, so anchored
    names (``YS-JAN``) and pandas' shifting alias spellings do not slip through.
    ``None`` for anything with no CMIP frequency (e.g. weekly).
    """
    offset = pd.tseries.frequencies.to_offset(pandas_freq)
    if offset is None:
        return None
    if isinstance(offset, (pd_offsets.YearBegin, pd_offsets.YearEnd)):
        return "yr"
    if isinstance(offset, (pd_offsets.MonthBegin, pd_offsets.MonthEnd)):
        return "mon"
    if isinstance(offset, pd_offsets.Day):
        return "day"
    try:
        hours = pd.Timedelta(offset).total_seconds() / 3600.0
    except (ValueError, TypeError):
        return None
    if hours < 1.0:
        return "subhr"
    if hours == 1.0:
        return "1hr"
    if hours == 3.0:
        return "3hr"
    if hours == 6.0:
        return "6hr"
    if hours < 24.0:
        return f"{int(hours)}hr"
    return "day"
