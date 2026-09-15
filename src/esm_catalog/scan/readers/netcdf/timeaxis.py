"""The time coordinate: find it by CF attributes, decode it to ``(start, end)``.

The axis is located with :mod:`cf_xarray` (``standard_name``/``axis``/``units``),
not by the name ``time`` -- models are not tidy (old FESOM called it ``T``).
Handles decoded ``datetime64`` values, ``cftime`` objects, and integer-encoded
time xarray could not decode (via the coordinate's CF units and calendar). A
missing or unanchored time axis yields ``(None, None)`` -- the time-invariant case.
"""

from __future__ import annotations

from datetime import datetime, timezone
from typing import Optional

import cf_xarray  # noqa: F401 - registers the .cf accessor on Dataset/DataArray
import numpy as np
import xarray as xr
from loguru import logger

DateTimeRange = tuple[Optional[datetime], Optional[datetime]]
"""The ``(start, end)`` of a time coordinate; either bound is ``None`` when absent."""

_EPOCH = np.datetime64("1970-01-01T00:00:00")
"""The Unix epoch as ``datetime64``, for converting numpy times to aware datetimes."""


def _time_coord(dataset: xr.Dataset) -> Optional[xr.DataArray]:
    """Return the CF-identified time coordinate, or ``None`` if the file has none.

    Uses :mod:`cf_xarray` so an axis is found by its ``standard_name``/``axis``/CF
    ``units`` rather than by being named ``time``. The absence of a time axis is
    meaningful -- it is the time-invariant (``fx``) signal.
    """
    try:
        return dataset.cf["time"]
    except (KeyError, AttributeError):
        return None


def _extract_time_range(dataset: xr.Dataset) -> DateTimeRange:
    """Return the ``(start, end)`` datetimes of the CF time coordinate.

    Handles decoded ``datetime64`` values, ``cftime`` objects, and integer-encoded
    time that xarray could not decode (via the coordinate's CF units and calendar).
    Returns ``(None, None)`` when there is no time coordinate -- the time-invariant
    case.
    """
    coord = _time_coord(dataset)
    if coord is None:
        return None, None
    try:
        return _time_bounds(coord)
    except Exception as error:  # noqa: BLE001 - never let one bad file abort a scan
        logger.warning("Could not extract time range from {}: {}", dataset, error)
        return None, None


def _time_bounds(coord: xr.DataArray) -> DateTimeRange:
    """The ``(start, end)`` datetimes of a time *coord*, decoding integer time.

    Returns ``(None, None)`` for an empty axis, or integer time with no units to
    anchor it. Shared by :func:`_extract_time_range` and :func:`_time_extent_iso`.
    """
    values = coord.values
    if len(values) == 0:
        return None, None
    first, last = values[0], values[-1]
    if np.issubdtype(values.dtype, np.integer):
        first, last = _decode_integer_times(coord, [first, last])
        if first is None:
            return None, None
    return _to_utc_datetime(first), _to_utc_datetime(last)


def _decode_integer_times(coord: xr.DataArray, values: list) -> list:
    """Decode integer *values* to cftime objects using the coord's units/calendar.

    Returns ``[None, None]`` when the coordinate carries no ``units`` attribute, so
    there is nothing to anchor the encoding to.
    """
    import cftime

    units = coord.attrs.get("units", "")
    calendar = coord.attrs.get("calendar", "standard")
    if not units:
        return [None, None]
    return list(cftime.num2date(values, units=units, calendar=calendar))


def _to_utc_datetime(value) -> datetime:
    """Convert a cftime object or ``datetime64`` scalar to a UTC-aware datetime."""
    if hasattr(value, "year"):
        return datetime(
            value.year,
            value.month,
            value.day,
            value.hour,
            value.minute,
            value.second,
            tzinfo=timezone.utc,
        )
    seconds = (value - _EPOCH) / np.timedelta64(1, "s")
    return datetime.fromtimestamp(float(seconds), tz=timezone.utc)


def _time_extent_iso(coord: Optional[xr.DataArray]) -> list:
    """Return the ``[start, end]`` of *coord* as ISO-8601 strings, or ``[None, None]``."""
    if coord is None:
        return [None, None]
    try:
        start, end = _time_bounds(coord)
    except Exception:  # noqa: BLE001 - a malformed axis must not abort the scan
        return [None, None]
    if start is None:
        return [None, None]
    return [start.isoformat(), end.isoformat()]
