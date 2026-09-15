"""Build the STAC datacube ``cube:dimensions`` mapping for a NetCDF dataset.

Each dimension is classified through :mod:`cf_xarray`'s axis detection
(``standard_name``/``axis``/``units``), so a temporal axis is found whatever it
is named and a spatial axis is tagged ``x``/``y``/``z`` -- no hard-coded name
lists, which never stay complete.
"""

from __future__ import annotations

import cf_xarray  # noqa: F401 - registers the .cf accessor on Dataset/DataArray
import xarray as xr

from esm_catalog.scan.readers.netcdf.timeaxis import _time_extent_iso
from esm_catalog.types import CubeDimension, CubeDimensions

_CF_AXIS_TO_STAC = {"X": "x", "Y": "y", "Z": "z"}
"""CF axis letters mapped to the STAC datacube spatial ``axis`` value."""


def _extract_dimensions(dataset: xr.Dataset) -> CubeDimensions:
    """Build the STAC datacube ``cube:dimensions`` mapping for *dataset*."""
    axis_of = _axis_of(dataset)
    dimensions: CubeDimensions = {}
    for name in dataset.sizes:
        coord = dataset.coords.get(name)
        entry: CubeDimension = {"type": "spatial", "extent": [None, None]}
        if coord is not None:
            values = coord.values
            entry["extent"] = [_to_python(values.min()), _to_python(values.max())]
            units = coord.attrs.get("units", "")
            if units:
                entry["unit"] = units

        axis = axis_of.get(str(name))
        if axis == "T":
            entry["type"] = "temporal"
            entry["extent"] = _time_extent_iso(coord)
        elif axis in _CF_AXIS_TO_STAC:
            entry["type"] = "spatial"
            entry["axis"] = _CF_AXIS_TO_STAC[axis]

        dimensions[str(name)] = entry
    return dimensions


_CF_COORDINATE_TO_AXIS = {
    "latitude": "Y",
    "longitude": "X",
    "vertical": "Z",
    "time": "T",
}
"""cf_xarray coordinate kinds mapped to the axis letter they stand on."""


def _axis_of(dataset: xr.Dataset) -> dict[str, str]:
    """Map each coordinate *and its dimensions* to its CF axis letter (X/Y/Z/T).

    cf_xarray reports geographic coordinates under ``coordinates``
    (``latitude``/``longitude``/``vertical``/``time``) and formal axes under
    ``axes`` (``X``/``Y``/``Z``/``T``); a lat/lon pair may appear in only the
    former, so both are consulted. A 2-D geographic coordinate (``nav_lon(y, x)``)
    leaves its dimensions (``x``) unlabelled, so each axis is also projected onto
    the coordinate's dimensions.
    """
    mapping: dict[str, str] = {}

    def _assign(coord_name: str, axis: str) -> None:
        mapping.setdefault(coord_name, axis)
        coord = dataset.coords.get(coord_name)
        if coord is not None:
            for dim in coord.dims:
                mapping.setdefault(str(dim), axis)

    for kind, names in dataset.cf.coordinates.items():
        axis = _CF_COORDINATE_TO_AXIS.get(kind)
        if axis:
            for name in names:
                _assign(name, axis)
    for axis, names in dataset.cf.axes.items():
        for name in names:
            _assign(name, axis)
    return mapping


def _to_python(value):
    """Return a numpy scalar as a native Python type, or *value* unchanged."""
    if hasattr(value, "item"):
        return value.item()
    return value
