"""Spatial coordinates: latitude/longitude detection, bounding box, geometry.

Latitude and longitude are located by their CF attributes (``standard_name``,
``units``, ``axis``) through :mod:`cf_xarray`, not by a fixed list of names -- no
such list is ever complete. A file whose lat/lon cannot be found or validated
falls back to the whole-Earth extent.
"""

from __future__ import annotations

from typing import Optional

import cf_xarray  # noqa: F401 - registers the .cf accessor on Dataset/DataArray
import numpy as np
import xarray as xr

from esm_catalog.types import BBox, Geometry

SpatialExtent = tuple[BBox, Geometry]
"""A bounding box paired with its GeoJSON polygon."""

_GLOBAL_BBOX: BBox = [-180.0, -90.0, 180.0, 90.0]
"""The whole-Earth fallback extent, used when no valid geographic coords are found."""


def _extract_bbox(dataset: xr.Dataset) -> SpatialExtent:
    """Return the ``(bbox, geometry)`` from latitude/longitude coordinates.

    Falls back to the whole-Earth extent when no geographic coordinates are present
    or their ranges fail validation.
    """
    latitude = _cf_coord(dataset, "latitude")
    longitude = _cf_coord(dataset, "longitude")
    if latitude is None or longitude is None:
        return _global_extent()

    try:
        lat_min, lat_max = float(np.nanmin(latitude)), float(np.nanmax(latitude))
        lon_min, lon_max = float(np.nanmin(longitude)), float(np.nanmax(longitude))
    except (ValueError, TypeError):
        return _global_extent()

    in_range = -90 <= lat_min <= lat_max <= 90 and -180 <= lon_min <= lon_max <= 180
    if not in_range:
        return _global_extent()

    bbox: BBox = [lon_min, lat_min, lon_max, lat_max]
    return bbox, _bbox_to_polygon(bbox)


def _cf_coord(dataset: xr.Dataset, key: str) -> Optional[np.ndarray]:
    """Return the values of the CF coordinate *key* (``latitude``/``longitude``).

    ``None`` when :mod:`cf_xarray` cannot identify one -- the file carries no
    recognisable geographic axis of that kind.
    """
    try:
        return dataset.cf[key].values
    except (KeyError, AttributeError):
        return None


def _global_extent() -> SpatialExtent:
    """Return the whole-Earth bounding box and its polygon."""
    return _GLOBAL_BBOX, _bbox_to_polygon(_GLOBAL_BBOX)


def _bbox_to_polygon(bbox: BBox) -> Geometry:
    """Return the closed GeoJSON polygon of a ``[west, south, east, north]`` box."""
    lon_min, lat_min, lon_max, lat_max = bbox
    return {
        "type": "Polygon",
        "coordinates": [
            [
                [lon_min, lat_min],
                [lon_max, lat_min],
                [lon_max, lat_max],
                [lon_min, lat_max],
                [lon_min, lat_min],
            ]
        ],
    }
