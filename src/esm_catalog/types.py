"""Shared domain vocabulary for esm_catalog.

Lightweight type aliases (not data models — those are the pydantic models in
models.py) naming the shapes that recur across the catalog builders and
extensions. Each alias carries an attribute docstring so its meaning surfaces
on IDE hover wherever it is used.
"""

from __future__ import annotations

from datetime import datetime
from typing import TypedDict

ExperimentId = str
"""An experiment identifier, e.g. 'PI-CTRL'."""

ComponentName = str
"""A model component, e.g. 'echam', 'fesom'."""

License = str
"""A license identifier, e.g. an SPDX id or 'proprietary'."""

ExtensionUrl = str
"""A canonical extension schema URL (the value side of the registry; the name
side is the ``Extension`` StrEnum in registry.py)."""

Href = str
"""A STAC asset href — a file:// or protocol URI."""

BBox = list[float]
"""A bounding box, [west, south, east, north]."""


class FileMetadata(TypedDict, total=False):
    """The metadata a scanner (scan_netcdf/scan_grib) produces for one file.

    Every key is optional — a scanner fills what it can extract. ``dimensions``
    is a STAC datacube Dimensions object, forwarded verbatim (opaque here).
    """

    variable: str
    variables: list[dict]
    component: ComponentName
    format: str
    dimensions: dict
    datetime_start: datetime
    datetime_end: datetime
    datetime_str: str
    output_frequency: str
    geometry: dict
    bbox: BBox
