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

VariableName = str
"""A data variable name, e.g. 'tas'."""

License = str
"""A license identifier, e.g. an SPDX id or 'proprietary'."""

ExtensionUrl = str
"""A canonical extension schema URL (the value side of the registry; the name
side is the ``Extension`` StrEnum in registry.py)."""

Href = str
"""A STAC asset href — a file:// or protocol URI."""

BBox = list[float]
"""A bounding box, [west, south, east, north]."""

Geometry = dict
"""A GeoJSON geometry object (forwarded verbatim onto the STAC Item)."""


class CubeDimension(TypedDict, total=False):
    """One entry in a STAC datacube ``cube:dimensions`` mapping.

    ``type`` is ``"temporal"``/``"spatial"``, ``axis`` names the spatial axis
    (``x``/``y``/``z``), ``extent`` is the ``[min, max]`` span, ``unit`` the
    coordinate's units. Any reader produces this shape, not just NetCDF.
    """

    type: str
    axis: str
    extent: list
    unit: str


CubeDimensions = dict[str, CubeDimension]
"""A STAC datacube Dimensions object, keyed by dimension name."""


class ScannedVariable(TypedDict, total=False):
    """One entry in a scanner's ``variables`` list, before datacube mapping.

    Every key is optional; ``name`` identifies the variable, the rest are the
    CF-style attributes a scanner may extract.
    """

    name: VariableName
    units: str
    dimensions: list[str]
    description: str
    long_name: str
    standard_name: str


class FileMetadata(TypedDict, total=False):
    """The metadata a Reader (e.g. NetCDFReader) produces for one file.

    Every key is optional — a reader fills what it can extract. ``dimensions``
    is a STAC datacube Dimensions object, forwarded verbatim (opaque here).
    """

    variable: str
    variables: list[ScannedVariable]
    component: ComponentName
    format: str
    dimensions: CubeDimensions
    datetime_start: datetime
    datetime_end: datetime
    datetime_str: str
    frequency: str
    geometry: Geometry
    bbox: BBox
