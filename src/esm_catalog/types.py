"""Shared domain vocabulary for esm_catalog.

Lightweight type aliases (not data models — those are the pydantic models in
models.py) naming the shapes that recur across the catalog builders and
extensions. Each alias carries an attribute docstring so its meaning surfaces
on IDE hover wherever it is used.
"""

from __future__ import annotations

from datetime import datetime
from typing import Optional

from pydantic import BaseModel, ConfigDict

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


class ScannedVariable(BaseModel):
    """One entry in a scanner's ``variables`` list, before datacube mapping.

    Every field is optional; ``name`` identifies the variable, the rest are the
    CF-style attributes a scanner may extract. ``extra="allow"`` lets a scanner
    attach attributes not enumerated here.
    """

    model_config = ConfigDict(extra="allow")

    name: Optional[VariableName] = None
    units: Optional[str] = None
    dimensions: list[str] = []
    description: Optional[str] = None
    long_name: Optional[str] = None
    standard_name: Optional[str] = None


class FileMetadata(BaseModel):
    """The metadata a scanner (scan_netcdf/scan_grib) produces for one file.

    A scanner fills what it can extract; every field is optional and
    ``extra="allow"`` permits keys not enumerated here. Validated at the scan
    boundary, so a malformed reader output is caught where it is produced.
    ``dimensions`` is a STAC datacube Dimensions object, forwarded verbatim.
    """

    model_config = ConfigDict(extra="allow")

    variable: Optional[str] = None
    variables: list[ScannedVariable] = []
    component: Optional[ComponentName] = None
    format: Optional[str] = None
    dimensions: dict = {}
    datetime_start: Optional[datetime] = None
    datetime_end: Optional[datetime] = None
    datetime_str: Optional[str] = None
    output_frequency: Optional[str] = None
    geometry: Optional[dict] = None
    bbox: Optional[BBox] = None
