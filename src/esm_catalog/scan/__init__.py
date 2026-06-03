from __future__ import annotations
"""File scanning: extract metadata from NetCDF and GRIB files."""

from esm_catalog.scan.context import (
    CollectionContext,
    RestartFileError,
    is_restart_file,
    resolve_context,
)
from esm_catalog.scan.detect import UnsupportedFormatError, scan_file

__all__ = [
    "CollectionContext",
    "RestartFileError",
    "UnsupportedFormatError",
    "is_restart_file",
    "resolve_context",
    "scan_file",
]
