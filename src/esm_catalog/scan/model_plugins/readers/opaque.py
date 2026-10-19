"""Opaque binary restart dumps: claim + no-op reader.

Covers restart files with no public, parseable structure -- cataloguing
means recording that the file exists (path/checksum/size, filled in by the
scan layer itself from the OutputFile, not by a Reader), not extracting
scientific content that isn't there to extract:

- LPJ-GUESS's per-task state dump (``restart/lpj_guess/lpjg_state_<year>/
  <n>.state``) -- a raw internal serialization of the model's in-memory
  patch/individual state, confirmed live (43MB, no text header, no magic
  bytes) against a real AWI-ESM3 run.
- FESOM's iceberg restart (``restart/fesom/iceberg.restart[.ISM]``) --
  confirmed live to be 0 bytes whenever the iceberg module produced nothing
  for the segment; FESOM's real oce/ice/bio restarts are NetCDF files
  inside directories named ``fesom.<date>.<stream>.restart`` and never reach
  this claimer (a directory is excluded before format detection).

Never reads the file: a 43MB binary with nothing to extract isn't worth a
network round trip just to confirm it opens.
"""

from __future__ import annotations

from typing import Optional

from upath import UPath

from esm_catalog.scan.format import FileFormat
from esm_catalog.scan.readers.format_plugins import hookimpl as format_hookimpl
from esm_catalog.scan.readers.netcdf.coords import _GLOBAL_BBOX, _bbox_to_polygon
from esm_catalog.scan.readers.plugins import hookimpl as reader_hookimpl
from esm_catalog.types import FileMetadata

__all__ = ["OpaqueBinaryReader"]


class OpaqueBinaryReader:
    """Catalogues a file's existence without parsing its content.

    No per-task/per-gridcell restart dump carries a meaningful spatial
    extent of its own, so this reuses the same global-bbox fallback
    ``coords.py`` already uses for a netcdf file with no lat/lon coords --
    a pystac Item with ``geometry=None`` is spec-legal, but
    ``storage/geoparquet.py``'s writer unconditionally calls
    ``shapely.geometry.shape(...)`` on it and crashes (confirmed live:
    writing the fx shard once a real restart dump without a global bbox
    hit it).
    """

    supports_remote = True

    def read(self, path: UPath) -> FileMetadata:
        return FileMetadata(
            format=str(FileFormat.opaque_binary),
            bbox=_GLOBAL_BBOX,
            geometry=_bbox_to_polygon(_GLOBAL_BBOX),
        )


@format_hookimpl
def claim_by_suffix(suffix: str, component: Optional[str]) -> Optional[FileFormat]:
    if component == "lpj_guess" and suffix == ".state":
        return FileFormat.opaque_binary
    if component == "fesom" and suffix in (".restart", ".ism"):
        return FileFormat.opaque_binary
    return None


@reader_hookimpl
def get_reader(file_format: FileFormat) -> Optional[OpaqueBinaryReader]:
    return OpaqueBinaryReader() if file_format == FileFormat.opaque_binary else None
