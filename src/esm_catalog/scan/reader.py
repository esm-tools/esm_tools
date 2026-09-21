"""The pluggable reader contract: :class:`FileFormat` -> :class:`Reader`.

A ``Reader`` extracts :class:`~esm_catalog.types.FileMetadata` from one file of a
given format. Readers register themselves for a format (see
:mod:`esm_catalog.scan.readers.plugins`), so the scan core dispatches by format
without importing any concrete reader — adding a format is adding a reader
plus a ``get_reader`` hookimpl, never an edit to the core.
"""

from __future__ import annotations

from typing import Protocol, runtime_checkable

from upath import UPath

from esm_catalog.scan.format import FileFormat
from esm_catalog.types import FileMetadata


class UnsupportedContentError(Exception):
    """A file whose format is recognised but whose content a reader cannot handle
    in the current context -- e.g. NetCDF-3 classic reached over a remote stream,
    which the HDF5-only backend cannot open (and which is too large to fetch
    whole). Treated as a clean skip (unsupported), never a read failure; a local
    scan of the same file, read through the default engine, catalogues it.
    """


@runtime_checkable
class Reader(Protocol):
    """Extracts :class:`~esm_catalog.types.FileMetadata` from one file.

    Attributes
    ----------
    supports_remote : bool
        Whether the reader can read a non-local path (a ``UPath`` whose protocol
        is not ``file``). The walk routes remote paths only to readers that
        declare ``True``; a local-only reader (e.g. one shelling out to a native
        library) declares ``False``.
    """

    supports_remote: bool

    def read(self, path: UPath) -> FileMetadata:
        """Read *path* and return its scanned metadata."""
        ...


def reader_for(file_format: FileFormat) -> Reader:
    """Return the registered :class:`Reader` for *file_format*.

    Raises
    ------
    LookupError
        If no reader is registered — the format is detectable but unhandled
        (e.g. GRIB before its reader ships).
    """
    from esm_catalog.scan.readers.plugins import get_reader_plugin_manager

    reader = get_reader_plugin_manager().hook.get_reader(file_format=file_format)
    if reader is None:
        raise LookupError(f"no reader registered for format '{file_format}'")
    return reader
