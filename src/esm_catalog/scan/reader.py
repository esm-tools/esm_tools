"""The pluggable reader registry: :class:`FileFormat` -> :class:`Reader`.

A ``Reader`` extracts :class:`~esm_catalog.types.FileMetadata` from one file of a
given format. Readers register themselves for a format, so the scan core
dispatches by format without importing any concrete reader — adding a format is
adding a reader plus a ``register`` call, never an edit to the core.
"""

from __future__ import annotations

from typing import Protocol, runtime_checkable

from loguru import logger
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


READERS: dict[FileFormat, Reader] = {}
"""The format -> reader registry; populated by :func:`register` at import time."""


def register(file_format: FileFormat, reader: Reader) -> None:
    """Register *reader* as the handler for *file_format*.

    Warns if a reader was already registered for *file_format*: a silent override
    almost always means two modules claiming the same format by accident.
    """
    if file_format in READERS:
        logger.warning(
            "reader for {} overridden: {} replaces {}",
            file_format,
            type(reader).__name__,
            type(READERS[file_format]).__name__,
        )
    READERS[file_format] = reader


def reader_for(file_format: FileFormat) -> Reader:
    """Return the registered :class:`Reader` for *file_format*.

    Raises
    ------
    LookupError
        If no reader is registered — the format is detectable but unhandled
        (e.g. GRIB before its reader ships).
    """
    try:
        return READERS[file_format]
    except KeyError:
        raise LookupError(f"no reader registered for format '{file_format}'") from None
