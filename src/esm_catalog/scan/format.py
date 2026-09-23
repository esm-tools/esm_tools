"""File-format detection for the scan layer.

A file's format is resolved by extension first, then a magic-byte sniff — so
extension-less ESM model output (ECHAM/FESOM write files like ``expid_200001.01``)
is still recognised. The returned ``FileFormat`` selects the reader (see
``reader.py``); detection never opens the file with a heavy library.

Which suffixes and magic bytes belong to a format is not known here: each
reader module claims its own via the pluggy hooks in
:mod:`esm_catalog.scan.readers.format_plugins` -- adding a format never means
editing this module.
"""

from __future__ import annotations

from enum import auto

from upath import UPath

from esm_catalog._compat import StrEnum
from esm_catalog.scan.readers.format_plugins import get_format_plugin_manager


class FileFormat(StrEnum):
    """A scannable file format, as resolved by :func:`detect`."""

    netcdf = auto()
    grib = auto()


class UnknownFormatError(ValueError):
    """Raised when a path matches no known format by extension or magic bytes."""


def detect(path: UPath) -> FileFormat:
    """Resolve the :class:`FileFormat` of *path*.

    Extension is tried first (cheap, and authoritative when present) via the
    :func:`~esm_catalog.scan.readers.format_plugins.FormatSpec.claim_by_suffix`
    hook; otherwise the first four bytes are sniffed via
    :func:`~esm_catalog.scan.readers.format_plugins.FormatSpec.claim_by_magic`,
    which catches the extension-less files ESM models emit.

    Parameters
    ----------
    path : UPath
        The file to identify (local or remote).

    Returns
    -------
    FileFormat
        The detected format.

    Raises
    ------
    UnknownFormatError
        If neither extension nor magic bytes identify a known format.
    """
    suffix = path.suffix.lower()
    by_suffix = get_format_plugin_manager().hook.claim_by_suffix(suffix=suffix)
    if by_suffix is not None:
        return by_suffix
    return _sniff(path)


def _sniff(path: UPath) -> FileFormat:
    """Identify *path* by its first four magic bytes."""
    with path.open("rb") as handle:
        head = handle.read(4)
    by_magic = get_format_plugin_manager().hook.claim_by_magic(head=head)
    if by_magic is not None:
        return by_magic
    raise UnknownFormatError(
        f"{path}: no known format (extension {path.suffix!r}, magic {head!r})."
    )
