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
from typing import Optional

from upath import UPath

from esm_catalog._compat import StrEnum
from esm_catalog.scan.readers.format_plugins import get_format_plugin_manager


class FileFormat(StrEnum):
    """A scannable file format, as resolved by :func:`detect`."""

    netcdf = auto()
    grib = auto()
    lpjguess_table = auto()
    opaque_binary = auto()


class UnknownFormatError(ValueError):
    """Raised when a path matches no known format by extension or magic bytes."""


def needs_sniff(path: UPath, component: Optional[str] = None) -> bool:
    """Whether :func:`detect` would have to sniff *path*'s magic bytes.

    Lets a caller fetch a remote, extension-less file once and reuse those
    same bytes for both the sniff and the eventual read, instead of paying
    one remote open for the sniff and a second for the read -- see
    ``scan/ingest.py``'s ``_read_output_file``.
    """
    suffix = path.suffix.lower()
    claimed = get_format_plugin_manager().hook.claim_by_suffix(
        suffix=suffix, component=component
    )
    return claimed is None


def detect(path: UPath, component: Optional[str] = None) -> FileFormat:
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
    component : str or None, optional
        The owning component (e.g. ``'lpj_guess'``), when known -- lets a
        claimer scope a generic-looking suffix/signature to one model's own
        convention (e.g. a bare ``.out`` extension, which many unrelated
        tools also use) instead of claiming it unconditionally for every
        file sharing that suffix. ``None`` for a context that genuinely has
        no component yet. Forwarded to both hooks verbatim; an existing
        claimer that doesn't declare this parameter in its own signature
        (netcdf, grib) is simply never passed it -- pluggy only passes the
        subset of a hookspec's parameters each hookimpl actually asks for by
        name, so this is not a breaking change to either.

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
    by_suffix = get_format_plugin_manager().hook.claim_by_suffix(
        suffix=suffix, component=component
    )
    if by_suffix is not None:
        return by_suffix
    return _sniff(path, component)


def _sniff(path: UPath, component: Optional[str] = None) -> FileFormat:
    """Identify *path* by its first four magic bytes."""
    with path.open("rb") as handle:
        head = handle.read(4)
    by_magic = get_format_plugin_manager().hook.claim_by_magic(
        head=head, component=component
    )
    if by_magic is not None:
        return by_magic
    raise UnknownFormatError(
        f"{path}: no known format (extension {path.suffix!r}, magic {head!r})."
    )
