"""File-format detection for the scan layer.

A file's format is resolved by extension first, then a magic-byte sniff — so
extension-less ESM model output (ECHAM/FESOM write files like ``expid_200001.01``)
is still recognised. The returned ``FileFormat`` selects the reader (see
``reader.py``); detection never opens the file with a heavy library.
"""

from __future__ import annotations

from enum import auto

from upath import UPath

from esm_catalog._compat import StrEnum


class FileFormat(StrEnum):
    """A scannable file format, as resolved by :func:`detect`."""

    netcdf = auto()
    grib = auto()


class UnknownFormatError(ValueError):
    """Raised when a path matches no known format by extension or magic bytes."""


_NETCDF_SUFFIXES = frozenset({".nc", ".nc4", ".cdf", ".netcdf"})
"""Filename suffixes that unambiguously mean NetCDF."""

_GRIB_SUFFIXES = frozenset({".grb", ".grb2", ".grib", ".grib2"})
"""Filename suffixes that unambiguously mean GRIB."""

_GRIB_MAGIC = b"GRIB"
"""GRIB1/GRIB2 signature at offset 0."""

_HDF5_MAGIC = b"\x89HDF"
"""NetCDF-4 / HDF5 signature at offset 0."""

_CDF_MAGIC = b"CDF"
"""Classic NetCDF signature (CDF\\x01 / \\x02 / \\x05) at offset 0."""


def detect(path: UPath) -> FileFormat:
    """Resolve the :class:`FileFormat` of *path*.

    Extension is tried first (cheap, and authoritative when present); otherwise
    the first four bytes are sniffed, which catches the extension-less files ESM
    models emit.

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
    if suffix in _NETCDF_SUFFIXES:
        return FileFormat.netcdf
    if suffix in _GRIB_SUFFIXES:
        return FileFormat.grib
    return _sniff(path)


def _sniff(path: UPath) -> FileFormat:
    """Identify *path* by its first four magic bytes."""
    with path.open("rb") as handle:
        head = handle.read(4)
    if head.startswith(_GRIB_MAGIC):
        return FileFormat.grib
    if head.startswith(_HDF5_MAGIC) or head.startswith(_CDF_MAGIC):
        return FileFormat.netcdf
    raise UnknownFormatError(
        f"{path}: no known format (extension {path.suffix!r}, magic {head!r})."
    )
