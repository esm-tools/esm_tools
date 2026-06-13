"""Auto-detect file format and dispatch to the correct scanner.

Supports both local paths and remote URIs via fsspec/UPath.
"""

from __future__ import annotations

from pathlib import Path, PurePosixPath
from typing import TYPE_CHECKING, Union

from esm_catalog.scan.netcdf import scan_netcdf

if TYPE_CHECKING:
    from upath import UPath


class UnsupportedFormatError(ValueError):
    """Raised when the file format cannot be determined or is not supported."""


_NETCDF_SUFFIXES = {".nc", ".nc4", ".nc3", ".cdf", ".h5", ".hdf5", ".hdf"}
_GRIB_SUFFIXES = {".grb", ".grb2", ".grib", ".grib2"}

# Magic bytes for format sniffing when the suffix is absent or non-standard
# (e.g. ECHAM output: basic-001_185001.01_echam has no .grb extension)
_MAGIC_GRIB = b"GRIB"
_MAGIC_HDF5 = b"\x89HDF"
_MAGIC_NC3  = b"CDF\x01"
_MAGIC_NC4  = b"CDF\x02"


def _dispatch_grib(path: "Union[Path, UPath]") -> dict:
    """Dispatch GRIB file to the appropriate scanner (lazy: GRIB/ECHAM land in PR-A1b-2)."""
    from pathlib import Path as LocalPath
    try:
        from esm_catalog.scan.echam import is_echam_file, scan_echam
        from esm_catalog.scan.grib import scan_grib
    except ImportError as exc:  # pragma: no cover
        raise UnsupportedFormatError(
            "GRIB/ECHAM scanning is not available yet (lands in PR-A1b-2)."
        ) from exc
    local_path = LocalPath(path) if not isinstance(path, LocalPath) else path
    if is_echam_file(local_path):
        return scan_echam(path)
    return scan_grib(path)


def _sniff_format(path: "Union[Path, UPath]") -> str | None:
    """Return 'grib', 'netcdf', or None by reading the first 4 magic bytes.

    Works with both local Path and remote UPath objects.
    """
    try:
        # UPath and Path both support .open()
        with path.open("rb") as fh:
            magic = fh.read(4)
    except (OSError, IOError):
        return None
    if magic == _MAGIC_GRIB:
        return "grib"
    if magic in (_MAGIC_HDF5, _MAGIC_NC3, _MAGIC_NC4):
        return "netcdf"
    return None


def scan_file(path: "Union[Path, UPath, str]") -> dict:
    """Detect the format of *path* and return its metadata dict.

    Supports local paths and remote URIs (ssh://, scoutfs://, s3://, etc.).

    Detection order:
    1. File extension (fast path for conventionally named files).
    2. Magic-byte sniffing (handles extension-less ECHAM/FESOM output).

    Raises UnsupportedFormatError for unknown or unsupported formats.
    """
    # Handle string paths (convert to UPath if URI, Path if local)
    if isinstance(path, str):
        from esm_catalog.uri import parse_uri
        path = parse_uri(path)

    # Get suffix (works for both Path and UPath)
    suffix = PurePosixPath(str(path)).suffix.lower()

    if suffix in _NETCDF_SUFFIXES:
        return scan_netcdf(path)

    if suffix in _GRIB_SUFFIXES:
        return _dispatch_grib(path)

    # Extension is absent or non-standard — try magic bytes
    fmt = _sniff_format(path)
    if fmt == "grib":
        return _dispatch_grib(path)
    if fmt == "netcdf":
        return scan_netcdf(path)

    raise UnsupportedFormatError(
        f"Unsupported file format '{suffix}' for: {path}\n"
        f"Supported: NetCDF ({', '.join(sorted(_NETCDF_SUFFIXES))}), "
        f"GRIB ({', '.join(sorted(_GRIB_SUFFIXES))})"
    )
