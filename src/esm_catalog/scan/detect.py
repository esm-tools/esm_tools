"""Auto-detect file format and dispatch to the correct scanner."""

from pathlib import Path

from esm_catalog.scan.grib import scan_grib
from esm_catalog.scan.netcdf import scan_netcdf


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


def _sniff_format(path: Path) -> str | None:
    """Return 'grib', 'netcdf', or None by reading the first 4 magic bytes."""
    try:
        with open(path, "rb") as fh:
            magic = fh.read(4)
    except OSError:
        return None
    if magic == _MAGIC_GRIB:
        return "grib"
    if magic in (_MAGIC_HDF5, _MAGIC_NC3, _MAGIC_NC4):
        return "netcdf"
    return None


def scan_file(path: Path) -> dict:
    """Detect the format of *path* and return its metadata dict.

    Detection order:
    1. File extension (fast path for conventionally named files).
    2. Magic-byte sniffing (handles extension-less ECHAM/FESOM output).

    Raises UnsupportedFormatError for unknown or unsupported formats.
    """
    path = Path(path)
    suffix = path.suffix.lower()

    if suffix in _NETCDF_SUFFIXES:
        return scan_netcdf(path)

    if suffix in _GRIB_SUFFIXES:
        return scan_grib(path)

    # Extension is absent or non-standard — try magic bytes
    fmt = _sniff_format(path)
    if fmt == "grib":
        return scan_grib(path)
    if fmt == "netcdf":
        return scan_netcdf(path)

    raise UnsupportedFormatError(
        f"Unsupported file format '{suffix}' for: {path}\n"
        f"Supported: NetCDF ({', '.join(sorted(_NETCDF_SUFFIXES))}), "
        f"GRIB ({', '.join(sorted(_GRIB_SUFFIXES))})"
    )
