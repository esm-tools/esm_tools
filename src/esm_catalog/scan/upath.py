"""Universal Path support for remote and local filesystem access.

Enables scanning of files on remote systems via fsspec protocols:
    - ssh://host/path/to/file.nc
    - sftp://host/path/to/file.nc
    - scoutfs://host/path/to/file.nc  (AWI tape system)
    - s3://bucket/path/to/file.nc
    - file:///local/path/to/file.nc
    - /local/path/to/file.nc  (implicit file://)

Example usage::

    from esm_catalog.scan.upath import parse_uri, list_files, open_file

    # Parse a URI into UPath
    path = parse_uri("ssh://albedo0/work/user/experiment/outdata")

    # List NetCDF files recursively
    files = list_files(path, patterns=["*.nc", "*.nc4"])

    # Open a file for reading (works with any protocol)
    with open_file(path / "temp.nc", "rb") as f:
        magic = f.read(4)
"""

from __future__ import annotations

from pathlib import Path, PurePosixPath
from typing import TYPE_CHECKING, BinaryIO, Iterator
from contextlib import contextmanager

from loguru import logger

if TYPE_CHECKING:
    from upath import UPath


def _get_upath() -> type:
    """Import UPath lazily to avoid hard dependency."""
    try:
        from upath import UPath
        return UPath
    except ImportError:
        raise ImportError(
            "universal-pathlib is required for remote filesystem support.\n"
            "Install with: pip install universal-pathlib fsspec paramiko"
        )


def parse_uri(uri: str, **storage_options) -> "UPath":
    """Parse a URI string into a UPath object.

    Supports both explicit protocols (ssh://host/path) and local paths (/path).

    Args:
        uri: URI string like "ssh://albedo0/work/user/exp" or "/local/path"
        **storage_options: Protocol-specific options (e.g., username, key_filename)

    Returns:
        UPath object that can be used like pathlib.Path but works with any fsspec protocol.

    Examples:
        >>> path = parse_uri("ssh://albedo0/work/user/experiment")
        >>> path = parse_uri("/local/path/to/data")
        >>> path = parse_uri("s3://bucket/prefix", anon=True)
    """
    UPath = _get_upath()

    # Handle local paths without protocol
    if not _has_protocol(uri):
        # It's a local path - use regular Path for efficiency
        return UPath(Path(uri).resolve())

    # Parse the URI
    return UPath(uri, **storage_options)


def _has_protocol(uri: str) -> bool:
    """Check if a URI string has an explicit protocol."""
    # Common protocols we support
    protocols = ("ssh://", "sftp://", "scoutfs://", "s3://", "gs://",
                 "file://", "http://", "https://", "ftp://",
                 "simplecache://", "filecache://")
    return any(uri.startswith(p) for p in protocols)


def cache_remote_file(
    path: "UPath | Path",
    cache_dir: "Path",
    progress=None,
) -> Path:
    """Download a remote file to local cache and return the cached path.

    Uses content-based caching: files are stored as cache_dir/hostname/path/to/file.
    If already cached, returns existing path without re-downloading.

    Args:
        path: Remote UPath to cache
        cache_dir: Local directory for cached files
        progress: Optional rich.progress.Progress object for download progress

    Returns:
        Local Path to cached file
    """
    from pathlib import Path as LocalPath

    # Determine cache location based on remote path
    if hasattr(path, "fs") and hasattr(path.fs, "host"):
        host = path.fs.host or "localhost"
    else:
        host = "unknown"

    # Create cache path: cache_dir/host/path/to/file
    remote_path = path.path if hasattr(path, "path") else str(path)
    # Remove leading slash for path joining
    remote_path = remote_path.lstrip("/")
    cached_path = LocalPath(cache_dir) / host / remote_path

    # Check if already cached
    if cached_path.exists():
        logger.debug("Using cached: {}", cached_path.name)
        return cached_path

    filename = path.name if hasattr(path, "name") else str(path).split("/")[-1]
    cached_path.parent.mkdir(parents=True, exist_ok=True)

    # Simple streaming copy without progress bar (avoids threading issues)
    logger.info("Caching: {}", filename)
    chunk_size = 1024 * 1024  # 1MB chunks
    with path.open("rb") as src:
        with cached_path.open("wb") as dst:
            while chunk := src.read(chunk_size):
                dst.write(chunk)

    return cached_path


def to_uri(path: "UPath | Path") -> str:
    """Convert a UPath or Path to a URI string.

    Args:
        path: UPath or Path object

    Returns:
        URI string suitable for storage in STAC item href
    """
    if hasattr(path, "protocol") and path.protocol:
        # UPath with explicit protocol - need to reconstruct full URI with host
        # str(UPath) sometimes loses the hostname for SSH paths
        protocol = path.protocol

        # Try to get host from the path's filesystem
        if hasattr(path, "fs") and hasattr(path.fs, "host"):
            host = path.fs.host
            # path.path gives the path portion without protocol
            path_part = path.path if hasattr(path, "path") else str(path)
            if host:
                return f"{protocol}://{host}{path_part}"

        # Fallback to str() representation
        uri = str(path)
        # Fix malformed URIs like ssh:///path (missing host)
        if uri.startswith(f"{protocol}:///") and not uri.startswith(f"{protocol}://localhost"):
            # Can't recover host here, just return as-is
            pass
        return uri
    else:
        # Local path - return as file:// URI for consistency
        resolved = Path(path).resolve()
        return f"file://{resolved}"


def list_files(
    root: "UPath | Path | str",
    patterns: list[str] | None = None,
    **storage_options,
) -> Iterator["UPath"]:
    """Recursively list files matching patterns under root.

    Args:
        root: Root directory (UPath, Path, or URI string)
        patterns: Glob patterns to match (default: ["*.nc", "*.nc4", "*.grb", "*.grb2"])
        **storage_options: Protocol-specific options

    Yields:
        UPath objects for each matching file
    """
    UPath = _get_upath()

    if patterns is None:
        patterns = ["*.nc", "*.nc4", "*.grb", "*.grb2", "*.grib", "*.grib2"]

    # Convert to UPath if needed
    if isinstance(root, str):
        root = parse_uri(root, **storage_options)
    elif isinstance(root, Path) and not hasattr(root, "protocol"):
        root = UPath(root)

    logger.debug("Listing files in {} with patterns {}", root, patterns)

    seen: set[str] = set()

    for pattern in patterns:
        try:
            logger.debug("Searching for pattern: {}", pattern)
            for match in root.rglob(pattern):
                # Deduplicate by string path
                key = str(match)
                if key not in seen:
                    seen.add(key)
                    logger.debug("Found: {}", match.name)
                    yield match
        except Exception as e:
            logger.warning("Error listing {} with pattern {}: {}", root, pattern, e)


def list_all_files(
    root: "UPath | Path | str",
    skip_extensions: set[str] | None = None,
    **storage_options,
) -> Iterator["UPath"]:
    """Recursively list all files under root (for magic-byte detection).

    Args:
        root: Root directory
        skip_extensions: Extensions to skip (e.g., {".log", ".txt"})
        **storage_options: Protocol-specific options

    Yields:
        UPath objects for each file
    """
    UPath = _get_upath()

    if skip_extensions is None:
        skip_extensions = {".codes", ".txt", ".log", ".sh", ".py", ".yaml", ".yml", ".json"}

    if isinstance(root, str):
        root = parse_uri(root, **storage_options)
    elif isinstance(root, Path) and not hasattr(root, "protocol"):
        root = UPath(root)

    try:
        for match in root.rglob("*"):
            if match.is_file():
                suffix = match.suffix.lower()
                if suffix not in skip_extensions:
                    yield match
    except Exception as e:
        logger.warning("Error listing all files in {}: {}", root, e)


@contextmanager
def open_file(path: "UPath | Path | str", mode: str = "rb", **storage_options):
    """Open a file from any fsspec-compatible filesystem.

    Args:
        path: UPath, Path, or URI string
        mode: File mode (default: "rb" for binary read)
        **storage_options: Protocol-specific options

    Yields:
        File-like object

    Example:
        >>> with open_file("ssh://albedo0/work/file.nc", "rb") as f:
        ...     data = f.read()
    """
    UPath = _get_upath()

    if isinstance(path, str):
        path = parse_uri(path, **storage_options)
    elif isinstance(path, Path) and not hasattr(path, "protocol"):
        path = UPath(path)

    with path.open(mode) as f:
        yield f


def read_magic_bytes(path: "UPath | Path | str", n: int = 4, **storage_options) -> bytes:
    """Read the first n bytes from a file (for format detection).

    Args:
        path: UPath, Path, or URI string
        n: Number of bytes to read

    Returns:
        First n bytes of the file
    """
    with open_file(path, "rb", **storage_options) as f:
        return f.read(n)


def get_file_size(path: "UPath | Path | str", **storage_options) -> int:
    """Get the size of a file in bytes.

    Args:
        path: UPath, Path, or URI string

    Returns:
        File size in bytes
    """
    UPath = _get_upath()

    if isinstance(path, str):
        path = parse_uri(path, **storage_options)
    elif isinstance(path, Path) and not hasattr(path, "protocol"):
        path = UPath(path)

    return path.stat().st_size


def is_file(path: "UPath | Path | str", **storage_options) -> bool:
    """Check if path is a file."""
    UPath = _get_upath()

    if isinstance(path, str):
        path = parse_uri(path, **storage_options)
    elif isinstance(path, Path) and not hasattr(path, "protocol"):
        path = UPath(path)

    return path.is_file()


def is_dir(path: "UPath | Path | str", **storage_options) -> bool:
    """Check if path is a directory."""
    UPath = _get_upath()

    if isinstance(path, str):
        path = parse_uri(path, **storage_options)
    elif isinstance(path, Path) and not hasattr(path, "protocol"):
        path = UPath(path)

    return path.is_dir()


def get_protocol(path: "UPath | Path | str") -> str:
    """Get the protocol of a path (e.g., 'ssh', 'file', 's3').

    Args:
        path: UPath, Path, or URI string

    Returns:
        Protocol string (empty string for local paths)
    """
    if isinstance(path, str):
        for proto in ("ssh", "sftp", "scoutfs", "s3", "gs", "file", "http", "https", "ftp"):
            if path.startswith(f"{proto}://"):
                return proto
        return "file"

    if hasattr(path, "protocol"):
        return path.protocol or "file"

    return "file"
