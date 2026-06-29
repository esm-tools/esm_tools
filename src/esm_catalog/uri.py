from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from upath import UPath


def _get_upath() -> type:
    try:
        from upath import UPath
        return UPath
    except ImportError:
        raise ImportError("universal-pathlib is required: pip install universal-pathlib")


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
    if not _has_protocol(uri):
        return UPath(Path(uri).resolve())
    return UPath(uri, **storage_options)


def _has_protocol(uri: str) -> bool:
    """Check if a URI string has an explicit protocol."""
    protocols = ("ssh://", "sftp://", "scoutfs://", "s3://", "gs://",
                 "file://", "http://", "https://", "ftp://",
                 "simplecache://", "filecache://")
    return any(uri.startswith(p) for p in protocols)


def to_uri(path: "UPath | Path") -> str:
    """Convert a UPath or Path to a URI string.

    Args:
        path: UPath or Path object

    Returns:
        URI string suitable for storage in STAC item href
    """
    if hasattr(path, "protocol") and path.protocol:
        protocol = path.protocol
        if hasattr(path, "fs") and hasattr(path.fs, "host"):
            host = path.fs.host
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
    return f"file://{Path(path).resolve()}"
