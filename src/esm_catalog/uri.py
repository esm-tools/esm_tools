"""Neutral URI helpers shared by stac/ and scan/ (no internal esm_catalog deps).

parse_uri / to_uri were previously in esm_catalog.scan.upath; they are pure
path<->URI converters with no scanning responsibility, so they live here to
keep the STAC model free of any dependency on the scan layer.
"""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING

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
