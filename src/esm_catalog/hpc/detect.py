from __future__ import annotations
"""Detect HPC storage type and facility from a file path or filesystem."""

import re
from functools import lru_cache
from pathlib import Path

import esm_parser


# Map filesystem f_type magic numbers to storage type names
# (from linux/magic.h)
_FS_TYPE_NAMES = {
    0x65735546: "fuse",    # FUSE_SUPER_MAGIC
    0x01021994: "tmpfs",
    0xEF53: "ext4",
    0x6969: "nfs",
    0x5346544E: "ntfs",
    0x9123683E: "btrfs",
    0x58465342: "xfs",
    0x47504653: "gpfs",    # GPFS (IBM Spectrum Scale)
    0x0BD00BD0: "lustre",  # LUSTRE_SUPER_MAGIC
}

# storage: entry fields that get copied straight into the returned dict,
# prefixed with "hpc:".
_STORAGE_FIELDS = ("facility", "system", "storage_type", "state", "recall_time_estimate")


@lru_cache(maxsize=1)
def _load_storage_entries() -> dict:
    """Load all `storage:` entries from `configs/machines/*.yaml` and
    `configs/storage/*.yaml`.

    Returns a dict mapping entry name -> its fields (``path_str`` plus any of
    ``_STORAGE_FIELDS``), in the order the yaml files were read.
    """
    config_root = Path(esm_parser.CONFIG_PATH)
    entries = {}
    for subdir in ("machines", "storage"):
        for yaml_path in sorted((config_root / subdir).glob("*.yaml")):
            data = esm_parser.yaml_file_to_dict(str(yaml_path))
            entries.update(data.get("storage") or {})
    return entries


def detect_hpc_storage(path) -> dict:
    """Return HPC storage metadata for *path*.

    Tries path-pattern matching first (fast, no syscall), matching against
    `storage:` entries loaded from `configs/machines/*.yaml` and
    `configs/storage/*.yaml`.  Falls back to filesystem statvfs for
    unmatched paths.

    Works with both local Path and remote UPath objects.

    Returns a dict with a subset of these keys:
        hpc:facility, hpc:system, hpc:storage_type, hpc:state,
        hpc:recall_time_estimate
    """
    # Check if remote path (UPath with protocol)
    is_remote = hasattr(path, "protocol") and path.protocol and path.protocol != "file"

    # For remote paths, use the path attribute or string representation
    # For local paths, resolve to absolute
    if is_remote:
        path_str = path.path if hasattr(path, "path") else str(path)
    else:
        path_str = str(Path(path).resolve())

    for fields in _load_storage_entries().values():
        pattern = fields.get("path_str")
        if pattern and re.search(pattern, path_str):
            return {
                f"hpc:{field}": fields[field]
                for field in _STORAGE_FIELDS
                if field in fields
            }

    # Fallback: probe the actual filesystem (only for local paths)
    if is_remote:
        # Can't statvfs a remote path - return generic remote storage
        return {"hpc:storage_type": "remote", "hpc:state": "online"}

    return _detect_from_statvfs(path)


def _detect_from_statvfs(path: Path) -> dict:
    """Use os.statvfs to identify the filesystem type."""
    try:
        import ctypes
        buf = _statfs(path)
        fs_type = _FS_TYPE_NAMES.get(buf, "posix")
        return {"hpc:storage_type": fs_type, "hpc:state": "online"}
    except Exception:
        return {"hpc:storage_type": "posix", "hpc:state": "online"}


def _statfs(path: Path) -> int:
    """Return f_type from statfs(2).  Linux only.

    On non-Linux platforms (macOS, BSD), returns 0 to fall back to
    generic POSIX detection since statfs structure differs significantly.
    """
    import sys

    # Only Linux has f_type in statfs - macOS/BSD have different structures
    if sys.platform != "linux":
        return 0

    import ctypes
    import ctypes.util

    libc_name = ctypes.util.find_library("c")
    if not libc_name:
        return 0
    libc = ctypes.CDLL(libc_name, use_errno=True)

    # struct statfs layout (simplified — f_type is the first unsigned long)
    class _StatfsResult(ctypes.Structure):
        _fields_ = [
            ("f_type", ctypes.c_long),
            ("f_bsize", ctypes.c_long),
            ("f_blocks", ctypes.c_ulong),
            ("f_bfree", ctypes.c_ulong),
            ("f_bavail", ctypes.c_ulong),
            ("f_files", ctypes.c_ulong),
            ("f_ffree", ctypes.c_ulong),
            ("f_fsid", ctypes.c_long * 2),
            ("f_namelen", ctypes.c_long),
            ("f_frsize", ctypes.c_long),
            ("f_flags", ctypes.c_long),
            ("f_spare", ctypes.c_long * 4),
        ]

    result = _StatfsResult()
    ret = libc.statfs(str(path).encode(), ctypes.byref(result))
    if ret != 0:
        return 0
    return result.f_type
