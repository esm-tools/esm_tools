"""Detect HPC storage type and facility from a file path or filesystem."""

import os
from pathlib import Path


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


def detect_hpc_storage(path: Path) -> dict:
    """Return HPC storage metadata for *path*.

    Tries path-pattern matching first (fast, no syscall).  Falls back to
    filesystem statvfs for unknown paths.

    Returns a dict with a subset of these keys:
        hpc:facility, hpc:system, hpc:storage_type, hpc:state,
        hpc:recall_time_estimate
    """
    path_str = str(path.resolve())

    # AWI Albedo cluster (Lustre)
    if "/albedo/" in path_str:
        return {
            "hpc:facility": "AWI",
            "hpc:system": "albedo",
            "hpc:storage_type": "lustre",
            "hpc:state": "online",
        }

    # DKRZ Levante cluster (Lustre)
    if "/work/" in path_str and "/levante" not in path_str:
        # Heuristic: /work/ on DKRZ is Lustre, checked via hostname elsewhere
        return {
            "hpc:facility": "DKRZ",
            "hpc:system": "levante",
            "hpc:storage_type": "lustre",
            "hpc:state": "online",
        }

    # Tape/HPSS archive paths
    if "/arch/" in path_str or "/hpss/" in path_str:
        return {
            "hpc:storage_type": "hpss",
            "hpc:state": "offline",
            "hpc:recall_time_estimate": 300,
        }

    # Fallback: probe the actual filesystem
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
    """Return f_type from statfs(2).  Linux only."""
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
