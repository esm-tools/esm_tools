from __future__ import annotations
"""HSM (Hierarchical Storage Management) state queries with rate limiting."""

import subprocess
import time
from pathlib import Path
from threading import Lock

from loguru import logger


# Simple token-bucket rate limiter: max 10 HSM queries per second
_RATE_LIMIT_INTERVAL = 0.1  # seconds between queries
_rate_lock = Lock()
_last_query_time: float = 0.0


def _throttle():
    """Block if we are querying faster than the rate limit."""
    global _last_query_time
    with _rate_lock:
        now = time.monotonic()
        elapsed = now - _last_query_time
        if elapsed < _RATE_LIMIT_INTERVAL:
            time.sleep(_RATE_LIMIT_INTERVAL - elapsed)
        _last_query_time = time.monotonic()


def get_hsm_state(path: Path) -> str:
    """Return the HSM state of *path*.

    Returns one of: "online", "nearline", "offline", "migrating", "unknown"

    Tries dmattr (GPFS/Spectrum Scale) first, then scoutfs inode flags.
    Falls back to "online" if neither tool is available.
    """
    _throttle()

    state = _dmattr_state(path)
    if state != "unknown":
        return state

    state = _scoutfs_state(path)
    if state != "unknown":
        return state

    logger.debug("No HSM tool available for {}; assuming online", path)
    return "online"


def _dmattr_state(path: Path) -> str:
    """Query dmattr (IBM Spectrum Scale / GPFS DMF)."""
    try:
        result = subprocess.run(
            ["dmattr", "-l", str(path)],
            capture_output=True,
            text=True,
            timeout=10,
        )
        if result.returncode != 0:
            return "unknown"
        output = result.stdout.lower()
        # dmattr output contains "state:" field
        if "state: reg" in output or "state: dls" in output:
            return "online"
        if "state: mig" in output:
            return "migrating"
        if "state: dul" in output or "state: ofl" in output:
            return "nearline"
        if "state: offl" in output:
            return "offline"
        return "online"
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return "unknown"


def _scoutfs_state(path: Path) -> str:
    """Query scoutfs inode staging flags (DDN Lustre HSM variant)."""
    try:
        result = subprocess.run(
            ["lfs", "hsm_state", str(path)],
            capture_output=True,
            text=True,
            timeout=10,
        )
        if result.returncode != 0:
            return "unknown"
        output = result.stdout.lower()
        if "released" in output:
            return "nearline"
        if "archived" in output and "exists" not in output:
            return "nearline"
        return "online"
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return "unknown"
