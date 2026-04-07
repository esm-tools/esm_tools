"""HPC storage STAC extension: tape state, recall time, storage tier."""

from datetime import datetime, timezone
from pathlib import Path

from esm_catalog.hpc.detect import detect_hpc_storage
from esm_catalog.hpc.state import get_hsm_state
from esm_catalog.stac.extensions.registry import EXTENSION_URLS


def add_hpc_extension(item: dict, path, metadata: dict | None = None) -> dict:
    """Inject HPC storage extension fields into *item* for *path*.

    Item-level fields (item["properties"]):
        hpc:facility, hpc:system, hpc:storage_tier, hpc:last_access

    Asset-level fields (item["assets"]["data"]):
        hpc:storage_type, hpc:state, hpc:recall_time_estimate

    Works with both local Path and remote UPath objects.

    Args:
        metadata: If provided, uses pre-captured file_atime from the scan
                  worker to avoid a redundant stat() call on the main thread.
    """
    # Check if remote path (UPath with protocol)
    is_remote = hasattr(path, "protocol") and path.protocol and path.protocol != "file"

    # For local paths, convert to Path; for remote, keep as-is
    if not is_remote:
        path = Path(path)

    storage_info = detect_hpc_storage(path)

    # Item-level properties
    props = item["properties"]
    if "hpc:facility" in storage_info:
        props["hpc:facility"] = storage_info["hpc:facility"]
    if "hpc:system" in storage_info:
        props["hpc:system"] = storage_info["hpc:system"]
    if "hpc:storage_tier" in storage_info:
        props["hpc:storage_tier"] = storage_info["hpc:storage_tier"]
    else:
        # Derive tier from storage type
        storage_type = storage_info.get("hpc:storage_type", "posix")
        state = storage_info.get("hpc:state", "online")
        props["hpc:storage_tier"] = _derive_tier(storage_type, state)

    # Last access time -- prefer pre-captured stat from worker
    atime = (metadata or {}).get("file_atime")
    if atime is not None:
        props["hpc:last_access"] = datetime.fromtimestamp(
            atime, tz=timezone.utc
        ).isoformat()
    else:
        try:
            st = path.stat()
            props["hpc:last_access"] = datetime.fromtimestamp(
                st.st_atime, tz=timezone.utc
            ).isoformat()
        except (OSError, Exception):
            pass

    # Asset-level fields
    asset = item["assets"].get("data", {})
    asset["hpc:storage_type"] = storage_info.get("hpc:storage_type", "posix")

    # Only query HSM state if file appears to be on tape
    if storage_info.get("hpc:storage_type") in ("hpss", "dmf"):
        asset["hpc:state"] = get_hsm_state(path)
    else:
        asset["hpc:state"] = storage_info.get("hpc:state", "online")

    if "hpc:recall_time_estimate" in storage_info:
        asset["hpc:recall_time_estimate"] = storage_info["hpc:recall_time_estimate"]

    item["assets"]["data"] = asset

    url = EXTENSION_URLS["hpc"]
    if url not in item["stac_extensions"]:
        item["stac_extensions"].append(url)

    return item


def _derive_tier(storage_type: str, state: str) -> str:
    if storage_type in ("hpss", "dmf") or state in ("offline", "nearline"):
        return "cold"
    if storage_type in ("gpfs",):
        return "warm"
    return "hot"
