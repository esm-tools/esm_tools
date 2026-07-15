"""HPC storage STAC extension: facility, system, storage tier, last access.

Expects a *machine_config* dict — the parsed ESM-Tools machine section for
whatever machine the scan is running on — with (any of) these keys:
    facility, system, storage_type

Detection is not done by probing the filesystem or matching path patterns:
the caller already knows which machine it is running on, so it resolves and
passes in that machine's own config section directly.

Fields hpc:state and hpc:recall_time_estimate (subprocess-based HSM state
queries via dmattr/lfs) were dropped: they require machine-specific tools and
add operational risk for marginal query value.
"""

from __future__ import annotations

from datetime import datetime, timezone
from pathlib import Path
from typing import TYPE_CHECKING, Optional, Union

from esm_catalog.registry import EXTENSION_URLS

if TYPE_CHECKING:
    import pystac
    from upath import UPath


def add_hpc_extension(
    item: "pystac.Item",
    path: Union[Path, "UPath"],
    machine_config: Optional[dict] = None,
) -> None:
    """Inject HPC storage extension fields into *item* for *path*.

    Item-level fields (item.properties): hpc:facility, hpc:system,
    hpc:storage_tier, hpc:last_access.
    Asset-level fields (item.assets["data"].extra_fields): hpc:storage_type.

    *machine_config* is the parsed ESM-Tools machine section for the machine
    the scan is running on. If None, only hpc:last_access is populated
    (graceful degradation for standalone/test usage).
    """
    populated = False

    if machine_config:
        facility = machine_config.get("facility")
        system = machine_config.get("system")
        storage_type = machine_config.get("storage_type")

        if facility:
            item.properties["hpc:facility"] = facility
            populated = True
        if system:
            item.properties["hpc:system"] = system
            populated = True
        if storage_type:
            item.properties["hpc:storage_tier"] = _derive_tier(storage_type)
            if "data" in item.assets:
                item.assets["data"].extra_fields["hpc:storage_type"] = storage_type
            populated = True

    last_access = _get_last_access(path)
    if last_access is not None:
        item.properties["hpc:last_access"] = last_access
        populated = True

    if populated:
        url = EXTENSION_URLS["hpc"]
        if url not in item.stac_extensions:
            item.stac_extensions.append(url)


def _get_last_access(path) -> Optional[str]:
    """Return path's last-access time as an ISO 8601 UTC string, or None."""
    try:
        stat = path.stat()
    except Exception:
        return None
    return datetime.fromtimestamp(stat.st_atime, tz=timezone.utc).isoformat()


def _derive_tier(storage_type: str) -> str:
    """Map a raw filesystem/storage type to a coarse hot/warm/cold tier."""
    if storage_type in ("hpss", "dmf", "tape"):
        return "cold"
    if storage_type == "gpfs":
        return "warm"
    return "hot"
