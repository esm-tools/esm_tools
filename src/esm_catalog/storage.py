"""Storage STAC extension: facility, system, storage tier, last access.

Expects a *machine_config* dict — the parsed ESM-Tools machine section for
whatever machine the scan is running on — with (any of) these keys:
    facility, system, storage_type, storage_tier

storage_tier, if given, is used as-is instead of derived from storage_type
via the hot/warm/cold heuristic in _derive_tier — that heuristic only
knows a handful of storage_type values, and defaults to leaving the tier
unset for anything it doesn't recognize.

Detection is not done by probing the filesystem or matching path patterns:
the caller already knows which machine it is running on, so it resolves and
passes in that machine's own config section directly. The one exception is
storage:last_access, which requires an explicit stat() call on *path* and is
only attempted when the caller opts in via probe_last_access.

Fields storage:state and storage:recall_time_estimate (subprocess-based HSM
state queries via dmattr/lfs) were dropped: they require machine-specific
tools and add operational risk for marginal query value.
"""

from __future__ import annotations

from datetime import datetime, timezone
from pathlib import Path
from typing import TYPE_CHECKING, Optional, Union

from pystac.utils import datetime_to_str

from esm_catalog.registry import EXTENSION_URLS

if TYPE_CHECKING:
    import pystac
    from upath import UPath

# Coarse access-latency tier per storage_type. Anything not listed here is
# unknown, not "hot" — a site whose storage_type we don't recognize should
# not be silently assumed fast. Sites with a type outside this list, or a
# type where the default doesn't hold, can set machine_config["storage_tier"]
# explicitly to bypass this heuristic entirely.
_TIER_BY_STORAGE_TYPE = {
    "hpss": "cold",
    "dmf": "cold",
    "tape": "cold",
    "gpfs": "warm",
    "lustre": "hot",
}


def add_storage_extension(
    item: "pystac.Item",
    path: Union[Path, "UPath"],
    machine_config: Optional[dict] = None,
    probe_last_access: bool = False,
) -> None:
    """Inject storage extension fields into *item* for *path*.

    Item-level fields (item.properties): storage:facility, storage:system,
    storage:tier, storage:last_access.
    Asset-level fields (item.assets["data"].extra_fields): storage:type.

    *machine_config* is the parsed ESM-Tools machine section for the machine
    the scan is running on.

    *probe_last_access* opts into calling path.stat() to populate
    storage:last_access. Defaults to False: for remote or tape-backed
    paths a stat() call can be slow or trigger a recall, so this extension
    never touches the filesystem unless the caller explicitly asks for it.
    """
    populated = False

    if machine_config:
        facility = machine_config.get("facility")
        system = machine_config.get("system")
        storage_type = machine_config.get("storage_type")
        tier = machine_config.get("storage_tier") or _derive_tier(storage_type)

        if facility:
            item.properties["storage:facility"] = facility
            populated = True
        if system:
            item.properties["storage:system"] = system
            populated = True
        if storage_type:
            if "data" in item.assets:
                item.assets["data"].extra_fields["storage:type"] = storage_type
            populated = True
        if tier:
            item.properties["storage:tier"] = tier
            populated = True

    if probe_last_access:
        last_access = _get_last_access(path)
        if last_access is not None:
            item.properties["storage:last_access"] = datetime_to_str(last_access)
            populated = True

    if populated:
        url = EXTENSION_URLS["storage"]
        if url not in item.stac_extensions:
            item.stac_extensions.append(url)


def _get_last_access(path) -> Optional[datetime]:
    """Return path's last-access time as a UTC datetime, or None."""
    try:
        stat = path.stat()
    except Exception:
        return None
    return datetime.fromtimestamp(stat.st_atime, tz=timezone.utc)


def _derive_tier(storage_type: Optional[str]) -> Optional[str]:
    """Map a known filesystem/storage type to a coarse hot/warm/cold tier.

    Returns None for an unrecognized (or missing) storage_type rather than
    guessing — callers who know better can pass machine_config["storage_tier"]
    directly.
    """
    if not storage_type:
        return None
    return _TIER_BY_STORAGE_TYPE.get(storage_type)
