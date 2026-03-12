"""Resolve collection membership before STAC Item creation.

This module is the solution to the design hole described in ARCHITECTURE.md:
the DuckDB schema requires a non-NULL `collection` value at insert time, but
the original scan → item → insert flow had no step that resolved this value.

`resolve_context()` must be called BEFORE make_item(). It returns a
CollectionContext dataclass that carries experiment_id, component, and the
derived collection_id. If context cannot be resolved, it raises ValueError
rather than returning a NULL-collection context — a silent NULL is worse than
a failed insert because it produces a catalog that appears to work but cannot
be navigated via STAC Browser's collection tree.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

from loguru import logger


@dataclass
class CollectionContext:
    """Resolved collection membership for a single file."""

    experiment_id: str
    component: str
    collection_id: str       # "{experiment_id}-{component}"
    collection_title: str
    experiment_path: Path | None = None  # Path to experiment root (for namelist scanning)


def resolve_context(
    path,
    config: dict | None = None,
    db=None,
) -> CollectionContext:
    """Resolve collection membership for *path*.

    Two strategies, tried in order:

    1. **ESM-Tools config** (preferred — used during live tidy phase):
       experiment_id from config["general"]["expid"]
       component    from the component block whose outdata_dir contains *path*

    2. **Path parsing** (fallback — used during batch scan of legacy runs):
       Looks for ".../experiments/{experiment}/outdata/{component}/..." pattern

    If db is provided and the resolved collection does not yet exist in the
    database, the collection is created and inserted atomically here.

    Raises:
        ValueError: if neither strategy can resolve the context.
    """
    # For remote UPath objects, we need to work with the path string
    # Path(upath) fails with "expected str, bytes or os.PathLike object, not SFTPPath"
    if hasattr(path, "protocol") and path.protocol and path.protocol != "file":
        # Remote path - use PurePosixPath for parsing (no filesystem access)
        from pathlib import PurePosixPath
        path_for_parsing = PurePosixPath(path.path if hasattr(path, "path") else str(path))
    else:
        path_for_parsing = Path(path)

    ctx = _from_config(path_for_parsing, config) or _from_path(path_for_parsing)

    if ctx is None:
        raise ValueError(
            f"Cannot resolve collection context for: {path}\n"
            "Expected one of:\n"
            "  - ESM-Tools config with general.expid and component outdata_dir\n"
            "  - Path matching: .../experiments/{experiment}/outdata/{component}/..."
        )

    logger.debug(
        "Resolved context: experiment={} component={} collection={}",
        ctx.experiment_id,
        ctx.component,
        ctx.collection_id,
    )

    if db is not None:
        _ensure_collection(ctx, db)

    return ctx


# ------------------------------------------------------------------
# Strategy 1: ESM-Tools config
# ------------------------------------------------------------------

def _from_config(path: Path, config: dict | None) -> CollectionContext | None:
    if not config:
        return None

    general = config.get("general", {})
    experiment_id = general.get("expid")
    if not experiment_id:
        return None

    component = _find_component_for_path(path, config)
    if not component:
        return None

    # Try to find experiment path from config
    experiment_path = None
    base_dir = general.get("base_dir") or general.get("experiment_dir")
    if base_dir:
        experiment_path = Path(base_dir)

    return _make_ctx(experiment_id, component, experiment_path)


def _find_component_for_path(path, config: dict) -> str | None:
    """Return the component name whose outdata_dir is an ancestor of *path*."""
    # For PurePosixPath (remote), we can't resolve, so use as-is
    # For Path (local), resolve to absolute
    if hasattr(path, "resolve"):
        path_str = str(path.resolve())
    else:
        path_str = str(path)

    # ESM-Tools config has top-level keys for each component (fesom, echam, ...)
    skip_keys = {"general", "computer", "setup", "env", "defaults"}
    for key, block in config.items():
        if key in skip_keys or not isinstance(block, dict):
            continue
        outdata = (block.get("outdata_dir") or
                   block.get("experiment_outdata_dir") or
                   block.get("thisrun_outdata_dir"))
        if not outdata:
            continue
        try:
            # String-based check: is the path under the outdata directory?
            outdata_str = str(Path(outdata).resolve()) if hasattr(path, "resolve") else outdata
            if path_str.startswith(outdata_str):
                return key
        except (ValueError, TypeError):
            pass
    return None


# ------------------------------------------------------------------
# Strategy 2: Path parsing
# ------------------------------------------------------------------

def _from_path(path) -> CollectionContext | None:
    """Parse collection context from a path following ESM-Tools conventions.

    Expected patterns:
        .../experiments/{experiment}/outdata/{component}/file.nc
        .../experiments/{experiment}/outdata/{component}/subdir/file.nc

    Works with both Path (local) and PurePosixPath (remote).
    """
    # For PurePosixPath (remote), we can't resolve, so use as-is
    if hasattr(path, "resolve"):
        parts = path.resolve().parts
    else:
        parts = path.parts

    # Find "outdata" segment; experiment is one directory above it
    try:
        outdata_idx = _rindex(parts, "outdata")
    except ValueError:
        return None

    if outdata_idx < 1 or outdata_idx + 1 >= len(parts):
        return None

    component = parts[outdata_idx + 1]

    # Experiment: prefer the "experiments/{experiment}" convention
    exp_idx = _rindex_before(parts, "experiments", outdata_idx)
    if exp_idx is not None and exp_idx + 1 < outdata_idx:
        experiment_id = parts[exp_idx + 1]
        # Experiment path is everything up to and including the experiment name
        experiment_path = Path(*parts[: exp_idx + 2])
    else:
        # Fallback: use the parent directory of "outdata"
        experiment_id = parts[outdata_idx - 1]
        # Experiment path is the directory containing "outdata"
        experiment_path = Path(*parts[:outdata_idx])

    return _make_ctx(experiment_id, component, experiment_path)


# ------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------

def _make_ctx(
    experiment_id: str,
    component: str,
    experiment_path: Path | None = None,
) -> CollectionContext:
    collection_id = f"{experiment_id}-{component}"
    collection_title = f"{experiment_id} / {component}"
    return CollectionContext(
        experiment_id=experiment_id,
        component=component,
        collection_id=collection_id,
        collection_title=collection_title,
        experiment_path=experiment_path,
    )


def _ensure_collection(ctx: CollectionContext, db) -> None:
    """Create and insert the collection if it does not exist yet."""
    from esm_catalog.stac.collection import make_collection

    if db.collection_exists(ctx.collection_id):
        return

    collection = make_collection(ctx, experiment_path=ctx.experiment_path)
    db.insert_collection(collection)
    logger.info("Created collection: {}", ctx.collection_id)


def _rindex(seq: tuple, value: str) -> int:
    """Return the last index of *value* in *seq*."""
    for i in range(len(seq) - 1, -1, -1):
        if seq[i] == value:
            return i
    raise ValueError(f"{value!r} not in sequence")


def _rindex_before(seq: tuple, value: str, before: int) -> int | None:
    """Return the last index of *value* in seq[:before], or None."""
    for i in range(before - 1, -1, -1):
        if seq[i] == value:
            return i
    return None
