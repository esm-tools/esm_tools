"""Resolve collection membership before STAC Item creation.

This module is the solution to the design hole described in ARCHITECTURE.md:
the DuckDB schema requires a non-NULL `collection` value at insert time, but
the original scan → item → insert flow had no step that resolved this value.

`resolve_context()` must be called BEFORE make_item(). It returns a
CollectionContext dataclass that carries experiment_id, component, and the
derived collection_id. If context cannot be resolved, it raises
CollectionContextError rather than returning a NULL-collection context — a
silent NULL is worse than a failed insert because it produces a catalog that
appears to work but cannot be navigated via STAC Browser's collection tree.

Restart File Handling
---------------------
Restart files are filtered out by default. They are detected by:

1. **Directory-based**: Files in `/restart/` directories
2. **Filename patterns**: Files matching restart naming conventions:
   - `*_restart_*` (FESOM restart files)
   - `*.restart.*` (generic restart format)
   - `rerun_*` (ECHAM rerun files)
   - `*_rerun_*` (ECHAM rerun files)
   - `restart_*` (general restart prefix)

When a restart file is detected, `resolve_context()` raises `RestartFileError`
which the caller should catch and skip. This keeps the main output collections
clean while allowing explicit restart cataloging if needed.
"""

from __future__ import annotations

import re
from pathlib import Path

from loguru import logger

from esm_catalog.context import CollectionContext


class CollectionContextError(ValueError):
    """Raised when collection context cannot be resolved for a path.

    This is a normal outcome for files outside the outdata directory tree
    (work/, restart/, input/, config/, log/ etc.) and should be logged at
    DEBUG level, not ERROR.
    """


# Restart filename patterns - compiled for performance
_RESTART_PATTERNS = [
    re.compile(r".*_restart_.*", re.IGNORECASE),     # FESOM: fesom_restart_oce.nc
    re.compile(r".*\.restart\.", re.IGNORECASE),     # generic: model.restart.nc
    re.compile(r"^rerun_.*", re.IGNORECASE),         # ECHAM: rerun_basic-001_185001
    re.compile(r".*_rerun_.*", re.IGNORECASE),       # ECHAM variant: basic-001_rerun_185001
    re.compile(r"^restart_.*", re.IGNORECASE),       # generic prefix: restart_fesom.nc
]


class RestartFileError(ValueError):
    """Raised when a file is identified as a restart file.

    Restart files should be filtered out from the main catalog to keep
    output collections clean. Callers should catch this exception and
    skip the file.
    """
    pass


def is_restart_file(path) -> bool:
    """Check if a file is a restart file based on path and filename patterns.

    Detection methods:
    1. Directory-based: File is under a /restart/ directory
    2. Filename-based: Filename matches restart patterns

    Args:
        path: Path-like object (Path, PurePosixPath, or UPath)

    Returns:
        True if the file appears to be a restart file
    """
    # Get path parts for directory check
    if hasattr(path, "resolve"):
        parts = path.resolve().parts
    else:
        parts = path.parts

    # Check if any parent directory is named "restart"
    for part in parts[:-1]:  # Exclude the filename itself
        if part.lower() == "restart":
            return True

    # Check filename against restart patterns
    filename = parts[-1] if parts else ""
    for pattern in _RESTART_PATTERNS:
        if pattern.match(filename):
            return True

    return False


def resolve_context(
    path,
    config: dict | None = None,
    allow_restart: bool = False,
) -> CollectionContext:
    """Resolve collection membership for *path*.

    Two strategies, tried in order:

    1. **ESM-Tools config** (preferred — used during live tidy phase):
       experiment_id from config["general"]["expid"]
       component    from the component block whose outdata_dir contains *path*

    2. **Path parsing** (fallback — used during batch scan of legacy runs):
       Looks for ".../experiments/{experiment}/outdata/{component}/..." pattern

    Args:
        path: File path to resolve context for
        config: Optional ESM-Tools config dict
        allow_restart: If False (default), raise RestartFileError for restart files.
                       If True, allow restart files to be cataloged.

    Raises:
        RestartFileError: if the file is a restart file and allow_restart=False.
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

    # Check for restart files BEFORE attempting context resolution
    # This prevents restart files from polluting output collections
    if not allow_restart and is_restart_file(path_for_parsing):
        raise RestartFileError(
            f"Restart file detected: {path}\n"
            "Restart files are filtered out by default to keep output collections clean.\n"
            "Use allow_restart=True to include restart files in the catalog."
        )

    ctx = _from_config(path_for_parsing, config) or _from_path(path_for_parsing)

    if ctx is None:
        raise CollectionContextError(
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
        .../experiments/{experiment}/restart/{component}/file.nc

    Works with both Path (local) and PurePosixPath (remote).
    """
    # For PurePosixPath (remote), we can't resolve, so use as-is
    if hasattr(path, "resolve"):
        parts = path.resolve().parts
    else:
        parts = path.parts

    # Try multiple directory markers in order of preference
    # outdata is preferred (actual output), restart is secondary
    for marker in ("outdata", "restart"):
        try:
            marker_idx = _rindex(parts, marker)
        except ValueError:
            continue

        if marker_idx < 1 or marker_idx + 1 >= len(parts):
            continue

        component = parts[marker_idx + 1]

        # Experiment: prefer the "experiments/{experiment}" convention
        exp_idx = _rindex_before(parts, "experiments", marker_idx)
        if exp_idx is not None and exp_idx + 1 < marker_idx:
            experiment_id = parts[exp_idx + 1]
            # Experiment path is everything up to and including the experiment name
            experiment_path = Path(*parts[: exp_idx + 2])
        else:
            # Fallback: use the parent directory of the marker
            experiment_id = parts[marker_idx - 1]
            # Experiment path is the directory containing the marker
            experiment_path = Path(*parts[:marker_idx])

        return _make_ctx(experiment_id, component, experiment_path)

    return None


# ------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------

def _make_ctx(
    experiment_id: str,
    component: str,
    experiment_path: Path | None = None,
) -> CollectionContext:
    # Option A: one collection per experiment — collection_id == experiment_id.
    # The component is preserved on every item as properties.component and is
    # used as a facet filter rather than as a collection discriminator.
    collection_id = experiment_id
    collection_title = experiment_id
    return CollectionContext(
        experiment_id=experiment_id,
        component=component,
        collection_id=collection_id,
        collection_title=collection_title,
        experiment_path=experiment_path,
    )


def scan_all_namelists(experiment_path) -> dict:
    """Scan every component's config dir under experiment_path/config.

    Returns {component_name: {filename: {group: {key: value}}}}, suitable for
    CollectionContext.namelists_by_component (read by the STAC item extension).
    """
    from esm_catalog.scan.namelist import scan_namelist_directory

    by_component: dict = {}
    if experiment_path is None:
        return by_component
    config_root = Path(experiment_path) / "config"
    if not config_root.is_dir():
        return by_component
    for comp_dir in config_root.iterdir():
        if not comp_dir.is_dir():
            continue
        nls = scan_namelist_directory(comp_dir, comp_dir.name)
        if nls:
            by_component[comp_dir.name] = nls
    return by_component


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
