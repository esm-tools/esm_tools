from __future__ import annotations
"""Load and parse ESM-Tools finished_config.yaml."""

from pathlib import Path


def load_config(path: Path | str | None) -> dict | None:
    """Load an ESM-Tools YAML config file.

    Uses ruamel.yaml to preserve comments (important for downstream round-trip
    use by ESM-Tools itself).  Returns None if path is None.

    Accepts both `.yaml` files and the date-range-suffixed files that
    ESM-Tools writes (e.g. ``basic-001_finished_config.yaml_18500101-18500131``).
    These have no `.yaml` extension but are still valid YAML.
    """
    if path is None:
        return None
    from ruamel.yaml import YAML
    yaml = YAML()
    return yaml.load(Path(path))


def find_finished_configs(experiment_dir: Path | str) -> list[Path]:
    """Find all finished_config files for an experiment, sorted by date range.

    ESM-Tools writes one finished_config per run period, named::

        {expid}_finished_config.yaml_{YYYYMMDD}-{YYYYMMDD}

    and updates a symlink ``{expid}_finished_config.yaml`` to the latest one.
    This function returns the concrete per-run files (not the symlink), sorted
    chronologically by their date-range suffix.

    Args:
        experiment_dir: Root experiment directory (contains a ``config/``
            subdirectory) **or** the config directory itself.

    Returns:
        List of matching Paths, sorted by name (which is chronological for
        zero-padded ISO date ranges).  Empty list if none are found.
    """
    experiment_dir = Path(experiment_dir)
    config_dir = experiment_dir / "config"
    if not config_dir.is_dir():
        config_dir = experiment_dir  # allow passing config dir directly

    candidates = sorted(config_dir.glob("*_finished_config.yaml_*"))
    return [p for p in candidates if p.is_file() and "_finished_config.yaml_" in p.name]


def get_outdata_files(config: dict, component: str) -> list[Path]:
    """Return the output files for *component* from a finished_config.

    ESM-Tools records the absolute target paths of every output file in
    ``config[component]["outdata_targets"]``, a dict that maps stream names to
    absolute paths.

    Args:
        config:    Dict loaded by :func:`load_config`.
        component: Component name (e.g. ``"echam"``, ``"fesom"``).

    Returns:
        List of :class:`~pathlib.Path` objects for files that exist; empty list
        if the component or ``outdata_targets`` key is absent.
    """
    comp_block = config.get(component, {})
    if not isinstance(comp_block, dict):
        return []
    targets = comp_block.get("outdata_targets") or {}
    return [Path(p) for p in targets.values() if p]


def find_file_operations_log(
    experiment_dir: Path | str,
    component: str,
    run_datestamp: str,
) -> Path | None:
    """Find the file_operations_tidy YAML for one component run.

    ESM-Tools writes one log per component per run period::

        {log_dir}/{expid}_{component}_file_operations_tidy_{run_datestamp}.yaml

    Args:
        experiment_dir: Root experiment directory.
        component:      Component name, e.g. ``"fesom"``.
        run_datestamp:  Date range string, e.g. ``"19580101-19580131"``.

    Returns:
        Path to the file if it exists, None otherwise.
    """
    experiment_dir = Path(experiment_dir)
    log_dir = experiment_dir / "log"
    if not log_dir.is_dir():
        return None

    expid = experiment_dir.name
    candidate = log_dir / f"{expid}_{component}_file_operations_tidy_{run_datestamp}.yaml"
    if candidate.is_file():
        return candidate

    matches = sorted(log_dir.glob(f"*_{component}_file_operations_tidy_{run_datestamp}.yaml"))
    return matches[0] if matches else None


def get_outdata_from_file_operations(path: Path | str) -> list[dict]:
    """Extract outdata entries from a file_operations_tidy YAML.

    ESM-Tools writes this log during the tidy phase.  It records every file
    moved, copied, or symlinked and includes MD5 checksums, making it the
    preferred source for catalog construction — checksums come for free.

    Only the ``outdata`` category is returned; ``log``, ``restart_out``, and
    ``unknown`` entries are intentionally excluded.

    Args:
        path: Path to the ``*_file_operations_tidy_*.yaml`` file.

    Returns:
        List of dicts, one per output file::

            {
              "destination": Path,        # canonical path after tidy
              "source":      Path | None, # original path in run work dir
              "checksum":    str | None,  # MD5 hex string
              "tidy_op":     str | None,  # "copy", "move", or "link"
            }
    """
    cfg = load_config(path)
    if cfg is None:
        return []

    records: list[dict] = []
    for component_block in cfg.values():
        if not isinstance(component_block, dict):
            continue
        files_block = component_block.get("files") or {}
        outdata = files_block.get("outdata") or {}
        for _filename, entry in outdata.items():
            if not isinstance(entry, dict):
                continue
            dst = entry.get("destination")
            if not dst:
                continue
            src = entry.get("source")
            checksum = entry.get("checksum")
            records.append({
                "destination": Path(str(dst).strip()),
                "source": Path(str(src).strip()) if src else None,
                "checksum": str(checksum).strip() if checksum else None,
                "tidy_op": str(entry.get("tidy_op", "")).strip() or None,
            })
    return records


def find_vcs_info(experiment_dir: Path | str) -> Path | None:
    """Find the ``{expid}_vcs_info.yaml`` file for an experiment, if present.

    ESM-Tools writes this file during the prepare step (see
    ``esm_runscripts.prepare``), recording per-model git info (commit hash,
    branch name, model directory, uncommitted diff) plus esm_tools' own repo
    info. Unlike ``finished_config``, it is not date-range-suffixed — it gets
    overwritten each run and compared against the previous run's copy.

    Args:
        experiment_dir: Root experiment directory (contains a ``log/``
            subdirectory) **or** the log directory itself.

    Returns:
        Path to the file if found, None otherwise.
    """
    experiment_dir = Path(experiment_dir)
    log_dir = experiment_dir / "log"
    if not log_dir.is_dir():
        log_dir = experiment_dir  # allow passing log dir directly

    expid = experiment_dir.name
    candidate = log_dir / f"{expid}_vcs_info.yaml"
    if candidate.is_file():
        return candidate

    # Glob fallback in case expid differs from directory name
    matches = sorted(log_dir.glob("*_vcs_info.yaml"))
    return matches[0] if matches else None


def load_vcs_info(path: Path | str) -> dict:
    """Load a ``{expid}_vcs_info.yaml`` file.

    Returns a dict keyed by model name (plus ``"esm_tools"`` for the
    esm-tools repo itself). Each value is either a dict with keys ``path``,
    ``hash``, ``branch_name``, ``diffs`` (for git-controlled models), or a
    plain string explaining why no git info is available (e.g. "Not a
    git-controlled model!").

    Args:
        path: Path to the ``*_vcs_info.yaml`` file.

    Returns:
        Dict as loaded by :func:`load_config`, or ``{}`` if *path* is None.
    """
    return load_config(path) or {}


def _parse_prev_run_config_file(path_str: str) -> dict:
    """Derive parent-simulation lineage info from a ``prev_run_config_file`` path.

    ESM-Tools names finished_config files
    ``{expid}_finished_config.yaml_{start}-{end}``, so the parent's expid and
    branch-off year can be recovered from the path alone, without opening it.

    Args:
        path_str: Value of ``config[component]["prev_run_config_file"]``.

    Returns:
        Dict with ``parent_expid``, ``parent_path``, ``branch_off_year``
        (``None`` for any piece that can't be parsed). Empty dict if
        *path_str* doesn't match the expected naming convention.
    """
    marker = "_finished_config.yaml_"
    p = Path(path_str)
    name = p.name
    if marker not in name:
        return {}

    parent_expid, _, daterange = name.partition(marker)
    start = daterange.split("-")[0] if daterange else ""
    branch_off_year = int(start[:4]) if start[:4].isdigit() else None

    return {
        "parent_expid": parent_expid or None,
        "parent_path": str(p.parent),
        "branch_off_year": branch_off_year,
    }


def extract_stac_metadata(config: dict, vcs_info: dict | None = None) -> dict:
    """Extract STAC-relevant metadata from a finished_config.

    Args:
        config: Dict loaded by :func:`load_config`.
        vcs_info: Optional dict loaded by :func:`load_vcs_info`. When given,
            each component's git ``hash``, ``branch_name``, and model
            ``path`` (source/binary directory) are merged in under the same
            keys, for components that have a corresponding entry.

    Returns:
        Dict with keys: ``expid``, ``scenario``, ``resolution``,
        ``setup_name``, ``setup_version``, ``run_datestamp``, ``lresume``,
        ``components`` (nested per-component metadata dict).
    """
    general = config.get("general", {})
    skip_keys = {"general", "computer", "setup", "env", "defaults", "recom"}
    vcs_info = vcs_info or {}

    components: dict[str, dict] = {}
    for key, block in config.items():
        if key in skip_keys or not isinstance(block, dict):
            continue
        meta = block.get("metadata") or {}
        if not isinstance(meta, dict) or not meta:
            continue
        component = {
            "version": block.get("version"),
            "institute": meta.get("Institute"),
            "authors": meta.get("Authors"),
            "description": meta.get("Description"),
            "publications": meta.get("Publications"),
        }
        model_vcs = vcs_info.get(key)
        if isinstance(model_vcs, dict):
            component["hash"] = model_vcs.get("hash")
            component["branch_name"] = model_vcs.get("branch_name")
            component["path"] = model_vcs.get("path")

        component["is_cold_start"] = not bool(block.get("lresume", False))
        prev_run_config_file = block.get("prev_run_config_file")
        if prev_run_config_file:
            component.update(_parse_prev_run_config_file(str(prev_run_config_file)))
        restart_in = block.get("restart_in_sources") or block.get("restart_in_targets")
        if isinstance(restart_in, dict) and restart_in:
            component["restart_files"] = list(restart_in.values())

        components[key] = component

    return {
        "expid": general.get("expid"),
        "scenario": general.get("scenario"),
        "resolution": general.get("resolution"),
        "setup_name": general.get("setup_name"),
        "setup_version": general.get("version"),
        "run_datestamp": general.get("run_datestamp"),
        "lresume": general.get("lresume"),
        "components": components,
    }
