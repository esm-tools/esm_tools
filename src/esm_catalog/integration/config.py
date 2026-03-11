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

    # Search for any *_finished_config.yaml_* file in the config directory.
    # The date-range suffix makes these files unique; the plain symlink
    # (no date suffix) is excluded by the is_file() check (symlinks resolve)
    # and the "_finished_config.yaml_" substring check.
    candidates = sorted(config_dir.glob("*_finished_config.yaml_*"))
    return [p for p in candidates if p.is_file() and "_finished_config.yaml_" in p.name]


def get_outdata_files(config: dict, component: str) -> list[Path]:
    """Return the output files for *component* from a finished_config.

    ESM-Tools records the absolute target paths of every output file in
    ``config[component]["outdata_targets"]``, a dict that maps stream names to
    absolute paths (e.g. ``{"echam_nc": "/work/.../basic-001_185001.01_echam"}``).

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
    # Try the canonical name first (fast path)
    candidate = log_dir / f"{expid}_{component}_file_operations_tidy_{run_datestamp}.yaml"
    if candidate.is_file():
        return candidate

    # Glob fallback in case expid differs from directory name
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
    # Top-level: {component: {files: {category: {filename: entry}}}}
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


def extract_stac_metadata(config: dict) -> dict:
    """Extract STAC-relevant metadata from a finished_config.

    Aggregates top-level experiment identity and per-component scientific
    metadata into a flat dict suitable for enriching STAC Items and Collections.

    Args:
        config: Dict loaded by :func:`load_config`.

    Returns:
        Dict with keys:

        ``expid``, ``scenario``, ``resolution``, ``setup_name``,
        ``setup_version``, ``run_datestamp``, ``lresume``
            From ``config["general"]``.

        ``components``
            Nested dict ``{component: {version, institute, authors,
            description, publications}}`` — populated from each component's
            ``metadata`` block (e.g. ``config["echam"]["metadata"]``).
            Only components with a non-empty ``metadata`` block are included.
    """
    general = config.get("general", {})
    skip_keys = {"general", "computer", "setup", "env", "defaults", "recom"}

    components: dict[str, dict] = {}
    for key, block in config.items():
        if key in skip_keys or not isinstance(block, dict):
            continue
        meta = block.get("metadata") or {}
        if not isinstance(meta, dict) or not meta:
            continue
        components[key] = {
            "version": block.get("version"),
            "institute": meta.get("Institute"),
            "authors": meta.get("Authors"),
            "description": meta.get("Description"),
            "publications": meta.get("Publications"),
        }

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
