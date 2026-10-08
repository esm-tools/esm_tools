"""VCS/provenance STAC extension: which code version produced this experiment.

Collection-level only (a Collection is a whole experiment, see
:mod:`esm_catalog.namelist`'s own docstring for the same reasoning). Two
sources, in priority order:

1. ``<expid>_vcs_info_initial.yaml`` under the experiment's ``log/`` dir --
   written once by ``esm_runscripts.prepare.add_vcs_info`` from a frozen,
   per-experiment source snapshot (see that function's docstring). This is
   authoritative: the exact code used, whole lifetime of the experiment.
2. If that file is missing (every run older than the snapshot fix, or a run
   with the extension never enabled) -- read git state live from each
   component's ``model_dir`` in the finished_config. This is the *shared*,
   possibly-drifted central checkout, not a per-experiment snapshot, so the
   result is always marked ``reconstructed: true``: it is a best-effort
   after-the-fact guess at what was probably used, not a recorded fact.
"""

from __future__ import annotations

import subprocess
from typing import Any, Optional

import yaml
from upath import UPath

from esm_catalog.models import ExperimentMetadata
from esm_catalog.plugins import hookimpl
from esm_catalog.registry import Extension
from esm_catalog.stac_ext import apply_extension

_DIFF_LINE_CAP = 500
"""Full diffs beyond this are replaced with a stat-only note -- a drifted
shared checkout can be arbitrarily dirty, and this extension must not dump an
unbounded diff into a Collection document."""

_GIT_TIMEOUT_SECONDS = 10


def _vcs_info_initial_path(exp_root: UPath, experiment_id: str) -> UPath:
    return exp_root / "log" / f"{experiment_id}_vcs_info_initial.yaml"


def read_vcs_info_initial(exp_metadata: ExperimentMetadata) -> Optional[dict]:
    """The authoritative, frozen VCS snapshot, or ``None`` if never written."""
    exp_root = UPath(str(exp_metadata.experiment_path))
    path = _vcs_info_initial_path(exp_root, exp_metadata.experiment_id)
    if not path.exists():
        return None
    try:
        data = yaml.safe_load(path.read_text())
    except Exception:  # noqa: BLE001 -- a corrupt file is not fatal to the scan
        return None
    return data if isinstance(data, dict) else None


def _run_git(path: str, *args: str) -> Optional[str]:
    try:
        result = subprocess.run(
            ["git", "-C", path, *args],
            capture_output=True,
            text=True,
            timeout=_GIT_TIMEOUT_SECONDS,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired):
        return None
    if result.returncode != 0:
        return None
    return result.stdout.strip()


def _reconstruct_component_vcs(path: str) -> dict[str, Any]:
    """Live git state for *path*, marked as a reconstruction, never a record.

    Mirrors ``esm_runscripts.helpers.get_all_git_info``'s shape (path, hash,
    branch_name, remote, diffs), minus that module's own dependency, plus the
    ``reconstructed``/``note`` pair this extension adds everywhere it has to
    guess instead of read a recorded fact.
    """
    note = (
        "Reconstructed after the fact from the shared model_dir, not a "
        "per-experiment snapshot -- may not reflect the exact code state "
        "actually used for this run (drift is possible)."
    )
    if _run_git(path, "rev-parse", "--is-inside-work-tree") != "true":
        return {"path": path, "reconstructed": True, "note": "not a git-controlled checkout"}

    diff = _run_git(path, "diff") or ""
    if diff.count("\n") > _DIFF_LINE_CAP:
        stat = _run_git(path, "diff", "--stat") or ""
        diff = f"[diff truncated, over {_DIFF_LINE_CAP} lines -- stat: {stat.strip().splitlines()[-1] if stat else 'unavailable'}]"

    return {
        "path": path,
        "hash": _run_git(path, "rev-parse", "--short", "HEAD"),
        "branch_name": _run_git(path, "rev-parse", "--abbrev-ref", "HEAD"),
        "remote": _run_git(path, "remote", "get-url", "origin"),
        "diffs": diff,
        "reconstructed": True,
        "note": note,
    }


def vcs_processing_software(exp_metadata: ExperimentMetadata) -> dict[str, dict]:
    """``processing:software``'s value: per-component VCS state, however sourced."""
    initial = read_vcs_info_initial(exp_metadata)
    if initial is not None:
        return initial
    return {
        component: _reconstruct_component_vcs(model_dir)
        for component, model_dir in exp_metadata.model_dirs.items()
    }


def add_vcs_collection_extension(collection, exp_metadata: ExperimentMetadata) -> None:
    software = vcs_processing_software(exp_metadata)
    if not software:
        return
    collection.extra_fields["processing:software"] = software
    if any(info.get("reconstructed") for info in software.values() if isinstance(info, dict)):
        collection.extra_fields["processing:lineage"] = (
            "One or more components' VCS state was reconstructed after the "
            "fact from the shared model_dir (no per-experiment snapshot was "
            "recorded for this run) -- see each component's own 'note'."
        )
    else:
        collection.extra_fields["processing:lineage"] = (
            "Recorded from a frozen per-experiment source snapshot "
            "(esm_runscripts.prepare.add_vcs_info)."
        )
    apply_extension(collection, Extension.processing, validate=False)


@hookimpl
def apply_to_collection(collection, exp_metadata, hints) -> None:
    add_vcs_collection_extension(collection, exp_metadata)
