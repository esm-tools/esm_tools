"""VCS/provenance STAC extension: which code version produced each component.

Item-level, not Collection-level: each Item is exactly one (component,
stream) (see item.py's module docstring), so each Item's own component has
exactly one VCS state -- there is no need to force a per-component dict onto
the Collection the way an earlier, now-abandoned design did (bare
``processing:software``, keyed by component). The real ``vcs`` stac-extension
fields (``vcs:type``/``vcs:branch``/``vcs:revision``) land directly on each
Item's properties instead; a plain, unprefixed ``remote`` sits alongside
them (the real extension has no remote field, and is ``additionalProperties:
false`` -- bare ``remote`` is a deliberate, acknowledged addition, not an
extension-schema field).

Two sources per component, in priority order:

1. ``<expid>_vcs_info_initial.yaml`` under the experiment's ``log/`` dir --
   written once by ``esm_runscripts.prepare.add_vcs_info`` from a frozen,
   per-experiment source snapshot (see that function's docstring). This is
   authoritative: the exact code used, whole lifetime of the experiment.
2. If that file is missing (every run older than the snapshot fix, or a run
   with the extension never enabled) -- read git state live from each
   component's ``model_dir`` in the finished_config. This is the *shared*,
   possibly-drifted central checkout, not a per-experiment snapshot, so the
   result is always marked ``reconstructed: true``: it is a best-effort
   after-the-fact guess at what was probably used, not a recorded fact. Both
   sources share the same per-component dict shape (path, hash, branch_name,
   remote, diffs, [reconstructed, note]) -- see
   ``esm_runscripts.helpers.get_all_git_info``.
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
    # exp_metadata.experiment_path is already the real UPath (storage_options
    # and all) -- re-deriving one from str() loses the host for remote
    # protocols (sftp://, ...); confirmed live (SSHFileSystem.__init__()
    # missing 'host').
    path = _vcs_info_initial_path(exp_metadata.experiment_path, exp_metadata.experiment_id)
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


def vcs_by_component(exp_metadata: ExperimentMetadata) -> dict[str, dict]:
    """Every component's VCS state (frozen snapshot if present, else reconstructed)."""
    initial = read_vcs_info_initial(exp_metadata)
    if initial is not None:
        return initial
    return {
        component: _reconstruct_component_vcs(model_dir)
        for component, model_dir in exp_metadata.model_dirs.items()
    }


#: collection_id -> already-computed vcs_by_component() result, computed once
#: per experiment (reading the snapshot file or shelling out to git is not
#: free, and every Item in the experiment would otherwise trigger it again).
_SOFTWARE_CACHE: dict[str, dict] = {}


def _vcs_by_component_once(exp_metadata: ExperimentMetadata) -> dict[str, dict]:
    collection_id = exp_metadata.collection_id
    if collection_id in _SOFTWARE_CACHE:
        return _SOFTWARE_CACHE[collection_id]
    software = vcs_by_component(exp_metadata)
    _SOFTWARE_CACHE[collection_id] = software
    return software


def vcs_item_props(component_vcs: dict) -> dict:
    """The ``vcs:*`` (+ bare ``remote``) Item properties for one component's VCS dict.

    Parameters
    ----------
    component_vcs : dict
        One component's entry from :func:`vcs_by_component` (frozen-snapshot
        or reconstructed shape -- both share path/hash/branch_name/remote/
        diffs/[reconstructed/note]).

    Returns
    -------
    dict
        The properties to set on the Item; empty if *component_vcs* has no
        hash (nothing to report).
    """
    if not component_vcs.get("hash"):
        return {}
    props: dict[str, Any] = {
        "vcs:type": "git",
        "vcs:revision": component_vcs["hash"],
    }
    if component_vcs.get("branch_name"):
        props["vcs:branch"] = component_vcs["branch_name"]
    if component_vcs.get("remote"):
        # Bare, not vcs:remote: the real extension has no remote field and is
        # additionalProperties: false.
        props["remote"] = component_vcs["remote"]
    if component_vcs.get("reconstructed"):
        props["reconstructed"] = True
        if component_vcs.get("note"):
            props["reconstructed_note"] = component_vcs["note"]
    return props


def add_vcs_item_extension(item, component: str, exp_metadata: ExperimentMetadata) -> None:
    """Set ``vcs:*`` on *item* from *component*'s VCS state, if any is known."""
    software = _vcs_by_component_once(exp_metadata)
    component_vcs = software.get(component)
    if not isinstance(component_vcs, dict):
        return
    props = vcs_item_props(component_vcs)
    if not props:
        return
    item.properties.update(props)
    apply_extension(item, Extension.vcs, validate=False)


@hookimpl
def apply_to_item(item, file_metadata, exp_metadata, hints) -> None:
    component = file_metadata.component
    if not component:
        return
    add_vcs_item_extension(item, component, exp_metadata)
