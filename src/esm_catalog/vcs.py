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

import shlex
import subprocess
from typing import Any, Optional

import yaml
from loguru import logger
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


def _ssh_host(exp_metadata: ExperimentMetadata) -> Optional[str]:
    """The remote host *model_dirs* live on, or ``None`` for a local scan.

    ``model_dirs`` are plain path strings taken from the finished_config,
    which was itself written on whatever host ran the experiment -- the same
    host ``experiment_path`` points at. A local ``git -C <model_dir>``
    subprocess only makes sense when that host is this machine.
    """
    path = exp_metadata.experiment_path
    if getattr(path, "protocol", None) in ("ssh", "sftp"):
        return getattr(path, "storage_options", {}).get("host") or None
    return None


def _run_git(path: str, *args: str, host: Optional[str] = None) -> Optional[str]:
    if host is None:
        argv = ["git", "-C", path, *args]
    else:
        # ssh joins trailing argv with bare spaces, not shell-quoting --
        # build one already-quoted remote command string ourselves.
        argv = ["ssh", host, shlex.join(["git", "-C", path, *args])]
    try:
        result = subprocess.run(
            argv,
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


def _reconstruct_component_vcs(path: str, host: Optional[str] = None) -> dict[str, Any]:
    """Live git state for *path*, marked as a reconstruction, never a record.

    Mirrors ``esm_runscripts.helpers.get_all_git_info``'s shape (path, hash,
    branch_name, remote, diffs), minus that module's own dependency, plus the
    ``reconstructed``/``note`` pair this extension adds everywhere it has to
    guess instead of read a recorded fact.

    *host* runs every git invocation over ``ssh <host> ...`` instead of a
    local subprocess -- required when *path* is a model_dir on the same
    remote machine ``exp_root`` was scanned from (see :func:`_ssh_host`);
    passwordless ssh to that host is assumed, same as the sftp access the
    scan itself already needs to reach ``exp_root``.
    """
    note = (
        "Reconstructed after the fact from the shared model_dir, not a "
        "per-experiment snapshot -- may not reflect the exact code state "
        "actually used for this run (drift is possible)."
    )
    if _run_git(path, "rev-parse", "--is-inside-work-tree", host=host) != "true":
        reason = "unreachable over ssh" if host else "not a git-controlled checkout"
        return {"path": path, "reconstructed": True, "note": reason}

    diff = _run_git(path, "diff", host=host) or ""
    if diff.count("\n") > _DIFF_LINE_CAP:
        stat = _run_git(path, "diff", "--stat", host=host) or ""
        diff = f"[diff truncated, over {_DIFF_LINE_CAP} lines -- stat: {stat.strip().splitlines()[-1] if stat else 'unavailable'}]"

    return {
        "path": path,
        "hash": _run_git(path, "rev-parse", "--short", "HEAD", host=host),
        "branch_name": _run_git(path, "rev-parse", "--abbrev-ref", "HEAD", host=host),
        "remote": _run_git(path, "remote", "get-url", "origin", host=host),
        "diffs": diff,
        "reconstructed": True,
        "note": note,
    }


def vcs_by_component(exp_metadata: ExperimentMetadata) -> dict[str, dict]:
    """Every component's VCS state (frozen snapshot if present, else reconstructed)."""
    initial = read_vcs_info_initial(exp_metadata)
    if initial is not None:
        return initial
    host = _ssh_host(exp_metadata)
    return {
        component: _reconstruct_component_vcs(model_dir, host=host)
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
    # vcs_item_props() silently returns {} for a component with no "hash" --
    # every failure mode (snapshot file missing, model_dir unreachable over
    # ssh, not a git checkout, component absent from model_dirs) collapsed
    # into total silence, with no way to tell which one happened for a given
    # real run (confirmed live: every production collection checked had zero
    # vcs:revision stamping on every item, with nothing in the logs saying
    # why). _reconstruct_component_vcs already computes a "note" explaining
    # the reason in the reconstructed case; this is the one place that both
    # sees it and runs only once per experiment (not once per Item).
    for component, component_vcs in software.items():
        if isinstance(component_vcs, dict) and not component_vcs.get("hash"):
            logger.warning(
                "No vcs:* info for component {!r} in {}: {}",
                component,
                collection_id,
                component_vcs.get("note", "no revision hash recorded"),
            )
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


#: (collection_id, component) pairs already warned about by
#: add_vcs_item_extension -- called once per Item, so without this a
#: component missing from model_dirs/the snapshot entirely would log once
#: per file instead of once per experiment.
_MISSING_COMPONENT_WARNED: set[tuple[str, str]] = set()


def add_vcs_item_extension(item, component: str, exp_metadata: ExperimentMetadata) -> None:
    """Set ``vcs:*`` on *item* from *component*'s VCS state, if any is known."""
    software = _vcs_by_component_once(exp_metadata)
    component_vcs = software.get(component)
    if not isinstance(component_vcs, dict):
        key = (exp_metadata.collection_id, component)
        if key not in _MISSING_COMPONENT_WARNED:
            _MISSING_COMPONENT_WARNED.add(key)
            logger.warning(
                "No vcs:* info for component {!r} in {}: not present in "
                "the vcs_info_initial snapshot or model_dirs at all.",
                component,
                exp_metadata.collection_id,
            )
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
