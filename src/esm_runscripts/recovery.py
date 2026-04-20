"""
Auto-recovery plugin for ``esm-runscripts``.

Implements the ``recover`` method of the ``check_error`` feature: when a
configured error pattern is found in a model log, the compute job is killed and
a small fix (absolute namelist override and/or additive delta) is applied
before the same run is resubmitted.

State persists across process boundaries in a JSON file next to the
experiment's ``.date`` file so that a fresh ``SimulationSetup`` (spawned by
``resubmit.maybe_resubmit``) can pick up the pending fix.
"""

import json
import os

import f90nml
from loguru import logger


def _state_path(config):
    return (
        f"{config['general']['experiment_scripts_dir']}"
        f"/{config['general']['expid']}_{config['general']['setup_name']}"
        f".recovery.json"
    )


def _k_gm_max_status_path(config):
    return (
        f"{config['general']['experiment_scripts_dir']}"
        f"/{config['general']['expid']}_{config['general']['setup_name']}"
        f".recovery_k_gm_max_status.dat"
    )


def load_state(config):
    path = _state_path(config)
    if not os.path.isfile(path):
        return None
    try:
        with open(path) as fh:
            return json.load(fh)
    except (OSError, json.JSONDecodeError) as e:
        logger.warning(f"Could not read recovery state at {path}: {e}")
        return None


def _write_state(config, state):
    path = _state_path(config)
    with open(path, "w") as fh:
        json.dump(state, fh, indent=2)
    logger.info(f"Recovery state written to {path}")


def clear_state(config):
    path = _state_path(config)
    if os.path.isfile(path):
        os.remove(path)
        logger.info(f"Cleared recovery state at {path}")


def _format_numeric(value):
    return f"{float(value):.15g}"


def _tracking_year(config, state=None):
    if state and state.get("run_date"):
        try:
            return int(str(state["run_date"])[:4])
        except (TypeError, ValueError):
            pass

    current_date = config["general"].get("current_date")
    if current_date is not None:
        try:
            return int(current_date.year)
        except AttributeError:
            try:
                return int(str(current_date)[:4])
            except (TypeError, ValueError):
                pass

    return None


def _load_k_gm_max_status_entries(config):
    path = _k_gm_max_status_path(config)
    entries = {}
    if not os.path.isfile(path):
        return entries

    with open(path) as fh:
        for lineno, line in enumerate(fh, start=1):
            stripped = line.strip()
            if not stripped:
                continue
            parts = stripped.split()
            if len(parts) != 3:
                logger.warning(
                    f"Malformed recovery k_gm_max status line {lineno} in {path}; "
                    "expected 3 columns."
                )
                continue
            try:
                year = int(parts[0])
                value = float(parts[1])
                status = int(parts[2])
            except ValueError:
                logger.warning(
                    f"Malformed recovery k_gm_max status line {lineno} in {path}; "
                    "could not parse numeric fields."
                )
                continue
            entries[year] = (value, status)
    return entries


def _write_k_gm_max_status_entries(config, entries):
    path = _k_gm_max_status_path(config)
    with open(path, "w") as fh:
        for year in sorted(entries):
            value, status = entries[year]
            fh.write(f"{year} {_format_numeric(value)} {int(status)}\n")
    logger.info(f"Recovery k_gm_max status written to {path}")


def has_k_gm_max_status_entry(config, year=None):
    if year is None:
        year = _tracking_year(config)
    if year is None:
        return False
    return year in _load_k_gm_max_status_entries(config)


def _read_k_gm_max_from_namelist(path):
    if not path or not os.path.isfile(path):
        return None
    try:
        nml = f90nml.read(path)
        return nml["oce_dyn"]["k_gm_max"]
    except (OSError, KeyError, TypeError, ValueError) as e:
        logger.warning(f"Could not read k_gm_max from {path}: {e}")
        return None


def _current_k_gm_max(config):
    work_dir = config["general"].get("thisrun_work_dir")
    if work_dir:
        value = _read_k_gm_max_from_namelist(os.path.join(work_dir, "namelist.oce"))
        if value is not None:
            return value

    fesom_cfg = config.get("fesom", {})
    cfg_dir = fesom_cfg.get("thisrun_config_dir")
    if cfg_dir:
        value = _read_k_gm_max_from_namelist(os.path.join(cfg_dir, "namelist.oce"))
        if value is not None:
            return value

    return None


def _pending_k_gm_max(target_changes):
    return (
        target_changes.get("namelist.oce", {})
        .get("oce_dyn", {})
        .get("k_gm_max")
    )


def update_k_gm_max_status(config, status, value=None, year=None):
    if year is None:
        year = _tracking_year(config)
    if year is None:
        logger.warning("Could not determine year for recovery k_gm_max status.")
        return

    entries = _load_k_gm_max_status_entries(config)
    if value is None and year in entries:
        value = entries[year][0]
    if value is None:
        value = _current_k_gm_max(config)
    if value is None:
        logger.warning(
            f"Could not determine k_gm_max for year {year}; skipping status update."
        )
        return

    entries[year] = (float(value), int(status))
    _write_k_gm_max_status_entries(config, entries)


def record_pending_k_gm_max(config, state, target_changes):
    value = _pending_k_gm_max(target_changes)
    if value is None:
        return
    update_k_gm_max_status(
        config,
        status=0,
        value=value,
        year=_tracking_year(config, state),
    )


def record_success(config):
    update_k_gm_max_status(config, status=1)


def record_failure(config):
    update_k_gm_max_status(config, status=0)


def record_trigger(config, component, trigger, trigger_cfg):
    """
    Called by observe.py when a ``recover`` trigger fires. Creates or updates
    the recovery state file, bumping the attempt counter if the same trigger
    has already fired before.

    Returns the updated state dict, or ``None`` if max_retries is already
    exhausted (caller should log an error and stop).
    """
    max_retries = int(trigger_cfg.get("max_retries", 3))
    fix = trigger_cfg.get("fix", {}) or {}

    current = load_state(config) or {}
    prev_attempt = 0
    if (
        current.get("active")
        and current.get("trigger") == trigger
        and current.get("component") == component
    ):
        prev_attempt = int(current.get("attempt", 0))

    attempt = prev_attempt + 1
    if attempt > max_retries:
        logger.error(
            f"Recovery trigger '{trigger}' in component '{component}' has "
            f"already been retried {prev_attempt} times (max_retries="
            f"{max_retries}); giving up."
        )
        return None

    state = {
        "active": True,
        "component": component,
        "trigger": trigger,
        "attempt": attempt,
        "max_retries": max_retries,
        "run_number": config["general"].get("run_number"),
        "run_date": str(config["general"].get("current_date")),
        "fix": fix,
        "message": trigger_cfg.get("message", ""),
    }
    _write_state(config, state)
    return state


def has_pending_recovery(config):
    state = load_state(config)
    return bool(state and state.get("active"))


def _merge_namelist_changes(target, addition):
    for nml_name, groups in (addition or {}).items():
        target.setdefault(nml_name, {})
        for group, entries in (groups or {}).items():
            target[nml_name].setdefault(group, {})
            target[nml_name][group].update(entries or {})


def _get_recovery_namelist_path(config, component, nml_name):
    """
    Return the best namelist path to use as the baseline for additive recovery
    deltas.

    For retries of the SAME run, ``copy_files_to_thisrun`` refreshes the
    ``thisrun_config_dir`` from the original inputs before ``apply_recovery_fix``
    runs. To make additive perturbations accumulate across retries, prefer the
    namelist from the existing ``work`` directory when present, and fall back to
    ``thisrun_config_dir`` otherwise.
    """
    work_dir = config["general"].get("thisrun_work_dir")
    if work_dir:
        work_path = os.path.join(work_dir, nml_name)
        if os.path.isfile(work_path):
            return work_path

    cfg_dir = config[component].get("thisrun_config_dir")
    if cfg_dir:
        cfg_path = os.path.join(cfg_dir, nml_name)
        if os.path.isfile(cfg_path):
            return cfg_path

    return None


def _resolve_deltas(config, component, deltas):
    """
    Turn a nested delta spec into absolute namelist_changes by reading the
    current value from the namelist file in ``thisrun_config_dir`` and adding
    the delta. Keeps integer/float typing consistent with the source value.
    """
    resolved = {}
    cfg_dir = config[component].get("thisrun_config_dir")
    work_dir = config["general"].get("thisrun_work_dir")
    if (
        (not cfg_dir or not os.path.isdir(cfg_dir))
        and (not work_dir or not os.path.isdir(work_dir))
    ):
        logger.warning(
            f"Cannot apply namelist_deltas for '{component}': "
            f"neither thisrun_config_dir nor thisrun_work_dir is available."
        )
        return resolved

    for nml_name, groups in (deltas or {}).items():
        nml_path = _get_recovery_namelist_path(config, component, nml_name)
        if not nml_path:
            logger.warning(
                f"Recovery delta targets missing namelist '{nml_name}' in "
                f"thisrun_work_dir/thisrun_config_dir; skipping."
            )
            continue
        nml = f90nml.read(nml_path)
        for group, entries in (groups or {}).items():
            for key, delta in (entries or {}).items():
                try:
                    current = nml[group][key]
                except KeyError:
                    logger.warning(
                        f"Recovery delta target {nml_name}:{group}:{key} not "
                        f"found in namelist; skipping."
                    )
                    continue
                new_value = current + delta
                if isinstance(current, int) and isinstance(delta, int):
                    new_value = int(new_value)
                resolved.setdefault(nml_name, {}).setdefault(group, {})[key] = (
                    new_value
                )
                logger.info(
                    f"Recovery perturbation: {nml_name}:{group}:{key} "
                    f"{current} -> {new_value} (delta {delta:+}; source {nml_path})"
                )
    return resolved


def apply_fix_to_config(config):
    """
    Called from prepcompute. If a recovery state is active, merges its ``fix``
    block into the target component's ``namelist_changes`` so that the normal
    ``Namelist.nmls_modify`` step picks it up.
    """
    state = load_state(config)
    if not state or not state.get("active"):
        return config

    expected_run = config["general"].get("run_number")
    if state.get("run_number") != expected_run:
        logger.warning(
            f"Stale recovery state (run_number={state.get('run_number')} vs "
            f"current {expected_run}); clearing without applying."
        )
        clear_state(config)
        return config

    component = state["component"]
    if component not in config:
        logger.warning(
            f"Recovery targets component '{component}' which is not in this "
            f"config; clearing state."
        )
        clear_state(config)
        return config

    logger.warning(
        "=" * 70
        + f"\nAPPLYING RECOVERY FIX (attempt {state['attempt']}/"
        f"{state['max_retries']}) for trigger '{state['trigger']}' in "
        f"component '{component}'\n"
        + (f"Reason: {state['message']}\n" if state.get("message") else "")
        + "=" * 70
    )

    fix = state.get("fix", {}) or {}
    absolute = fix.get("namelist_changes", {}) or {}
    deltas = fix.get("namelist_deltas", {}) or {}

    resolved_deltas = _resolve_deltas(config, component, deltas)

    target = config[component].setdefault("namelist_changes", {})
    _merge_namelist_changes(target, absolute)
    _merge_namelist_changes(target, resolved_deltas)
    record_pending_k_gm_max(config, state, target)

    return config


def trigger_recovery_resubmit(config):
    """
    Called from ``resubmit.maybe_resubmit`` when a recovery is pending. Skips
    the normal date increment and directly submits a fresh ``SimulationSetup``
    for ``prepcompute`` (same run_number, same date). The pending state is
    left in place so ``apply_fix_to_config`` can consume it.
    """
    state = load_state(config)
    logger.warning(
        f"Recovery pending (attempt {state['attempt']}/{state['max_retries']}"
        f") — resubmitting prepcompute for the same run."
    )

    from . import resubmit

    resubmit.resubmit_SimulationSetup(config, "prepcompute")
    return config
