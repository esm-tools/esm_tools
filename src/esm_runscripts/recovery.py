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


def _resolve_deltas(config, component, deltas):
    """
    Turn a nested delta spec into absolute namelist_changes by reading the
    current value from the namelist file in ``thisrun_config_dir`` and adding
    the delta. Keeps integer/float typing consistent with the source value.
    """
    resolved = {}
    cfg_dir = config[component].get("thisrun_config_dir")
    if not cfg_dir or not os.path.isdir(cfg_dir):
        logger.warning(
            f"Cannot apply namelist_deltas for '{component}': "
            f"thisrun_config_dir unavailable."
        )
        return resolved

    for nml_name, groups in (deltas or {}).items():
        nml_path = os.path.join(cfg_dir, nml_name)
        if not os.path.isfile(nml_path):
            logger.warning(
                f"Recovery delta targets missing namelist: {nml_path}; skipping."
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
                    f"{current} -> {new_value} (delta {delta:+})"
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
