"""
Auto-recovery plugin for ``esm-runscripts``.

Implements the ``recover`` method of the ``check_error`` feature: when a
configured error pattern is found in a model log, the running compute
launcher is stopped and a small fix (absolute namelist override and/or
additive delta) is applied before the same run is resubmitted.

Two pieces of state are written to the experiment's ``scripts`` directory:

* ``<expid>_<setup>.recovery.json`` — short-lived JSON that carries the
  pending fix across the ``observe`` → ``maybe_resubmit`` → fresh
  ``SimulationSetup`` → ``prepcompute`` boundary. Removed once a retried run
  finishes without firing a ``recover`` trigger again.

* ``<expid>_<setup>.recovery_status.dat`` — long-lived per-year log of
  every run that needed recovery, which namelist entries were perturbed, and
  whether the retry eventually succeeded. The file is generic: any keys
  present in the trigger's ``fix.namelist_changes`` / ``fix.namelist_deltas``
  are flattened to ``<namelist>:<group>:<key>`` tokens and appended to the
  row for that year. Setup-specific post-processing (e.g. plotting FESOM's
  ``K_GM_max`` history) can read it without requiring any changes here.
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


def _status_path(config):
    return (
        f"{config['general']['experiment_scripts_dir']}"
        f"/{config['general']['expid']}_{config['general']['setup_name']}"
        f".recovery_status.dat"
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


def _format_value(value):
    if isinstance(value, float):
        return f"{value:.15g}"
    return str(value)


def _parse_value(text):
    try:
        return int(text)
    except ValueError:
        pass
    try:
        return float(text)
    except ValueError:
        return text


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


def _load_status_entries(config):
    """
    Parse the recovery status file. Each non-empty line has the form::

        <year> <status> [<namelist>:<group>:<key>=<value> ...]

    Returns ``{year: {"status": int, "values": {flat_key: value}}}``.
    """
    path = _status_path(config)
    entries = {}
    if not os.path.isfile(path):
        return entries

    with open(path) as fh:
        for lineno, line in enumerate(fh, start=1):
            stripped = line.strip()
            if not stripped:
                continue
            parts = stripped.split()
            if len(parts) < 2:
                logger.warning(
                    f"Malformed recovery status line {lineno} in {path}; "
                    "expected at least <year> <status>."
                )
                continue
            try:
                year = int(parts[0])
                status = int(parts[1])
            except ValueError:
                logger.warning(
                    f"Malformed recovery status line {lineno} in {path}; "
                    "could not parse year/status."
                )
                continue
            values = {}
            for item in parts[2:]:
                if "=" not in item:
                    continue
                key, _, raw = item.partition("=")
                values[key] = _parse_value(raw)
            entries[year] = {"status": status, "values": values}
    return entries


def _write_status_entries(config, entries):
    path = _status_path(config)
    with open(path, "w") as fh:
        for year in sorted(entries):
            entry = entries[year]
            tokens = [str(year), str(int(entry["status"]))]
            for key in sorted(entry.get("values", {})):
                tokens.append(f"{key}={_format_value(entry['values'][key])}")
            fh.write(" ".join(tokens) + "\n")
    logger.info(f"Recovery status written to {path}")


def has_recovery_status_entry(config, year=None):
    if year is None:
        year = _tracking_year(config)
    if year is None:
        return False
    return year in _load_status_entries(config)


def update_recovery_status(config, status, values=None, year=None):
    """
    Update (or create) the status row for ``year``. ``values`` is an optional
    flat ``{namelist:group:key: value}`` mapping that is merged into the row's
    existing values. ``status`` is ``1`` on success, ``0`` for
    failure / still-pending.
    """
    if year is None:
        year = _tracking_year(config)
    if year is None:
        logger.warning("Could not determine year for recovery status.")
        return

    entries = _load_status_entries(config)
    merged_values = dict(entries.get(year, {}).get("values", {}))
    if values:
        merged_values.update(values)
    entries[year] = {"status": int(status), "values": merged_values}
    _write_status_entries(config, entries)


def _flatten_fix_values(fix, resolved_deltas):
    """
    Flatten the ``fix`` block to ``{namelist:group:key: value}`` entries. The
    ``namelist_deltas`` section contributes the *resolved* absolute value (not
    the delta itself) so the log records what actually ended up in the
    namelist.
    """
    flat = {}

    absolute = (fix or {}).get("namelist_changes", {}) or {}
    for nml, groups in absolute.items():
        for group, entries in (groups or {}).items():
            for key, value in (entries or {}).items():
                flat[f"{nml}:{group}:{key}"] = value

    deltas = (fix or {}).get("namelist_deltas", {}) or {}
    for nml, groups in deltas.items():
        for group, entries in (groups or {}).items():
            for key in (entries or {}):
                resolved = (
                    resolved_deltas.get(nml, {})
                    .get(group, {})
                    .get(key)
                )
                if resolved is not None:
                    flat[f"{nml}:{group}:{key}"] = resolved

    return flat


def record_pending_recovery(config, state, resolved_deltas):
    """
    Persist the resolved fix values for the current retry as a status row with
    ``status=0`` (pending). Called from ``apply_fix_to_config`` so the values
    survive across process boundaries regardless of which component is being
    patched.
    """
    flat = _flatten_fix_values(state.get("fix", {}), resolved_deltas)
    if not flat:
        return
    update_recovery_status(
        config,
        status=0,
        values=flat,
        year=_tracking_year(config, state),
    )


def record_success(config):
    update_recovery_status(config, status=1)


def record_failure(config):
    update_recovery_status(config, status=0)


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
    current value from the component's namelist and adding the delta. Prefers
    the value from ``thisrun_work_dir`` (so additive deltas accumulate across
    same-run retries) and falls back to ``thisrun_config_dir``. Keeps
    integer/float typing consistent with the source value.
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
    ``Namelist.nmls_modify`` step picks it up, and appends the resolved values
    to the long-lived recovery status log.
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
    record_pending_recovery(config, state, resolved_deltas)

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
