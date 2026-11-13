"""Native job-crash detection for a backgrounded job.

Replaces legacy's ``observe.py``: watches the PID a backgrounded command
left behind, polling until it exits, and meanwhile tails the job's log
for ``<model>.check_error`` keyword triggers -- a ``kill``-method match
means the job is going wrong and should be stopped now, not left to run
to its walltime limit.

Unlike legacy, a matched ``kill`` trigger *raises*
:class:`~.errors.JobCrashedError` rather than calling ``scancel``/
``sys.exit`` itself -- what to do about a crashed job (cancel the
scheduler allocation, what exit code to use) is the caller's decision,
not this module's. Also drops legacy's crash-to-database recording
(``database_actions.database_entry_crashed``) -- that database layer is
unused and slated for removal, not carried forward here.
"""

from __future__ import annotations

import os
import time
from dataclasses import dataclass
from typing import List

import psutil
from loguru import logger

from .errors import JobCrashedError

_KNOWN_METHODS = ("warn", "kill")


@dataclass(frozen=True)
class ErrorTrigger:
    """One keyword to watch for in a log file."""

    keyword: str
    search_file: str
    method: str  # "warn" or "kill"
    message: str
    frequency: int
    next_check: int


def build_error_triggers(config: dict, log_file: str) -> List[ErrorTrigger]:
    """Every keyword trigger to watch for in ``log_file``: a built-in
    generic ``"error"`` keyword, plus each component's own
    ``check_error`` block."""
    triggers = [
        ErrorTrigger(
            keyword="error",
            search_file=log_file,
            method="warn",
            message="keyword error detected, watch out",
            frequency=60,
            next_check=60,
        )
    ]
    for model_conf in config.values():
        if not isinstance(model_conf, dict):
            continue
        for keyword, spec in model_conf.get("check_error", {}).items():
            if isinstance(spec, dict):
                search_file = log_file
                spec_file = spec.get("file")
                if spec_file:
                    if spec_file in ("stdout", "stderr"):
                        search_file = log_file
                    else:
                        search_file = spec_file.replace(
                            "@jobid@", str(config["general"]["jobid"])
                        )
                method = spec.get("method")
                if method not in _KNOWN_METHODS:
                    method = "warn"
                message = spec.get("message", f"keyword {keyword} detected, watch out")
                try:
                    frequency = int(spec.get("frequency", 60))
                except (TypeError, ValueError):
                    frequency = 60
            elif isinstance(spec, str):
                search_file = log_file
                method = "warn"
                message = f"keyword {keyword} detected, watch out"
                frequency = 60
            else:
                continue
            triggers.append(ErrorTrigger(keyword, search_file, method, message, frequency, frequency))
    return triggers


def check_for_errors(triggers: List[ErrorTrigger], elapsed: int) -> List[ErrorTrigger]:
    """Scan each trigger whose ``next_check`` has elapsed.

    Raises :class:`JobCrashedError` on the first ``kill``-method match.
    A ``warn`` match logs once and the trigger is dropped (matches
    legacy's behaviour -- it doesn't re-warn every poll). Returns the
    triggers still pending.
    """
    remaining = []
    for trigger in triggers:
        if trigger.next_check > elapsed:
            remaining.append(trigger)
            continue

        matched = False
        if os.path.isfile(trigger.search_file):
            with open(trigger.search_file) as f:
                for line in f:
                    if trigger.keyword.upper() in line.upper():
                        matched = True
                        break

        if matched:
            if trigger.method == "kill":
                raise JobCrashedError(trigger.keyword, trigger.message)
            logger.warning(f"WARNING: {trigger.message}")
            continue

        remaining.append(
            ErrorTrigger(
                trigger.keyword,
                trigger.search_file,
                trigger.method,
                trigger.message,
                trigger.frequency,
                trigger.next_check + trigger.frequency,
            )
        )
    return remaining


def job_is_still_running(pid: int) -> bool:
    return psutil.pid_exists(pid)


def wait_and_observe(pid: int, config: dict, log_file: str, poll_seconds: int = 10) -> None:
    """Block until ``pid`` exits, raising :class:`JobCrashedError` the
    moment a ``kill``-method trigger matches its log file. One final
    scan runs after the PID exits, so a crash in the job's last moments
    isn't missed."""
    triggers = build_error_triggers(config, log_file)
    elapsed = 0
    while job_is_still_running(pid):
        triggers = check_for_errors(triggers, elapsed)
        elapsed += poll_seconds
        time.sleep(poll_seconds)
    check_for_errors(triggers, elapsed=max(t.next_check for t in triggers) if triggers else elapsed)
