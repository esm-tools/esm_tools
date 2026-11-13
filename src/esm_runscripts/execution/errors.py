"""Errors raised by execution-time job monitoring. Raised, not
``sys.exit``\\ ed -- the decision to actually terminate anything (scheduler
cancellation, process exit code) belongs to the caller, not to the
detection logic that notices something went wrong.
"""

from __future__ import annotations


class JobCrashedError(Exception):
    """Raised by :func:`~esm_runscripts.execution.observe.check_for_errors`
    when a ``kill``-method error trigger matches a line in the watched
    job's log."""

    def __init__(self, trigger: str, message: str):
        self.trigger = trigger
        self.message = message
        super().__init__(f"{trigger}: {message}")
