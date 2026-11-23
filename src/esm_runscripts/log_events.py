"""Controlled event vocabulary + the shared JSONL emission helper for
native pplan's structured event log (``<experiment_log_file>.jsonl``,
added by :func:`~esm_runscripts.logfiles.set_jsonl_event_sink`).

Lives outside both ``execution/`` and ``runscript/`` because both need
it without depending on each other: :mod:`esm_runscripts.execution.
dispatch` emits :class:`DispatchEvent`\\ s from Python (dispatch-time
truth -- "I handed this off"); :mod:`esm_runscripts.execution.cli`'s
observe loop and :mod:`esm_runscripts.runscript.generation` both emit
:class:`ExecutionEvent`\\ s (actual-process truth -- "it's really
running" / "it really finished"), the latter from a single shell-
embedded line, since that's the one moment real wall-clock start can
only be known from inside the spawned process itself, after Python has
already returned (``compute`` backgrounds and returns immediately).
"""

from __future__ import annotations

from esm_tools._compat import StrEnum


class DispatchEvent(StrEnum):
    """Dispatch-time truth: what :func:`~esm_runscripts.execution.
    dispatch._cascade` itself did. A closed set, not a free-form string
    -- same convention as :class:`~esm_runscripts.pplan.plan.Executor`/
    :class:`~esm_runscripts.pplan.plan.MultiStepKind`."""

    DISPATCHED = "dispatch.dispatched"
    """A step is about to run -- emitted right before dispatching it."""

    FINISHED = "dispatch.finished"
    """An ``IN_PROCESS`` step actually ran to completion, synchronously,
    in this call."""

    LAUNCHED = "dispatch.launched"
    """A ``LOCAL``/``BATCH`` step was launched (subprocess spawned or
    scheduler job submitted) -- fire-and-forget, not "finished"."""

    FAILED = "dispatch.failed"
    """A step raised while dispatching."""


class ExecutionEvent(StrEnum):
    """The actual spawned process's own lifecycle -- distinct from
    :class:`DispatchEvent`, which only ever means "Python handed this
    off" for a ``LOCAL``/``BATCH`` target, never that it's actually
    running or done. ``STARTED`` is emitted from the generated shell
    script itself, right before the real command runs (the only point
    that knows true wall-clock start); ``FINISHED``/``FAILED`` are
    emitted from :func:`~esm_runscripts.execution.observe.
    wait_and_observe`'s caller, once it detects the watched PID actually
    exiting (cleanly, or via a ``kill``-trigger crash)."""

    STARTED = "execution.started"
    FINISHED = "execution.finished"
    FAILED = "execution.failed"


def jsonl_path(config: dict) -> str:
    """Where the structured event log lives for this experiment -- a
    sibling of ``general.experiment_log_file``, the one source both the
    Python-side sink (:func:`~esm_runscripts.logfiles.
    set_jsonl_event_sink`) and the shell-embedded line
    (:func:`~esm_runscripts.runscript.generation.execution_started_line`)
    agree on."""
    return f'{config["general"]["experiment_log_file"]}.jsonl'


def log_event(config: dict, event: "DispatchEvent | ExecutionEvent", multi_step_name: str, executor=None, **fields) -> None:
    """Emit one structured event via loguru, picked up only by
    :func:`~esm_runscripts.logfiles.set_jsonl_event_sink`'s filter (a
    no-op if that sink was never added -- e.g. ``--wf-engine legacy``,
    which never touches this module at all). ``multi_step_name`` is
    always the real step name passed in by the caller, never read back
    from ``config["general"]["jobtype"]`` (CLI's ``-t`` default, never
    updated as dispatch walks the chain)."""
    from loguru import logger

    general = config.get("general", {})
    logger.bind(
        expid=general.get("expid"),
        run_number=general.get("run_number"),
        current_date=str(general.get("current_date")) if general.get("current_date") else None,
        jobid=general.get("jobid"),
        multi_step_name=multi_step_name,
        executor=executor.value if executor is not None else None,
        event=event,
        **fields,
    ).debug(event)
