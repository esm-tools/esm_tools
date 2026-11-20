"""Picking the right execution strategy for a resolved
:class:`~esm_runscripts.pplan.plan.Workflow`'s work, per
:class:`~esm_runscripts.pplan.plan.Executor`.

This is the "execution" half of the staged vocabulary-rename plan (see
esm-tools/esm_tools#1519): rename -> models -> REPL -> execution/provenance.
``pplan.plan`` stays the read-only model layer; this module is where
deciding *how* a sub-plan's work actually runs belongs.
"""

import time

from ..log_events import DispatchEvent, log_event
from ..pplan import plan as plan_module
from . import launch, steps as steps_module


def run_in_process(workflow: "plan_module.Workflow", multi_step_name: str, config: dict) -> dict:
    """Run ``multi_step_name``'s steps directly, in the current process.

    Only valid for a sub-plan whose parent :class:`~plan.Plan` has
    ``executor == Executor.IN_PROCESS`` (in :class:`~plan.PlanExecution`)
    -- no new OS process, no runscript, just
    :func:`~esm_runscripts.execution.steps.run_steps` called right here
    (which resolves ``multi_step_name``'s recipe steps lazily, at this
    exact moment -- see that module's docstring for why). ``launch_local``/
    ``launch_batch`` (:mod:`.launch`) spawn a process instead.

    Always actually runs, even in check mode (``-c``) -- matching legacy's
    ``resubmit.py``, whose ``check_if_check`` only ever gates the next
    job's *submission* (``submit()``/``kill_after_submit``), never a
    step's own real work. An ``IN_PROCESS`` step (``prepexp``,
    ``prepcompute``, ``tidy``) builds the experiment tree and config that
    the next, possibly ``LOCAL``/``BATCH`` dispatch's own check-mode guard
    (:mod:`.launch`) depends on actually existing -- skipping it here
    would make ``-c`` fail before even reaching that guard.
    """
    parent = workflow.parent_plan_of(multi_step_name)
    execution = workflow.execution_of(multi_step_name)
    if execution.executor != plan_module.Executor.IN_PROCESS:
        raise ValueError(
            f"run_in_process called for {multi_step_name!r}, whose parent "
            f"plan {parent.name!r} has executor={execution.executor!r}, not "
            f"{plan_module.Executor.IN_PROCESS!r}."
        )
    return steps_module.run_steps(config, multi_step_name)


# One strategy per Executor value. advance() looks a target's executor up
# here rather than branching on it directly.
_DISPATCHERS = {
    plan_module.Executor.IN_PROCESS: run_in_process,
    plan_module.Executor.LOCAL: launch.launch_local,
    plan_module.Executor.BATCH: launch.launch_batch,
}


def _dispatch_one(workflow: "plan_module.Workflow", multi_step_name: str, config: dict, caller: str) -> dict:
    """Look ``multi_step_name``'s parent executor up in ``_DISPATCHERS``
    and run the matching strategy. Shared by :func:`start` and
    :func:`advance` -- ``caller`` is only for the error message, naming
    which one couldn't dispatch."""
    parent = workflow.parent_plan_of(multi_step_name)
    execution = workflow.execution_of(multi_step_name)
    strategy = _DISPATCHERS.get(execution.executor)
    if strategy is None:
        raise NotImplementedError(
            f"{caller}() can't dispatch {multi_step_name!r} yet -- its parent "
            f"plan {parent.name!r} has executor={execution.executor!r}, "
            f"and no strategy is registered for it (only "
            f"{[e.value for e in _DISPATCHERS]} so far)."
        )
    return strategy(workflow, multi_step_name, config)


def _cascade(workflow: "plan_module.Workflow", multi_step_name: str, config: dict, caller: str) -> dict:
    """Dispatch ``multi_step_name``, then keep cascading through
    ``Workflow.resubmit_triggers`` for as long as the step just dispatched
    is ``IN_PROCESS`` -- it really did finish, synchronously, right here,
    so whatever it triggers must run next in this same call. Stops at the
    first ``LOCAL``/``BATCH`` dispatch: that's fire-and-forget (a spawned
    process or scheduler submission), and whatever it finishes into gets
    resumed later, by its own observe re-entry calling :func:`advance`.

    Returns ``{multi_step_name: resulting_config, ...}`` for every step
    this call actually dispatched, entry point included.
    """
    executor = workflow.execution_of(multi_step_name).executor
    log_event(config, DispatchEvent.DISPATCHED, multi_step_name, executor)
    started = time.monotonic()
    try:
        result = _dispatch_one(workflow, multi_step_name, config, caller=caller)
    except Exception as exc:
        log_event(
            config, DispatchEvent.FAILED, multi_step_name, executor,
            duration_ms=int((time.monotonic() - started) * 1000),
            error=str(exc),
        )
        raise
    duration_ms = int((time.monotonic() - started) * 1000)
    finished_event = DispatchEvent.FINISHED if executor == plan_module.Executor.IN_PROCESS else DispatchEvent.LAUNCHED
    log_event(config, finished_event, multi_step_name, executor, duration_ms=duration_ms)

    results = {multi_step_name: result}
    if executor != plan_module.Executor.IN_PROCESS:
        return results

    if multi_step_name == workflow.exit_point:
        # The cycle that just tidied up is over -- current_date/run_number
        # must advance before anything cascades into resubmit_triggers'
        # target(s) (e.g. "prepcompute"), which prepares the *next* date's
        # tree. Advancing any earlier would make exit_point itself (e.g.
        # "tidy") run against a date whose directories were never created.
        from . import cycle

        cycle.advance_date(config)
        cycle.advance_run_number(config)
        cycle.sync_command_line_config(config)
        cycle.write_date_file(config)
        if cycle.is_experiment_over(config):
            config["general"]["experiment_over"] = True
            return results

    for next_name in workflow.resubmit_triggers.get(multi_step_name, []):
        results.update(_cascade(workflow, next_name, config, caller=caller))
    return results


def start(workflow: "plan_module.Workflow", config: dict) -> dict:
    """Dispatch ``workflow.entry_point`` -- the bootstrap call for a
    fresh invocation (nothing has triggered anything yet) -- then cascade
    through every ``IN_PROCESS`` link ``Workflow.resubmit_triggers``
    chains from it (e.g. ``prepexp -> prepcompute``), stopping at the
    first ``LOCAL``/``BATCH`` dispatch (``compute``, normally). See
    :func:`_cascade`."""
    return _cascade(workflow, workflow.entry_point, config, caller="start")


def advance(workflow: "plan_module.Workflow", finished_multi_step_name: str, config: dict) -> dict:
    """Dispatch whatever ``finished_multi_step_name`` triggers next, then
    cascade each of those through their own ``IN_PROCESS`` chains too (see
    :func:`_cascade`).

    Native replacement for legacy's ``resubmit_recursively``/
    ``maybe_resubmit`` (``resubmit.py``), reading
    ``Workflow.resubmit_triggers`` (the fan-out-capable native shape) in
    place of the legacy dict's per-plan ``next_submit`` list.

    Looks each target's parent ``Plan.executor`` up in ``_DISPATCHERS``; a
    target whose executor has no registered strategy raises
    ``NotImplementedError`` naming it, rather than silently doing nothing.

    Returns ``{multi_step_name: resulting_config}`` for every step this
    call (and its cascades) actually dispatched -- empty if
    ``finished_multi_step_name`` triggers nothing. Each trigger branch is
    dispatched independently against the same ``config`` passed in (not
    threaded sequentially through each other's result) -- fan-out targets
    are independent resubmissions, matching legacy's own
    ``resubmit_recursively``, which never chains one target's output into
    the next. An entry means "dispatched", not necessarily "finished":
    ``run_in_process`` really has run by the time it returns, so its
    entry reflects real work done; ``launch_local``/``launch_batch``
    return ``config`` unchanged, since launching a process or submitting
    to a scheduler is fire-and-forget -- their entry means "launched".
    """
    results: dict = {}
    for next_name in workflow.resubmit_triggers.get(finished_multi_step_name, []):
        results.update(_cascade(workflow, next_name, config, caller="advance"))
    return results
