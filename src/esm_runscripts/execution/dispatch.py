"""Picking the right execution strategy for a resolved
:class:`~esm_runscripts.pplan.plan.Workflow`'s work, per
:class:`~esm_runscripts.pplan.plan.Executor`.

This is the "execution" half of the staged vocabulary-rename plan (see
esm-tools/esm_tools#1519): rename -> models -> REPL -> execution/provenance.
``pplan.plan`` stays the read-only model layer; this module is where
deciding *how* a sub-plan's work actually runs belongs.
"""

from ..pplan import plan as plan_module
from . import launch


def run_in_process(workflow: "plan_module.Workflow", multi_step_name: str, config: dict) -> dict:
    """Run ``multi_step_name``'s steps directly, in the current process.

    Only valid for a sub-plan whose parent :class:`~plan.Plan` has
    ``executor == Executor.IN_PROCESS`` (in :class:`~plan.PlanExecution`)
    -- no new OS process, no runscript, just ``plan.run_steps()`` called
    right here. ``launch_local``/``launch_batch`` (:mod:`.launch`) spawn a
    process instead.
    """
    multi_step = workflow.multi_steps[multi_step_name]
    parent = workflow.parent_plan_of(multi_step_name)
    execution = workflow.execution_of(multi_step_name)
    if execution.executor != plan_module.Executor.IN_PROCESS:
        raise ValueError(
            f"run_in_process called for {multi_step_name!r}, whose parent "
            f"plan {parent.name!r} has executor={execution.executor!r}, not "
            f"{plan_module.Executor.IN_PROCESS!r}."
        )
    return plan_module.run_steps(multi_step, config)


# One strategy per Executor value. advance() looks a target's executor up
# here rather than branching on it directly.
_DISPATCHERS = {
    plan_module.Executor.IN_PROCESS: run_in_process,
    plan_module.Executor.LOCAL: launch.launch_local,
    plan_module.Executor.BATCH: launch.launch_batch,
}


def advance(workflow: "plan_module.Workflow", finished_multi_step_name: str, config: dict) -> dict:
    """Dispatch whatever ``finished_multi_step_name`` triggers next.

    Native replacement for legacy's ``resubmit_recursively``/
    ``maybe_resubmit`` (``resubmit.py``), reading
    ``Workflow.resubmit_triggers`` (the fan-out-capable native shape) in
    place of the legacy dict's per-plan ``next_submit`` list.

    Looks each target's parent ``Plan.executor`` up in ``_DISPATCHERS``; a
    target whose executor has no registered strategy raises
    ``NotImplementedError`` naming it, rather than silently doing nothing.

    Returns ``{next_multi_step_name: resulting_config}`` for every target
    this call dispatched -- empty if ``finished_multi_step_name`` triggers
    nothing. Each target is dispatched independently against the same
    ``config`` passed in (not threaded sequentially through each other's
    result) -- fan-out targets are independent resubmissions, matching
    legacy's own ``resubmit_recursively``, which never chains one target's
    output into the next.
    """
    results = {}
    for next_name in workflow.resubmit_triggers.get(finished_multi_step_name, []):
        parent = workflow.parent_plan_of(next_name)
        execution = workflow.execution_of(next_name)
        strategy = _DISPATCHERS.get(execution.executor)
        if strategy is None:
            raise NotImplementedError(
                f"advance() can't dispatch {next_name!r} yet -- its parent "
                f"plan {parent.name!r} has executor={execution.executor!r}, "
                f"and no strategy is registered for it (only "
                f"{[e.value for e in _DISPATCHERS]} so far)."
            )
        results[next_name] = strategy(workflow, next_name, config)
    return results
