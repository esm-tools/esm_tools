"""Native execution for the pplan engine: dispatching a resolved
:class:`~esm_runscripts.pplan.plan.Workflow`'s work.

This is the "execution" half of the staged vocabulary-rename plan (see
esm-tools/esm_tools#1519): rename -> models -> REPL -> execution/provenance.
``plan.py`` stays the read-only model layer; this module is where deciding
*how* a sub-plan's work actually runs belongs.
"""

from . import plan as plan_module


def dispatch_in_process(workflow: "plan_module.Workflow", sub_plan_name: str, config: dict) -> dict:
    """Run ``sub_plan_name``'s steps directly, in the current process.

    Only valid for a sub-plan whose parent :class:`~plan.Plan` has
    ``executor == Executor.IN_PROCESS`` -- no new OS process, no runscript,
    just ``plan.run_steps()`` called right here. ``local``/``batch``
    executors spawn a process and are handled elsewhere.
    """
    sub_plan = workflow.sub_plans[sub_plan_name]
    parent = workflow.parent_plan_of(sub_plan_name)
    if parent.executor != plan_module.Executor.IN_PROCESS:
        raise ValueError(
            f"dispatch_in_process called for {sub_plan_name!r}, whose parent "
            f"plan {parent.name!r} has executor={parent.executor!r}, not "
            f"{plan_module.Executor.IN_PROCESS!r}."
        )
    return plan_module.run_steps(sub_plan, config)


def advance(workflow: "plan_module.Workflow", finished_sub_plan_name: str, config: dict) -> dict:
    """Dispatch whatever ``finished_sub_plan_name`` triggers next.

    Native replacement for legacy's ``resubmit_recursively``/
    ``maybe_resubmit`` (``resubmit.py``), reading
    ``Workflow.resubmit_triggers`` (the fan-out-capable native shape) in
    place of the legacy dict's per-plan ``next_submit`` list.

    Only ``Executor.IN_PROCESS`` targets are actually dispatched here (via
    :func:`dispatch_in_process`); a target whose parent ``Plan`` has any
    other executor raises ``NotImplementedError`` naming it, rather than
    silently doing nothing -- ``local``/``batch`` dispatch (runscript
    generation + submission) doesn't exist yet.

    Returns ``{next_sub_plan_name: resulting_config}`` for every target
    this call dispatched -- empty if ``finished_sub_plan_name`` triggers
    nothing. Each target is dispatched independently against the same
    ``config`` passed in (not threaded sequentially through each other's
    result) -- fan-out targets are independent resubmissions, matching
    legacy's own ``resubmit_recursively``, which never chains one target's
    output into the next.
    """
    results = {}
    for next_name in workflow.resubmit_triggers.get(finished_sub_plan_name, []):
        parent = workflow.parent_plan_of(next_name)
        if parent.executor != plan_module.Executor.IN_PROCESS:
            raise NotImplementedError(
                f"advance() can't dispatch {next_name!r} yet -- its parent "
                f"plan {parent.name!r} has executor={parent.executor!r}, and "
                "only Executor.IN_PROCESS dispatch exists so far."
            )
        results[next_name] = dispatch_in_process(workflow, next_name, config)
    return results
