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
