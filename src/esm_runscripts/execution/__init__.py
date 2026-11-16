"""Native execution for the pplan engine: deciding *how* a resolved
:class:`~esm_runscripts.pplan.plan.Workflow`'s work actually runs.

``dispatch`` picks the right strategy per :class:`~esm_runscripts.pplan.plan.Executor`
(``run_in_process``, or ``launch``'s ``launch_local``/``launch_batch``).
Deliberately no wildcard re-exports -- import ``from .execution import
dispatch``/``from .execution import launch`` explicitly, matching
``pplan``'s own import convention.
"""
