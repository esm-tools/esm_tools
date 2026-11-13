"""Starting a :class:`~esm_runscripts.pplan.plan.MultiStep`'s runscript as
a new OS process -- directly (``LOCAL``) or via the scheduler
(``BATCH``). Replaces legacy's ``resubmit_batch_or_shell`` +
``resubmit.submit``, which wrote the runscript and shelled out via bare
``os.system`` with no error handling.
"""

from __future__ import annotations

import subprocess

from ..pplan.plan import Workflow
from ..runscript.batch_system import by_name as batch_system_by_name
from ..runscript.generation import build_runscript_context, runscript_path, write_runscript


def launch_local(workflow: "Workflow", multi_step_name: str, config: dict) -> dict:
    """Write ``multi_step_name``'s runscript and run it directly, no
    scheduler. Returns ``config`` unchanged -- launching is fire-and-forget
    here too (see :func:`launch_batch`); any effect the run itself has on
    ``config`` happens in the spawned process, not this one."""
    context = build_runscript_context(workflow, multi_step_name, config)
    path = runscript_path(config, multi_step_name)
    write_runscript(context, path, executable=True)

    subprocess.run([path], cwd=config["general"]["thisrun_scripts_dir"], check=True)
    return config


def launch_batch(workflow: "Workflow", multi_step_name: str, config: dict) -> dict:
    """Write ``multi_step_name``'s runscript and submit it to the
    scheduler named by ``computer.batch_system``. Returns ``config``
    unchanged: submission is fire-and-forget -- the scheduler queues the
    job and this call returns immediately, well before anything the job
    itself would do to ``config`` has happened. Unlike
    :func:`~esm_runscripts.execution.dispatch.run_in_process`, whose
    return value reflects real work already done, a ``LOCAL``/``BATCH``
    result in :func:`~esm_runscripts.execution.dispatch.advance`'s output
    dict means "launched", not "finished"."""
    context = build_runscript_context(workflow, multi_step_name, config)
    path = runscript_path(config, multi_step_name)
    write_runscript(context, path, executable=False)

    batch_system_name = config["computer"]["batch_system"]
    batch_system_by_name(batch_system_name)  # validates the name eagerly
    submit = config["computer"]["submit"]
    subprocess.run(
        [submit, path.name],
        cwd=config["general"]["thisrun_scripts_dir"],
        check=True,
    )
    return config
