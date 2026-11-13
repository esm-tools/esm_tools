"""Starting a :class:`~esm_runscripts.pplan.plan.MultiStep`'s runscript as
a new OS process -- directly (``LOCAL``) or via the scheduler
(``BATCH``). Replaces legacy's ``resubmit_batch_or_shell`` +
``resubmit.submit``, which wrote the runscript and shelled out via bare
``os.system`` with no error handling.
"""

from __future__ import annotations

import os
import subprocess

from ..pplan.plan import Workflow
from ..runscript.batch_system import by_name as batch_system_by_name
from ..runscript.generation import generate_runscript, get_run_filename, write_runscript


def launch_local(workflow: "Workflow", multi_step_name: str, config: dict) -> dict:
    """Write ``multi_step_name``'s runscript and run it directly, no
    scheduler."""
    context = generate_runscript(workflow, multi_step_name, config)
    path = get_run_filename(config, multi_step_name)
    write_runscript(context, path, executable=True)

    subprocess.run([path], cwd=config["general"]["thisrun_scripts_dir"], check=True)
    return config


def launch_batch(workflow: "Workflow", multi_step_name: str, config: dict) -> dict:
    """Write ``multi_step_name``'s runscript and submit it to the
    scheduler named by ``computer.batch_system``."""
    context = generate_runscript(workflow, multi_step_name, config)
    path = get_run_filename(config, multi_step_name)
    write_runscript(context, path, executable=False)

    batch_system_name = config["computer"]["batch_system"]
    batch_system_by_name(batch_system_name)  # validates the name eagerly
    submit = config["computer"]["submit"]
    subprocess.run(
        [submit, os.path.basename(path)],
        cwd=config["general"]["thisrun_scripts_dir"],
        check=True,
    )
    return config
