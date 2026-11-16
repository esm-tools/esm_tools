"""Native runscript generation.

Replaces legacy's ``batch_system.write_simple_runscript`` -- one ~175-line
function that wrote header, resource calculation (as a side effect),
per-step commands, and the self-resubmission CLI re-entry call all in one
pass, with three separate, pure(r) steps:

* :func:`build_runscript_context` -- builds a :class:`RunscriptContext`
  (plain data, nothing written yet).
* :func:`render` -- turns that context into the script text, via Jinja2
  (``templates/runscript.sh.j2``) rather than legacy's ``@tag@``
  ``str.replace`` chains.
* :func:`write_runscript` -- writes the rendered text to disk and chmods
  it executable for the shell case.

Deliberately NOT covered yet (see ``batch_system/__init__.py`` for why):
the MPMD ``srun --multi-prog`` hostfile launcher -- today's
:class:`MultiStep`/:class:`Plan` always run one executable per job. In-script
self-resubmission *is* covered -- see :func:`observe_call_line` and
:mod:`esm_runscripts.execution.observe`.
"""

from __future__ import annotations

import math
import stat
from pathlib import Path
from typing import List, Optional

import esm_environment
from esm_parser.pydantic_models import ProvenanceFreeModel
from jinja2 import Environment, FileSystemLoader, StrictUndefined, select_autoescape
from pydantic import ConfigDict

from .. import dataprocess
from ..pplan.plan import Executor, LaunchMode, MultiStepKind, Workflow
from .batch_system import by_name as batch_system_by_name

_TEMPLATE_DIR = Path(__file__).parent / "templates"
_ENV = Environment(
    loader=FileSystemLoader(_TEMPLATE_DIR),
    autoescape=select_autoescape(enabled_extensions=()),
    trim_blocks=True,
    lstrip_blocks=True,
)

# A separate, string-only Jinja environment for rendering one-line
# computer.<flag> values (e.g. slurm.yaml's tasks_flag: "--ntasks={{
# tasks }}") -- no loader/templates dir needed, just .from_string().
# StrictUndefined so a flag referencing a name outside _HEADER_FLAG_VARS
# (a typo, or a tag this code doesn't know about) raises immediately
# naming it, rather than silently rendering as empty.
_FLAG_ENV = Environment(undefined=StrictUndefined)

_HEADER_FLAG_VARS = ("tasks", "nodes", "partition", "jobtype")
"""The only variables a computer.<flag> value may reference inside
``{{ }}`` -- see :func:`batch_header_lines`. Documented here and in a
comment atop ``configs/other_software/batch_system/slurm.yaml``/
``pbs.yaml``, the two files that actually use them."""


class ResourceRequirements(ProvenanceFreeModel):
    """How many tasks/nodes a :class:`~esm_runscripts.pplan.plan.MultiStep`
    needs. Pure result, unlike legacy's ``calculate_requirements``, which
    computed the same numbers as a side effect of mutating ``config``."""

    model_config = ConfigDict(frozen=True)

    tasks: int
    nodes: int


class RunscriptContext(ProvenanceFreeModel):
    """Everything :func:`render` needs to produce the script text. Plain
    data -- no ``config`` dict leaks into the template."""

    model_config = ConfigDict(frozen=True)

    interpreter: Optional[str] = None
    header_lines: List[str] = []
    env_lines: List[str] = []
    extra_lines: List[str] = []
    command_lines: List[str] = []
    is_batch: bool = False


def _cores_per_node(config: dict, queue: Optional[str]) -> int:
    partitions = config["computer"]["partitions"]
    if queue is None:
        # No queue named -- fall back to whichever partition exists,
        # matching legacy's own assumption that every computer defines at
        # least "compute" or "pp".
        return next(iter(partitions.values()))["cores_per_node"]
    if queue not in partitions:
        raise ValueError(
            f"run_on_queue={queue!r} names a partition that doesn't exist "
            f"in computer.partitions (known: {sorted(partitions)})."
        )
    return partitions[queue]["cores_per_node"]


def _multi_step_execution(workflow: Workflow, multi_step_name: str):
    if multi_step_name not in workflow.multi_step_execution:
        raise KeyError(
            f"No MultiStepExecution found for {multi_step_name!r} -- known: "
            f"{sorted(workflow.multi_step_execution)}."
        )
    return workflow.multi_step_execution[multi_step_name]


def compute_requirements(workflow: Workflow, multi_step_name: str, config: dict) -> ResourceRequirements:
    """How many tasks/nodes ``multi_step_name`` needs.

    For an :attr:`MultiStepKind.ESM_MULTISTEP` step (a Python recipe --
    ``prepcompute``/``compute``/``tidy``/...), aggregates per-model
    ``nproc``/``nproca``*``nprocb``(+``nprocar``*``nprocbr``)/``nprocio``
    across ``general.valid_model_names``, matching legacy's
    ``calculate_requirements``. For a :attr:`MultiStepKind.BASH` step,
    reads the already-known ``MultiStepExecution.nproc`` directly instead
    of re-deriving it from the legacy workflow dict.
    """
    execution = _multi_step_execution(workflow, multi_step_name)
    parent_execution = workflow.execution_of(multi_step_name)
    cores_per_node = _cores_per_node(config, parent_execution.run_on_queue)

    if execution.kind != MultiStepKind.ESM_MULTISTEP:
        tasks = execution.nproc
        nodes = math.ceil(tasks / cores_per_node) if tasks else 0
        return ResourceRequirements(tasks=tasks, nodes=nodes)

    tasks = 0
    nodes = 0
    for model in config["general"]["valid_model_names"]:
        model_conf = config[model]
        omp_num_threads = int(model_conf.get("omp_num_threads", 1))

        if "nproc" in model_conf:
            model_tasks = model_conf["nproc"]
        elif "nproca" in model_conf and "nprocb" in model_conf:
            model_tasks = model_conf["nproca"] * model_conf["nprocb"]
            remove_values = ("remove_from_namelist", 0)
            if (
                model_conf.get("nprocar", 0) not in remove_values
                and model_conf.get("nprocbr", 0) not in remove_values
            ):
                model_tasks += model_conf["nprocar"] * model_conf["nprocbr"]
        else:
            continue

        model_tasks += model_conf.get("nprocio", 0)
        tasks += model_tasks
        nodes += math.ceil(model_tasks * omp_num_threads / cores_per_node)

    return ResourceRequirements(tasks=tasks, nodes=nodes)


def shell_header_lines(config: dict) -> List[str]:
    lines = []
    sh_interpreter = config["computer"].get("sh_interpreter")
    if sh_interpreter:
        lines.append(f"#!{sh_interpreter}")
    return lines


def batch_header_lines(
    workflow: Workflow, multi_step_name: str, requirements: ResourceRequirements, config: dict
) -> List[str]:
    """Scheduler header lines, reading ``computer.<flag>`` entries as
    Jinja templates -- e.g. ``slurm.yaml``'s
    ``tasks_flag: "--ntasks={{ tasks }}"`` -- with exactly the variables
    in :data:`_HEADER_FLAG_VARS` available."""
    computer = config["computer"]
    batch_system = batch_system_by_name(computer["batch_system"])

    parent_execution = workflow.execution_of(multi_step_name)
    partitions = computer["partitions"]
    partition_name = parent_execution.run_on_queue or next(iter(partitions))
    partition = partitions[partition_name]["name"]

    flag_vars = {
        "tasks": requirements.tasks,
        "nodes": requirements.nodes,
        "partition": partition,
        "jobtype": multi_step_name,
    }

    flags = ["partition_flag", "time_flag", batch_system.tasks_or_nodes_flag(), "output_flags", "name_flag"]
    for conditional in ("accounting_flag", "notification_flag", "hyperthreading_flag", "additional_flags", "overcommit_flag"):
        if conditional not in computer:
            continue
        value = computer[conditional]
        values = value if isinstance(value, list) else [value]
        if not any(v.strip() == "" for v in values):
            flags.append(conditional)

    execution = _multi_step_execution(workflow, multi_step_name)
    if execution.kind == MultiStepKind.ESM_MULTISTEP and multi_step_name in ("prepcompute", "tidy"):
        flags.append("exclusive_flag")

    values: List[str] = []
    for flag in flags:
        value = computer.get(flag)
        if value is None:
            continue
        values.extend(value if isinstance(value, list) else [value])

    header_start = computer.get("header_start")
    lines = shell_header_lines(config)
    if header_start is None:
        return lines
    for value in values:
        rendered = _FLAG_ENV.from_string(value).render(**flag_vars)
        lines.append(f"{header_start} {rendered}")
    return lines


def environment_lines(config: dict, multi_step_name: str) -> List[str]:
    env = esm_environment.EnvironmentInfos(config, "run")
    lines = list(env.commands)
    if multi_step_name.replace("_general", "") not in ("prepcompute", "compute", "prepare", "tidy", "inspect"):
        lines += dataprocess.sub_plan_environment(config, multi_step_name)
    lines.append("")
    return lines


def extra_lines(config: dict) -> List[str]:
    lines = []
    if config["general"].get("unlimited_stack_size", True):
        lines += ["# Set stack size to unlimited", "ulimit -s unlimited"]
    if config["general"].get("use_venv", False):
        lines += [
            "# Start everything in a venv",
            "source " + config["general"]["experiment_dir"] + "/.venv_esmtools/bin/activate",
        ]
    return lines


def observe_log_path(config: dict, multi_step_name: str) -> Path:
    general = config["general"]
    filename = (
        f'{general["expid"]}_{general["setup_name"]}_'
        f'{multi_step_name}_{general["jobid"]}.log'
    )
    return Path(general["thisrun_log_dir"]) / filename


def observe_call_line(config: dict, multi_step_name: str) -> str:
    """The CLI re-entry line a backgrounded command's runscript appends
    after it: a new ``esm_runscripts`` process that blocks on
    ``${process}`` (the PID just captured) and watches for crashes --
    see :mod:`esm_runscripts.execution.observe`. Legacy's equivalent is
    ``batch_system.write_simple_runscript``'s inline ``observe_call``
    construction."""
    general = config["general"]
    rundate = general["current_date"].format(form=9, givenph=False, givenpm=False, givenps=False)
    return (
        f'esm_runscripts {general["scriptname"]} -e {general["expid"]} '
        f"--wf-engine=pplan -t observe_{multi_step_name} -p ${{process}} "
        f'-s {rundate} -r {general["run_number"]}'
    )


def command_lines(workflow: Workflow, multi_step_name: str, config: dict, is_batch: bool) -> List[str]:
    execution = _multi_step_execution(workflow, multi_step_name)
    if execution.kind != MultiStepKind.ESM_MULTISTEP:
        batch_or_shell = "batch" if is_batch else "shell"
        return dataprocess.sub_plan_tasks(config, multi_step_name, batch_or_shell)

    if multi_step_name != "compute":
        return []

    computer = config["computer"]
    if "execution_command" not in computer:
        return []

    write_log = computer.get("write_execution_log", "")
    command = f"time {computer['execution_command']} 2>&1{write_log}"

    if execution.launch_mode != LaunchMode.BACKGROUND:
        return [command]

    return [f"{command} &", "process=$!", observe_call_line(config, multi_step_name)]


def build_runscript_context(workflow: Workflow, multi_step_name: str, config: dict) -> RunscriptContext:
    """Build the :class:`RunscriptContext` for ``multi_step_name`` -- the
    data :func:`render` needs, nothing written to disk yet."""
    parent_execution = workflow.execution_of(multi_step_name)
    is_batch = parent_execution.executor == Executor.BATCH

    if is_batch:
        requirements = compute_requirements(workflow, multi_step_name, config)
        header = batch_header_lines(workflow, multi_step_name, requirements, config)
    else:
        header = shell_header_lines(config)

    return RunscriptContext(
        interpreter=config["computer"].get("sh_interpreter"),
        header_lines=header,
        env_lines=environment_lines(config, multi_step_name),
        extra_lines=extra_lines(config),
        command_lines=command_lines(workflow, multi_step_name, config, is_batch),
        is_batch=is_batch,
    )


def render(context: RunscriptContext) -> str:
    template = _ENV.get_template("runscript.sh.j2")
    return template.render(context=context)


def runscript_path(config: dict, multi_step_name: str) -> Path:
    folder = config["general"]["thisrun_scripts_dir"]
    expid = config["general"]["expid"]
    run_datestamp = config["general"]["run_datestamp"]
    return Path(folder) / f"{expid}_{multi_step_name}_{run_datestamp}.run"


def write_runscript(context: RunscriptContext, path: Path, executable: bool = False) -> None:
    path.write_text(render(context))
    if executable:
        path.chmod(path.stat().st_mode | stat.S_IEXEC)
