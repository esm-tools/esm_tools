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
from ..log_events import ExecutionEvent, jsonl_path as _jsonl_path
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
    # ``run_on_queue`` holds the *resolved* SLURM partition name (e.g.
    # "mpp" on albedo -- see Workflow.default()'s
    # ``computer.partitions.compute.name``), not the partitions dict's own
    # key ("compute"/"pp"). The two happen to coincide on levante/DKRZ
    # (partition name == key there), which is why a bare ``partitions[queue]``
    # lookup worked there but not on albedo.
    for info in partitions.values():
        if info.get("name") == queue:
            return info["cores_per_node"]
    raise ValueError(
        f"run_on_queue={queue!r} names a partition that doesn't exist "
        f"in computer.partitions (known names: "
        f"{sorted(info.get('name') for info in partitions.values())})."
    )


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
    ``calculate_requirements`` -- including that function's side effect:
    assigning each model a contiguous ``start_proc``/``end_proc`` MPI-rank
    range (and ``start_core``/``end_core``, ``tasks``, ``threads``) in
    ``config[model]``, in ``valid_model_names`` order. Those ranges are
    what an MPMD launch's hostfile (:func:`multiprog_hostfile_content`)
    maps each model's binary onto -- without this, a coupled run's
    hostfile would have no ranks to write. For a :attr:`MultiStepKind.BASH`
    step, reads the already-known ``MultiStepExecution.nproc`` directly
    instead of re-deriving it from the legacy workflow dict, and does not
    touch any model's config (single-executable, no MPMD ranks to assign).
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
    start_proc = 0
    start_core = 0
    for model in config["general"]["valid_model_names"]:
        model_conf = config[model]
        omp_num_threads = int(model_conf.get("omp_num_threads", 1))

        if "nproc" in model_conf:
            # kh 21.04.22: multi-group support -- (nproc * mpi_num_groups)
            # MPI processes to start a program multiple times (used for
            # FESOM-REcoM tracer loop parallelization).
            mpi_num_groups = model_conf.get("mpi_num_groups", 1)
            model_conf["nproc"] *= mpi_num_groups
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
            # A library component (e.g. oasis3mct) needs no cores of its
            # own -- no start_proc/end_proc assigned, so it's skipped when
            # building the MPMD hostfile too.
            continue

        model_tasks += model_conf.get("nprocio", 0)
        model_conf["tasks"] = model_tasks
        model_conf["threads"] = model_tasks * omp_num_threads
        tasks += model_tasks
        nodes += math.ceil(model_tasks * omp_num_threads / cores_per_node)

        end_proc = start_proc + model_tasks - 1
        end_core = start_core + model_conf["threads"] - 1
        model_conf["start_proc"] = start_proc
        model_conf["end_proc"] = end_proc
        model_conf["start_core"] = start_core
        model_conf["end_core"] = end_core
        start_proc = end_proc + 1
        start_core = end_core + 1

    return ResourceRequirements(tasks=tasks, nodes=nodes)


def multiprog_hostfile_entries(config: dict) -> List[dict]:
    """One entry per model :func:`compute_requirements` assigned a
    ``start_proc``/``end_proc`` range to -- the data
    ``hostfile_srun.j2`` renders into the MPMD hostfile content, for
    whichever launcher (``srun``/``mpirun``) ``computer.launcher`` names.
    A model with no range (a library component, e.g. ``oasis3mct``), or
    no command to run, contributes no entry.

    When ``computer.heterogeneous_parallelization`` is set, a model's
    ``execution_command_het_par`` (set by
    :func:`~esm_runscripts.runscript.batch_system.slurm.
    SlurmBatchSystem.write_taskset_wrappers` -- the ``taskset``-wrapping
    ``prog_<model>.sh``) takes priority over its own ``execution_command``/
    ``executable``, matching legacy's ``Slurm.write_one_hostfile``
    exactly: the hostfile launches the wrapper, not the model directly.
    """
    heterogeneous = config["computer"].get("heterogeneous_parallelization", False)
    entries = []
    for model in config["general"]["valid_model_names"]:
        model_conf = config[model]
        start_proc = model_conf.get("start_proc")
        end_proc = model_conf.get("end_proc")
        if start_proc is None or end_proc is None:
            continue

        if heterogeneous and model_conf.get("execution_command_het_par"):
            command = f'./{model_conf["execution_command_het_par"]}'
        elif "execution_command" in model_conf:
            command = f'./{model_conf["execution_command"]}'
        elif "executable" in model_conf:
            command = f'./{model_conf["executable"]}'
        else:
            continue

        entries.append(
            {
                "start": start_proc,
                "end": end_proc,
                "ncpus": end_proc - start_proc + 1,
                "command": command,
            }
        )
    return entries


def multiprog_hostfile_content(config: dict) -> str:
    """The MPMD hostfile content for ``compute``'s launcher
    (``srun --multi-prog hostfile_srun`` or ``mpirun $(cat hostfile_srun)``
    -- see ``configs/other_software/batch_system/slurm.yaml``), rendered
    from :func:`multiprog_hostfile_entries` via ``hostfile_srun.j2``
    (``srun``'s ``--multi-prog`` format is one ``"{start}-{end}
    ./command"`` line per model; ``mpirun``'s is `` -np {n} ./command``
    `` : ``-joined on one line) -- matching legacy's ``Slurm.
    write_one_hostfile``'s two formats, built here via the template this
    module's docstring says generation should use, not manual string
    concatenation. Must run after :func:`compute_requirements` has
    populated ``start_proc``/``end_proc`` on each model; empty if no
    model got a range.
    """
    entries = multiprog_hostfile_entries(config)
    if not entries:
        return ""
    launcher = config["computer"].get("launcher")
    template = _ENV.get_template("hostfile_srun.j2")
    return template.render(launcher=launcher, entries=entries)


def taskset_wrapper_entries(config: dict) -> List[dict]:
    """One entry per model the ``taskset`` mechanism wraps -- skips
    ``oasis3mct`` (a library, matching legacy's ``write_het_par_wrappers``
    exactly) and any model with no command to run."""
    entries = []
    for model in config["general"]["valid_model_names"]:
        if model == "oasis3mct":
            continue
        model_conf = config[model]
        if "execution_command" in model_conf:
            command = f'./{model_conf["execution_command"]}'
        elif "executable" in model_conf:
            command = f'./{model_conf["executable"]}'
        else:
            continue
        entries.append(
            {
                "model": model,
                "omp_num_threads": model_conf.get("omp_num_threads", 1),
                "command": command,
            }
        )
    return entries


def hostlist_gen_lines(config: dict) -> List[str]:
    """The runtime shell lines that build ``hostlist`` (and export it as
    ``SLURM_HOSTFILE``) for a ``taskset``-mode heterogeneous MPI+OMP job
    -- one hostname line per MPI rank, spaced by each model's
    ``omp_num_threads`` so co-located threads land on the same physical
    node. The rank -> node-index assignment is pure arithmetic on data
    already known at script-generation time (``mpi_tasks``/
    ``omp_num_threads``/``cores_per_node``), so it's computed once, here,
    in Python -- not recomputed by a shell loop every time the job runs.
    Only resolving that index to an actual hostname stays in shell
    (``hostlist_gen.sh.j2``): physical node assignment
    (``$SLURM_JOB_NODELIST``) isn't known until the job is actually
    running. Ported from legacy's ``Slurm.add_hostlist_file_gen_lines``
    (same per-rank math, same resulting ``hostlist`` content). Empty (no
    lines) unless ``computer.heterogeneous_parallelization`` and
    ``computer.taskset`` are both set.
    """
    computer = config["computer"]
    if not (computer.get("heterogeneous_parallelization", False) and computer.get("taskset", False)):
        return []

    entries = []
    for model in config["general"]["valid_model_names"]:
        model_conf = config[model]
        if model == "oasis3mct" or not (
            model_conf.get("execution_command") or model_conf.get("executable")
        ):
            continue
        if "nproca" in model_conf:
            mpi_tasks = model_conf["nproca"] * model_conf["nprocb"]
        else:
            mpi_tasks = model_conf["nproc"]
        entries.append(
            {
                "model": model,
                "mpi_tasks": mpi_tasks,
                "omp_num_threads": model_conf.get("omp_num_threads", 1),
            }
        )
    if not entries:
        return []

    cores_per_node = config["computer"]["partitions"]["compute"]["cores_per_node"]
    host_indices = []
    current_core = 0
    for entry in entries:
        for _ in range(entry["mpi_tasks"]):
            host_indices.append(current_core // cores_per_node)
            current_core += entry["omp_num_threads"]

    template = _ENV.get_template("hostlist_gen.sh.j2")
    rendered = template.render(
        thisrun_work_dir=config["general"]["thisrun_work_dir"],
        host_indices=host_indices,
    )
    return rendered.splitlines()


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

    flags = ["partition_flag", "time_flag", batch_system.tasks_or_nodes_flag(config), "output_flags", "name_flag"]
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
    # taskset's core-pinning math (prog_<model>.sh, hostlist_gen_lines)
    # computes each rank's CPU slot as rank % cores_per_node -- only valid
    # with exclusive node ownership, or another job's processes could
    # occupy the same core indices. Legacy's own equivalent check reads
    # config["general"]["jobtype"], which happens to still be "prepcompute"
    # (the job currently doing the writing) when compute's own header gets
    # built, so legacy gets --exclusive for compute as a side effect, not
    # a deliberate decision -- checking the real requirement here instead.
    if (
        multi_step_name == "compute"
        and computer.get("heterogeneous_parallelization", False)
        and computer.get("taskset", False)
    ):
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


def execution_started_line(config: dict, multi_step_name: str, executor: Executor) -> str:
    """One shell line emitting an :class:`~esm_runscripts.log_events.
    ExecutionEvent.STARTED` JSON event, right before the real command
    runs -- the only point that knows true wall-clock start, since
    Python returns immediately after backgrounding (fire-and-forget) and
    never itself observes the spawned process actually beginning.
    ``FINISHED``/``FAILED`` are emitted later, from Python, by
    :func:`~esm_runscripts.execution.observe.wait_and_observe`'s caller,
    once it detects the watched PID actually exiting."""
    general = config["general"]
    template = _ENV.get_template("execution_started.sh.j2")
    return template.render(
        event=ExecutionEvent.STARTED,
        expid=general.get("expid"),
        run_number=general.get("run_number"),
        current_date=general.get("current_date"),
        jobid=general.get("jobid"),
        multi_step_name=multi_step_name,
        executor=executor.value,
        jsonl_path=_jsonl_path(config),
    ).rstrip("\n")


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

    # cd into thisrun_work_dir -- matches legacy's write_simple_runscript,
    # which does this for every multi_step unconditionally (batch or shell).
    # The MPMD execution_command (srun --multi-prog hostfile_srun, see
    # multiprog_hostfile_content) names the hostfile by bare relative
    # filename, so the job has to actually be running from this directory
    # for the launcher to find it.
    lines = [f'cd {config["general"]["thisrun_work_dir"]}']
    # taskset-mode heterogeneous MPI+OMP jobs need SLURM_HOSTFILE/hostlist
    # built at runtime, from inside the allocation (scontrol show hostnames
    # needs $SLURM_JOB_NODELIST, only set once the job's actually running)
    # -- empty for anything else (see hostlist_gen_lines).
    lines += hostlist_gen_lines(config)
    executor = workflow.execution_of(multi_step_name).executor
    lines.append(execution_started_line(config, multi_step_name, executor))

    write_log = computer.get("write_execution_log", "")
    command = f"time {computer['execution_command']} 2>&1{write_log}"

    if execution.launch_mode != LaunchMode.BACKGROUND:
        return lines + [command]

    # observe_call_line's "esm_runscripts {scriptname}" resolves scriptname
    # relative to cwd -- the runscript yaml lives in thisrun_scripts_dir,
    # not thisrun_work_dir (cd'd into above for the actual compute
    # command). Without this, the observe re-entry fails immediately
    # ("runscript not found"), silently dropping the rest of the cascade
    # (tidy, next-cycle resubmission) -- matches legacy's own
    # write_simple_runscript, which cd's back to scripts_dir right before
    # its equivalent resubmission call.
    return lines + [
        f"{command} &",
        "process=$!",
        f'cd {config["general"]["thisrun_scripts_dir"]}',
        observe_call_line(config, multi_step_name),
    ]


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
