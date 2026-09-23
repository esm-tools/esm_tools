"""Render the SLURM + Dask + Singularity job pipeline for a large scan.

``esm-catalog scan --distributed --scheduler tcp://...`` attaches to an
*already-running* Dask scheduler; it says nothing about how that scheduler
and its workers got there. This module renders the four SLURM scripts that
bootstrap one: ``sched.sbatch`` starts the scheduler (readiness-gated -- it
waits for the scheduler to announce itself before publishing its address, so
workers never race the port binding), ``worker.sbatch`` gets the workers
running (two modes, see below), ``driver.sbatch`` waits for the scheduler's
address then runs the actual ``scan`` (optionally ``push`` too), and
``cleanup.sbatch`` tears the scheduler and workers down once the driver
finishes (success or failure) -- submitted with
``--dependency=afterany:$DRIVER_JOBID`` so it fires regardless of how the
driver ended, rather than leaving the workers to idle out their own time
limit.

Two worker modes (``worker_mode``), because sites differ in how many
concurrently *running* jobs a user is allowed:

- ``"array"`` (default) -- a SLURM job array, one job per worker. Validated
  live at 3000 workers / 32,200 files on Albedo (AWI), which has no tight
  per-user running-job cap. Requires ``n_workers``.
- ``"multinode"`` -- one job, ``-N n_nodes``, ``srun`` fans
  ``n_nodes * cores_per_node`` worker tasks out across that single job's own
  allocation. For a site with a tight running-job cap (e.g. DKRZ Levante: 20
  concurrently running jobs in compute+shared combined -- a 3000-element
  array there would queue almost entirely, not run), this counts as exactly
  one running job no matter how many worker tasks start inside it. Requires
  ``n_nodes``; ``cores_per_node`` defaults to 128 (Levante's standard
  ``compute`` node).

Template values (``smp`` partition, ``/albedo`` bind path) default to Albedo
but are all overridable. Container runtime defaults to ``singularity`` --
Apptainer is a drop-in-compatible fork (identical CLI), so ``container_bin``/
``container_module`` can be set to ``apptainer`` on a site that uses that
name instead.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

from jinja2 import Environment, PackageLoader

SCHED_TEMPLATE = "sched.sbatch.j2"
DRIVER_TEMPLATE = "driver.sbatch.j2"
CLEANUP_TEMPLATE = "cleanup.sbatch.j2"

WORKER_TEMPLATE_BY_MODE = {
    "array": "worker.sbatch.j2",
    "multinode": "worker-multinode.sbatch.j2",
}

BASE_REQUIRED_VARS = (
    "job_prefix",
    "scratch_dir",
    "image_tag",
    "exp_root",
    "catalog_dir",
)

WORKER_MODE_REQUIRED_VARS = {
    "array": ("n_workers",),
    "multinode": ("n_nodes",),
}

DEFAULT_BIND_PATHS = ("/albedo",)
DEFAULT_CORES_PER_NODE = 128

DEFAULT_VARS_TEMPLATE = """\
# Variables for 'esm-catalog distributed render-scripts'. Fill in the
# CHANGE_ME values, then:
#   esm-catalog distributed render-scripts my-vars.yaml --out-dir ./rendered
#
# Any of these can also be passed as a --flag instead (overrides the file);
# see 'esm-catalog distributed render-scripts --help'.

job_prefix: catalog          # SLURM job names become <job_prefix>-sched / -worker / -driver / -cleanup
scratch_dir: /albedo/scratch/user/CHANGE_ME/tmp/esm-cat-scan   # coord/, container-cache/ live here
image_tag: CHANGE_ME         # e.g. v6.68.0-rc.1-test-0.1.11
exp_root: /albedo/work/projects/CHANGE_ME/esm_experiments/CHANGE_ME
catalog_dir: /albedo/scratch/user/CHANGE_ME/tmp/esm-cat-scan/catalog

# Pick a worker mode:
worker_mode: array           # one SLURM job per worker -- Albedo, no tight running-job cap
n_workers: 3000
# worker_mode: multinode      # one job, srun fans workers out inside it -- sites with a
# n_nodes: 4                 # tight per-user running-job cap (e.g. DKRZ Levante: 20 running)
# cores_per_node: 128         # workers per node in multinode mode (default: 128, Levante's compute node)

# Optional, shown with their defaults:
# partition: smp
# qos: 12h
# walltime: "04:00:00"
# bind_paths: [/albedo]        # repeat for multiple mounts
# container_bin: singularity   # or apptainer -- identical CLI, different binary name
# container_module: singularity  # defaults to whatever container_bin is
# push_after_scan: false
# server_url: https://stac-dev.awi.de
# log_dir: /custom/path/if/you/dont/want/$XDG_STATE_HOME/esm-catalog/logs
"""

_OPTIONAL_FIELD_COMMENTS = {
    "partition": "smp",
    "qos": "12h",
    "walltime": '"04:00:00"',
    "bind_paths": "[/albedo]        # repeat for multiple mounts",
    "container_bin": "singularity   # or apptainer -- identical CLI, different binary name",
    "container_module": "singularity  # defaults to whatever container_bin is",
    "push_after_scan": "false",
    "server_url": "https://stac-dev.awi.de",
    "log_dir": "/custom/path/if/you/dont/want/$XDG_STATE_HOME/esm-catalog/logs",
}


def render_vars_template(overrides: dict[str, Any] | None = None) -> str:
    """Render the ``--dump-vars-template`` skeleton, honoring any ``--flag`` overrides.

    Without *overrides* this is byte-identical to :data:`DEFAULT_VARS_TEMPLATE`.
    With overrides (the same dict the CLI builds from its ``--flag`` options),
    base identity fields substitute in, ``worker_mode`` selects which of the
    array/multinode blocks is active (uncommented) vs shown for reference
    (commented), and any overridden optional field is uncommented and filled
    in rather than left as a commented example -- so a dump that was told
    ``--worker-mode multinode --n-nodes 4`` actually produces a multinode
    vars file instead of silently discarding those flags.
    """
    overrides = overrides or {}

    job_prefix = overrides.get("job_prefix", "catalog")
    scratch_dir = overrides.get(
        "scratch_dir", "/albedo/scratch/user/CHANGE_ME/tmp/esm-cat-scan"
    )
    image_tag = overrides.get("image_tag", "CHANGE_ME")
    exp_root = overrides.get(
        "exp_root", "/albedo/work/projects/CHANGE_ME/esm_experiments/CHANGE_ME"
    )
    catalog_dir = overrides.get(
        "catalog_dir", "/albedo/scratch/user/CHANGE_ME/tmp/esm-cat-scan/catalog"
    )

    worker_mode = overrides.get("worker_mode", "array")
    n_workers = overrides.get("n_workers", 3000)
    n_nodes = overrides.get("n_nodes", 4)
    cores_per_node = overrides.get("cores_per_node", DEFAULT_CORES_PER_NODE)

    array_active = worker_mode != "multinode"
    array_prefix = "" if array_active else "# "
    multinode_prefix = "# " if array_active else ""
    worker_block = (
        f"{array_prefix}worker_mode: array           # one SLURM job per worker -- Albedo, no tight running-job cap\n"
        f"{array_prefix}n_workers: {n_workers}\n"
        f"{multinode_prefix}worker_mode: multinode      # one job, srun fans workers out inside it -- sites with a\n"
        f"{multinode_prefix}n_nodes: {n_nodes}                 # tight per-user running-job cap (e.g. DKRZ Levante: 20 running)\n"
        f"{multinode_prefix}cores_per_node: {cores_per_node}         # workers per node in multinode mode (default: 128, Levante's compute node)"
    )

    optional_lines = []
    for name, example in _OPTIONAL_FIELD_COMMENTS.items():
        if name in overrides:
            value = overrides[name]
            if name == "bind_paths":
                value = "[" + ", ".join(value) + "]"
            elif isinstance(value, bool):
                value = str(value).lower()
            optional_lines.append(f"{name}: {value}")
        else:
            optional_lines.append(f"# {name}: {example}")

    return (
        "# Variables for 'esm-catalog distributed render-scripts'. Fill in the\n"
        "# CHANGE_ME values, then:\n"
        "#   esm-catalog distributed render-scripts my-vars.yaml --out-dir ./rendered\n"
        "#\n"
        "# Any of these can also be passed as a --flag instead (overrides the file);\n"
        "# see 'esm-catalog distributed render-scripts --help'.\n"
        "\n"
        f"job_prefix: {job_prefix}          # SLURM job names become <job_prefix>-sched / -worker / -driver / -cleanup\n"
        f"scratch_dir: {scratch_dir}   # coord/, container-cache/ live here\n"
        f"image_tag: {image_tag}         # e.g. v6.68.0-rc.1-test-0.1.11\n"
        f"exp_root: {exp_root}\n"
        f"catalog_dir: {catalog_dir}\n"
        "\n"
        "# Pick a worker mode:\n"
        f"{worker_block}\n"
        "\n"
        "# Optional, shown with their defaults:\n"
        + "\n".join(optional_lines)
        + "\n"
    )


def _normalize(context: dict[str, Any]) -> dict[str, Any]:
    """Return *context* with the computed template fields filled in.

    ``bind_paths`` (a list, possibly a single string) becomes ``bind_flags``
    (one ``-B <path>`` per entry, pre-joined -- the templates just drop it in
    rather than looping over a list in Jinja). ``container_bin``/
    ``container_module`` default to ``singularity``; ``cache_env_var``
    follows whichever binary is in play (``SINGULARITY_CACHEDIR`` or
    ``APPTAINER_CACHEDIR``) since the two forks use different variable names
    for the same thing. ``cores_per_node`` (multinode mode only) defaults to
    :data:`DEFAULT_CORES_PER_NODE`.
    """
    context = dict(context)

    bind_paths = context.get("bind_paths") or DEFAULT_BIND_PATHS
    if isinstance(bind_paths, str):
        bind_paths = (bind_paths,)
    context["bind_flags"] = " ".join(f"-B {path}" for path in bind_paths)

    container_bin = context.setdefault("container_bin", "singularity")
    context.setdefault("container_module", container_bin)
    context.setdefault(
        "cache_env_var",
        "SINGULARITY_CACHEDIR" if container_bin == "singularity" else "APPTAINER_CACHEDIR",
    )
    context.setdefault("cores_per_node", DEFAULT_CORES_PER_NODE)
    return context


def render_scripts(context: dict[str, Any], out_dir: Path) -> list[Path]:
    """Render the four sbatch scripts into *out_dir*; return the written paths.

    Parameters
    ----------
    context : dict
        Template variables. Must contain every name in :data:`BASE_REQUIRED_VARS`
        plus whichever :data:`WORKER_MODE_REQUIRED_VARS` entry matches
        ``context.get("worker_mode", "array")``. Everything else has a
        default (partition, qos, walltime, ``bind_paths`` ->
        :data:`DEFAULT_BIND_PATHS`, ``container_bin``/``container_module`` ->
        ``singularity``, ``cores_per_node`` -> :data:`DEFAULT_CORES_PER_NODE`,
        push_after_scan, server_url) or is filled in by the caller
        (``log_dir``, conventionally
        ``esm_catalog.xdg.state_dir() / "logs"``).
    out_dir : Path
        Directory the four ``.sbatch`` files are written into (created if
        missing).

    Returns
    -------
    list of Path
        The four written script paths, in submission order (scheduler,
        worker, driver, cleanup).

    Raises
    ------
    ValueError
        If *context* is missing any required variable, or names an unknown
        ``worker_mode``.
    """
    worker_mode = context.get("worker_mode", "array")
    if worker_mode not in WORKER_TEMPLATE_BY_MODE:
        raise ValueError(
            f"unknown worker_mode {worker_mode!r}; expected one of "
            f"{sorted(WORKER_TEMPLATE_BY_MODE)}"
        )

    required = (*BASE_REQUIRED_VARS, *WORKER_MODE_REQUIRED_VARS[worker_mode])
    missing = [key for key in required if key not in context]
    if missing:
        raise ValueError(f"missing required variable(s): {', '.join(missing)}")

    context = _normalize(context)

    out_dir.mkdir(parents=True, exist_ok=True)
    env = Environment(
        loader=PackageLoader("esm_catalog", "distributed/templates"),
        keep_trailing_newline=True,
    )
    templates = (
        SCHED_TEMPLATE,
        WORKER_TEMPLATE_BY_MODE[worker_mode],
        DRIVER_TEMPLATE,
        CLEANUP_TEMPLATE,
    )
    written = []
    for name in templates:
        rendered = env.get_template(name).render(**context)
        out_name = "worker.sbatch" if name.startswith("worker") else name.removesuffix(".j2")
        path = out_dir / out_name
        path.write_text(rendered)
        path.chmod(0o755)
        written.append(path)
    return written
