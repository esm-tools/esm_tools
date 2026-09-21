"""Render the SLURM + Dask + Singularity job-array pipeline for a large scan.

``esm-catalog scan --distributed --scheduler tcp://...`` attaches to an
*already-running* Dask scheduler; it says nothing about how that scheduler
and its workers got there. This module renders the four SLURM scripts that
bootstrap one: ``sched.sbatch`` starts the scheduler (readiness-gated -- it
waits for the scheduler to announce itself before publishing its address, so
workers never race the port binding), ``worker.sbatch`` is a job array that
joins it, ``driver.sbatch`` waits for the scheduler's address then runs the
actual ``scan`` (optionally ``push`` too), and ``cleanup.sbatch`` tears the
scheduler and workers down once the driver finishes (success or failure) --
submitted with ``--dependency=afterany:$DRIVER_JOBID`` so it fires regardless
of how the driver ended, rather than leaving the array to idle out its own
time limit.

Validated live at 3000 workers / 32,200 files on Albedo (AWI); the template
values (``smp`` partition, ``/albedo`` bind path) default to that environment
but are all overridable. Container runtime defaults to ``singularity`` --
Apptainer is a drop-in-compatible fork (identical CLI), so ``container_bin``/
``container_module`` can be set to ``apptainer`` on a site that uses that
name instead.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

from jinja2 import Environment, PackageLoader

TEMPLATES = ("sched.sbatch.j2", "worker.sbatch.j2", "driver.sbatch.j2", "cleanup.sbatch.j2")

REQUIRED_VARS = (
    "job_prefix",
    "scratch_dir",
    "image_tag",
    "exp_root",
    "catalog_dir",
    "n_workers",
)

DEFAULT_BIND_PATHS = ("/albedo",)

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
n_workers: 3000

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


def _normalize(context: dict[str, Any]) -> dict[str, Any]:
    """Return *context* with the computed template fields filled in.

    ``bind_paths`` (a list, possibly a single string) becomes ``bind_flags``
    (one ``-B <path>`` per entry, pre-joined -- the templates just drop it in
    rather than looping over a list in Jinja). ``container_bin``/
    ``container_module`` default to ``singularity``; ``cache_env_var``
    follows whichever binary is in play (``SINGULARITY_CACHEDIR`` or
    ``APPTAINER_CACHEDIR``) since the two forks use different variable names
    for the same thing.
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
    return context


def render_scripts(context: dict[str, Any], out_dir: Path) -> list[Path]:
    """Render the four sbatch scripts into *out_dir*; return the written paths.

    Parameters
    ----------
    context : dict
        Template variables. Must contain every name in :data:`REQUIRED_VARS`;
        everything else has a default (partition, qos, walltime,
        ``bind_paths`` -> :data:`DEFAULT_BIND_PATHS`, ``container_bin``/
        ``container_module`` -> ``singularity``, push_after_scan,
        server_url) or is filled in by the caller (``log_dir``,
        conventionally ``esm_catalog.xdg.state_dir() / "logs"``).
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
        If *context* is missing any of :data:`REQUIRED_VARS`.
    """
    missing = [key for key in REQUIRED_VARS if key not in context]
    if missing:
        raise ValueError(f"missing required variable(s): {', '.join(missing)}")

    context = _normalize(context)

    out_dir.mkdir(parents=True, exist_ok=True)
    env = Environment(
        loader=PackageLoader("esm_catalog", "distributed/templates"),
        keep_trailing_newline=True,
    )
    written = []
    for name in TEMPLATES:
        rendered = env.get_template(name).render(**context)
        path = out_dir / name.removesuffix(".j2")
        path.write_text(rendered)
        path.chmod(0o755)
        written.append(path)
    return written
