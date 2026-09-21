"""Render the SLURM + Dask + Apptainer job-array pipeline for a large scan.

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
values (``smp`` partition, ``/albedo`` bind path, ``module load apptainer``)
default to that environment but are all overridable.
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


def render_scripts(context: dict[str, Any], out_dir: Path) -> list[Path]:
    """Render the four sbatch scripts into *out_dir*; return the written paths.

    Parameters
    ----------
    context : dict
        Template variables. Must contain every name in :data:`REQUIRED_VARS`;
        everything else has a default baked into the templates themselves
        (partition, qos, walltime, bind_path, apptainer_module,
        push_after_scan, server_url) or is filled in by the caller
        (``log_dir``, conventionally ``esm_catalog.xdg.state_dir() / "logs"``).
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
