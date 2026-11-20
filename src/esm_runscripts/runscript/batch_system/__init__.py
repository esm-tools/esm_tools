"""The real :class:`BatchSystem` protocol and its implementations.

Replaces the legacy ``batch_system.py``/``slurm.py``/``pbs.py`` trio: a
lowercase ``batch_system`` class whose ~20 methods were mostly
``@staticmethod``\\ s with no reason to be methods at all, delegating to
``self.bs`` (a ``Slurm``/``Pbs`` instance) for the genuinely
scheduler-specific handful. Here that duck-typed delegation is made
explicit as a :class:`typing.Protocol`, with two real implementations.

The MPMD hostfile launcher (``write_multiprog_hostfile``, legacy
``Slurm.prepare_launcher``'s "standard" branch) is ported -- a real,
load-bearing mechanism for coupled runs, not dead code. See
:mod:`esm_runscripts.runscript.generation`'s ``compute_requirements``/
``multiprog_hostfile_content`` for the per-model rank-range math and
hostfile-content building this method writes to disk; ``PbsBatchSystem``'s
own implementation is a no-op, matching legacy's ``Pbs`` (``"no hostfile
for PBS"`` -- all job info is in the launcher command itself).

The ``taskset`` heterogeneous-MPI+OMP mechanism (``write_taskset_wrappers``,
legacy's ``Slurm.write_het_par_wrappers``/``add_hostlist_file_gen_lines``)
is ALSO ported -- confirmed load-bearing, not dead, for at least one real
production config (``configs/setups/awicm3/awicm3.yaml`` pins
``taskset: true`` for ``name: levante`` specifically; only ``juwels`` has
moved to the newer ``packjob``/``hetjob`` approach there). ``tasks_or_
nodes_flag`` now takes ``config`` -- SLURM uses ``nodes_flag`` whenever
``computer.heterogeneous_parallelization`` is true (set by the shared
``prepare`` recipe's ``find_openmp`` step, from any model's
``omp_num_threads`` > 1), matching legacy's ``get_batch_header`` exactly;
PBS always uses ``nodes_flag`` regardless.

Still deliberately NOT ported from the legacy implementations (flagged,
not silently dropped):

* ``multi_srun`` -- confirmed dead/broken (see PR discussion): its own
  template-building function is unreachable (calls ``self.bs...`` from a
  ``@staticmethod``) and contains obvious placeholder garbage.
* The newer ``packjob``/``hetjob`` heterogeneous-parallelization path
  (``taskset: false``, ``Slurm.het_par_launcher_lines``/
  ``het_par_headers``) -- not yet exercised by any config this rewrite has
  been verified against (``juwels`` only); port when a real need for it
  shows up, rather than guessing at its correctness now.
* PBS's ``add_pre_launcher_lines`` (renaming the output stream via
  ``qalter`` once the job ID is known) -- not yet ported.
"""

from __future__ import annotations

from typing import Optional, Protocol, runtime_checkable


@runtime_checkable
class BatchSystem(Protocol):
    """What a scheduler backend must provide. Implemented by
    :class:`~.slurm.SlurmBatchSystem` and :class:`~.pbs.PbsBatchSystem`.
    """

    name: str

    def check_if_submitted(self) -> bool:
        """Whether the *current* shell is already running inside a job
        submitted by this scheduler (checks the scheduler's own job-id
        environment variable)."""
        ...

    def get_jobid(self) -> Optional[str]:
        """The current job's ID, if running inside one."""
        ...

    def get_job_state(self, jobid: str) -> Optional[str]:
        """The scheduler's short state string for ``jobid``."""
        ...

    def job_is_still_running(self, jobid: str) -> bool:
        """Whether ``jobid`` is still running or queued."""
        ...

    def tasks_or_nodes_flag(self, config: dict) -> str:
        """Which ``computer.<flag>`` key selects this scheduler's
        tasks-per-job header flag -- ``"tasks_flag"`` for a plain task
        count, ``"nodes_flag"`` for a node count. SLURM depends on
        ``config``: ``"nodes_flag"`` whenever ``computer.
        heterogeneous_parallelization`` is true, ``"tasks_flag"``
        otherwise. PBS always uses ``"nodes_flag"``, regardless of
        ``config``."""
        ...

    def write_multiprog_hostfile(self, config: dict) -> None:
        """Write this scheduler's MPMD hostfile for ``compute``'s coupled
        launch into ``general.thisrun_work_dir`` (where the job actually
        runs from -- see ``generation.command_lines``'s ``cd``), if this
        scheduler has one. No-op for schedulers without one (PBS). Must
        run after :func:`~esm_runscripts.runscript.generation.
        compute_requirements` has assigned each model's ``start_proc``/
        ``end_proc`` range, and after :func:`write_taskset_wrappers` (if
        ``computer.taskset`` is set) so any ``execution_command_het_par``
        override is already in place."""
        ...

    def write_taskset_wrappers(self, config: dict) -> None:
        """Write the per-model ``taskset``-wrapping shell scripts
        (``script_<model>.sh``, ``prog_<model>.sh``) into ``general.
        thisrun_work_dir`` that pin each MPI rank's threads to its own
        core range, and set ``config[model]["execution_command_het_par"]``
        to the wrapper's name -- :func:`write_multiprog_hostfile` then
        references that wrapper instead of the model's own executable. A
        no-op when ``computer.heterogeneous_parallelization`` or
        ``computer.taskset`` isn't set (including for schedulers --
        PBS -- that don't support this mechanism at all). Must run after
        :func:`~esm_runscripts.runscript.generation.compute_requirements`
        has assigned each model's ``start_core``/``end_core``."""
        ...


def by_name(name: str) -> type:
    """Look up a :class:`BatchSystem` implementation by
    ``computer.batch_system`` name (``"slurm"``/``"pbs"``)."""
    from .pbs import PbsBatchSystem
    from .slurm import SlurmBatchSystem

    known = {"slurm": SlurmBatchSystem, "pbs": PbsBatchSystem}
    if name not in known:
        raise UnknownBatchSystemError(name)
    return known[name]


class UnknownBatchSystemError(Exception):
    """Raised for a ``computer.batch_system`` name with no implementation."""
