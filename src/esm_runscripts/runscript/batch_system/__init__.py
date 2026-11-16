"""The real :class:`BatchSystem` protocol and its implementations.

Replaces the legacy ``batch_system.py``/``slurm.py``/``pbs.py`` trio: a
lowercase ``batch_system`` class whose ~20 methods were mostly
``@staticmethod``\\ s with no reason to be methods at all, delegating to
``self.bs`` (a ``Slurm``/``Pbs`` instance) for the genuinely
scheduler-specific handful. Here that duck-typed delegation is made
explicit as a :class:`typing.Protocol`, with two real implementations.

Deliberately NOT ported from the legacy implementations (flagged, not
silently dropped):

* The MPMD hostfile launcher (legacy ``Slurm.prepare_launcher``'s
  "standard" branch, ``srun --multi-prog`` with a per-component core-range
  hostfile) -- a real, load-bearing mechanism for coupled runs, not dead
  code. Needs its own design pass; :class:`BatchSystem` implementations
  here only support the single-executable-per-job case for now.
* ``multi_srun`` and the heterogeneous-parallelization/``taskset`` code
  paths -- confirmed dead/broken (see PR discussion): ``multi_srun``'s own
  template-building function is unreachable (calls ``self.bs...`` from a
  ``@staticmethod``) and contains obvious placeholder garbage; the
  het-par/taskset paths are themselves marked
  ``# TODO: remove it once it's not needed anymore (substituted by packjob)``
  in the legacy source.
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

    def tasks_or_nodes_flag(self) -> str:
        """Which ``computer.<flag>`` key selects this scheduler's
        tasks-per-job header flag -- ``"tasks_flag"`` for schedulers that
        take a task count directly, ``"nodes_flag"`` for ones (PBS) that
        only take a node count."""
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
