"""PBS :class:`~esm_runscripts.batch_system.BatchSystem` implementation."""

from __future__ import annotations

import os
import subprocess
from typing import Optional


class PbsBatchSystem:
    name = "pbs"

    @staticmethod
    def check_if_submitted() -> bool:
        return "PBS_JOBID" in os.environ

    @staticmethod
    def get_jobid() -> Optional[str]:
        return os.environ.get("PBS_JOBID")

    @staticmethod
    def get_job_state(jobid: str) -> Optional[str]:
        """The job's short state per ``qstat`` (see ``man qstat``)."""
        result = subprocess.run(
            ["qstat", str(jobid)], capture_output=True, text=True
        )
        fields = result.stdout.split()
        return fields[-3] if len(fields) > 2 else None

    @classmethod
    def job_is_still_running(cls, jobid: str) -> bool:
        return bool(cls.get_job_state(jobid))

    @staticmethod
    def tasks_or_nodes_flag(config: dict) -> str:
        return "nodes_flag"

    @staticmethod
    def write_multiprog_hostfile(config: dict) -> None:
        """No-op -- PBS has no hostfile mechanism; all job info lives in
        the launcher command itself (matches legacy's ``Pbs`` class)."""
        return None

    @staticmethod
    def write_taskset_wrappers(config: dict) -> None:
        """No-op -- the ``taskset`` mechanism is SLURM-specific (legacy's
        ``Pbs`` has no equivalent at all)."""
        return None
