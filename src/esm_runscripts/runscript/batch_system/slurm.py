"""SLURM :class:`~esm_runscripts.batch_system.BatchSystem` implementation."""

from __future__ import annotations

import os
import subprocess
from typing import Optional


class SlurmBatchSystem:
    name = "slurm"

    @staticmethod
    def check_if_submitted() -> bool:
        return "SLURM_JOB_ID" in os.environ

    @staticmethod
    def get_jobid() -> Optional[str]:
        return os.environ.get("SLURM_JOB_ID")

    @staticmethod
    def get_job_state(jobid: str) -> Optional[str]:
        """The job's state per ``squeue``'s ``%T`` format (see ``man
        squeue``, "JOB STATE CODES")."""
        result = subprocess.run(
            ["squeue", "-j", str(jobid), "-o", "%T", "-h"],
            capture_output=True,
            text=True,
        )
        state = result.stdout.strip()
        return state or None

    @classmethod
    def job_is_still_running(cls, jobid: str) -> bool:
        return bool(cls.get_job_state(jobid))

    @staticmethod
    def tasks_or_nodes_flag() -> str:
        return "tasks_flag"
