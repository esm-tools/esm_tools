"""SLURM :class:`~esm_runscripts.batch_system.BatchSystem` implementation."""

from __future__ import annotations

import os
import shutil
import subprocess
from pathlib import Path
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
    def tasks_or_nodes_flag(config: dict) -> str:
        return (
            "nodes_flag"
            if config["computer"].get("heterogeneous_parallelization", False)
            else "tasks_flag"
        )

    @staticmethod
    def write_multiprog_hostfile(config: dict) -> None:
        """Write the ``srun --multi-prog``/``mpirun`` hostfile both into
        ``thisrun_scripts_dir`` (a record, matching legacy's own write
        location) and ``thisrun_work_dir`` (where ``compute``'s command
        actually runs from, and where the launcher looks for it by its
        bare relative filename -- see ``generation.command_lines``'s
        ``cd``). Mirrors legacy's ``Slurm.prepare_launcher``'s "standard"
        branch + its ``shutil.copyfile`` into the work dir."""
        from ..generation import multiprog_hostfile_content

        content = multiprog_hostfile_content(config)
        if not content:
            return

        general = config["general"]
        filename = config["computer"].get("config_files", {}).get("hostfile", "hostfile_srun")

        scripts_path = Path(general["thisrun_scripts_dir"]) / filename
        scripts_path.write_text(content)

        work_path = Path(general["thisrun_work_dir"]) / filename
        shutil.copyfile(scripts_path, work_path)

    @staticmethod
    def write_taskset_wrappers(config: dict) -> None:
        """Write ``script_<model>.sh``/``prog_<model>.sh`` into
        ``thisrun_work_dir`` for every model with a command to run
        (skipping ``oasis3mct``, a library), and set each model's
        ``execution_command_het_par`` to its wrapper's filename.

        ``script_<model>.sh`` sets ``OMP_NUM_THREADS``/
        ``OASIS_OMP_NUM_THREADS`` then execs the model's real command.
        ``prog_<model>.sh`` is what the MPMD hostfile actually launches --
        it reads its own MPI rank (``PMI_RANK``/``PMIX_RANK``) at runtime
        and ``taskset``-pins itself to a core range computed from that
        rank, so co-located threads of the same rank share a core block
        rather than fighting over whatever cores happen to be free.
        Mirrors legacy's ``Slurm.write_het_par_wrappers`` exactly (down to
        the per-rank core math); a no-op unless ``computer.
        heterogeneous_parallelization`` and ``computer.taskset`` are both
        set. Must run after :func:`~esm_runscripts.runscript.generation.
        compute_requirements` (not for any value it reads, but because
        that's what decided ``heterogeneous_parallelization`` is really
        in play -- called from the same ``launch_batch`` step)."""
        computer = config["computer"]
        if not (
            computer.get("heterogeneous_parallelization", False)
            and computer.get("taskset", False)
        ):
            return

        from ..generation import _ENV, taskset_wrapper_entries

        cores_per_node = computer["partitions"]["compute"]["cores_per_node"]
        work_dir = Path(config["general"]["thisrun_work_dir"])
        script_template = _ENV.get_template("taskset_script.sh.j2")
        prog_template = _ENV.get_template("taskset_prog.sh.j2")

        for entry in taskset_wrapper_entries(config):
            model = entry["model"]

            script_path = work_dir / f"script_{model}.sh"
            script_path.write_text(script_template.render(**entry))
            script_path.chmod(0o755)

            prog_path = work_dir / f"prog_{model}.sh"
            prog_path.write_text(
                prog_template.render(cores_per_node=cores_per_node, **entry)
            )
            prog_path.chmod(0o755)

            config[model]["execution_command_het_par"] = prog_path.name
