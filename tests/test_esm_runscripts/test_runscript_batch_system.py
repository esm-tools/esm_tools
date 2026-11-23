"""Tests for the native BatchSystem Protocol's MPMD hostfile support."""

from esm_runscripts.runscript.batch_system.pbs import PbsBatchSystem
from esm_runscripts.runscript.batch_system.slurm import SlurmBatchSystem


def _config(tmp_path):
    scripts_dir = tmp_path / "scripts"
    work_dir = tmp_path / "work"
    scripts_dir.mkdir()
    work_dir.mkdir()
    return {
        "general": {
            "valid_model_names": ["oifs", "fesom", "oasis3mct"],
            "thisrun_scripts_dir": str(scripts_dir),
            "thisrun_work_dir": str(work_dir),
        },
        "oifs": {
            "nproc": 12,
            "execution_command": "oifs_exe",
            "start_proc": 0,
            "end_proc": 11,
        },
        "fesom": {
            "nproc": 128,
            "executable": "fesom_exe",
            "start_proc": 12,
            "end_proc": 139,
        },
        "oasis3mct": {},
        "computer": {"launcher": "srun", "config_files": {"hostfile": "hostfile_srun"}},
    }


def test_slurm_writes_hostfile_to_scripts_and_work_dir(tmp_path):
    config = _config(tmp_path)

    SlurmBatchSystem.write_multiprog_hostfile(config)

    expected = "0-11  ./oifs_exe\n12-139  ./fesom_exe\n"
    scripts_hostfile = tmp_path / "scripts" / "hostfile_srun"
    work_hostfile = tmp_path / "work" / "hostfile_srun"
    assert scripts_hostfile.read_text() == expected
    assert work_hostfile.read_text() == expected


def test_slurm_writes_nothing_when_no_model_has_ranks(tmp_path):
    config = _config(tmp_path)
    for model in ("oifs", "fesom"):
        del config[model]["start_proc"]
        del config[model]["end_proc"]

    SlurmBatchSystem.write_multiprog_hostfile(config)

    assert not (tmp_path / "scripts" / "hostfile_srun").exists()
    assert not (tmp_path / "work" / "hostfile_srun").exists()


def test_pbs_write_multiprog_hostfile_is_a_noop(tmp_path):
    config = _config(tmp_path)

    PbsBatchSystem.write_multiprog_hostfile(config)

    assert not (tmp_path / "scripts" / "hostfile_srun").exists()
    assert not (tmp_path / "work" / "hostfile_srun").exists()


def test_tasks_or_nodes_flag_depends_on_heterogeneous_parallelization():
    assert SlurmBatchSystem.tasks_or_nodes_flag({"computer": {}}) == "tasks_flag"
    assert (
        SlurmBatchSystem.tasks_or_nodes_flag(
            {"computer": {"heterogeneous_parallelization": True}}
        )
        == "nodes_flag"
    )


def test_pbs_tasks_or_nodes_flag_is_always_nodes_flag():
    assert PbsBatchSystem.tasks_or_nodes_flag({"computer": {}}) == "nodes_flag"
    assert (
        PbsBatchSystem.tasks_or_nodes_flag(
            {"computer": {"heterogeneous_parallelization": True}}
        )
        == "nodes_flag"
    )


def _heterogeneous_config(tmp_path):
    config = _config(tmp_path)
    config["general"]["valid_model_names"] = ["oifs", "fesom", "oasis3mct"]
    config["oifs"]["omp_num_threads"] = 2
    config["fesom"]["omp_num_threads"] = 1
    config["computer"]["heterogeneous_parallelization"] = True
    config["computer"]["taskset"] = True
    config["computer"]["partitions"] = {"compute": {"cores_per_node": 128}}
    return config


def test_slurm_write_taskset_wrappers_writes_script_and_prog_per_model(tmp_path):
    config = _heterogeneous_config(tmp_path)

    SlurmBatchSystem.write_taskset_wrappers(config)

    script = (tmp_path / "work" / "script_oifs.sh").read_text()
    assert "export OMP_NUM_THREADS=2" in script
    assert "export OASIS_OMP_NUM_THREADS=2" in script
    assert "./oifs_exe" in script

    prog = (tmp_path / "work" / "prog_oifs.sh").read_text()
    assert "PMI_RANK" in prog
    assert "index = init * 2" in prog
    assert "slot = index % 128" in prog
    assert "taskset -c $slot-$((slot + 2 - 1)) ./script_oifs.sh" in prog

    assert (tmp_path / "work" / "script_oifs.sh").stat().st_mode & 0o100
    assert (tmp_path / "work" / "prog_oifs.sh").stat().st_mode & 0o100

    assert config["oifs"]["execution_command_het_par"] == "prog_oifs.sh"
    assert config["fesom"]["execution_command_het_par"] == "prog_fesom.sh"
    # oasis3mct is a library -- no wrapper, no key set.
    assert "execution_command_het_par" not in config["oasis3mct"]


def test_slurm_write_taskset_wrappers_is_a_noop_without_both_flags(tmp_path):
    config = _heterogeneous_config(tmp_path)
    config["computer"]["taskset"] = False

    SlurmBatchSystem.write_taskset_wrappers(config)

    assert not (tmp_path / "work" / "script_oifs.sh").exists()
    assert "execution_command_het_par" not in config["oifs"]


def test_pbs_write_taskset_wrappers_is_a_noop(tmp_path):
    config = _heterogeneous_config(tmp_path)

    PbsBatchSystem.write_taskset_wrappers(config)

    assert not (tmp_path / "work" / "script_oifs.sh").exists()


def test_write_multiprog_hostfile_prefers_execution_command_het_par(tmp_path):
    config = _heterogeneous_config(tmp_path)
    config["oifs"]["execution_command_het_par"] = "prog_oifs.sh"
    config["fesom"]["execution_command_het_par"] = "prog_fesom.sh"

    SlurmBatchSystem.write_multiprog_hostfile(config)

    content = (tmp_path / "work" / "hostfile_srun").read_text()
    assert content == "0-11  ./prog_oifs.sh\n12-139  ./prog_fesom.sh\n"
