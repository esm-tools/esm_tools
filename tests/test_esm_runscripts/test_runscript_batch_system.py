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
