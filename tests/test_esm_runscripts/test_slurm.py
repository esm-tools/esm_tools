from esm_runscripts.slurm import Slurm


CONFIG = {
    "general": {"valid_model_names": ["fesom", "echam"]},
    "computer": {"heterogeneous_parallelization": False, "launcher": "srun"},
    "fesom": {"start_proc": 0, "end_proc": 3, "execution_command": "fesom.x"},
    "echam": {"start_proc": 4, "end_proc": 7, "execution_command": "echam6"},
}


def make_slurm(tmp_path):
    return Slurm({"general": {"thisrun_scripts_dir": str(tmp_path)}})


def test_write_one_hostfile_srun(tmp_path):
    hostfile = str(tmp_path / "hostfile_srun")
    make_slurm(tmp_path).write_one_hostfile(hostfile, CONFIG)
    assert (tmp_path / "hostfile_srun").read_text() == "0-3  ./fesom.x\n4-7  ./echam6"


AWICM3_CONFIG = {
    "general": {"valid_model_names": ["fesom", "oifs", "rnfmap", "xios"]},
    "computer": {"heterogeneous_parallelization": False, "launcher": "srun"},
    "fesom":  {"start_proc": 0,   "end_proc": 127, "execution_command": "prog_fesom.sh"},
    "oifs":   {"start_proc": 128, "end_proc": 139, "execution_command": "prog_oifs.sh"},
    "rnfmap": {"start_proc": 140, "end_proc": 140, "execution_command": "prog_rnfmap.sh"},
    "xios":   {"start_proc": 141, "end_proc": 141, "execution_command": "prog_xios.sh"},
}


def test_write_one_hostfile_srun_awicm3(tmp_path):
    hostfile = str(tmp_path / "hostfile_srun")
    make_slurm(tmp_path).write_one_hostfile(hostfile, AWICM3_CONFIG)
    expected = (
        "0-127  ./prog_fesom.sh\n"
        "128-139  ./prog_oifs.sh\n"
        "140-140  ./prog_rnfmap.sh\n"
        "141-141  ./prog_xios.sh"
    )
    assert (tmp_path / "hostfile_srun").read_text() == expected


def test_write_one_hostfile_mpirun(tmp_path):
    CONFIG["computer"]["launcher"] = "mpirun"
    hostfile = str(tmp_path / "hostfile_mpirun")
    make_slurm(tmp_path).write_one_hostfile(hostfile, CONFIG)
    assert (tmp_path / "hostfile_mpirun").read_text() == " -np 4 ./fesom.x : -np 4 ./echam6 "
