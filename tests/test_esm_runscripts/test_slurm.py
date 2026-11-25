import io

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


def make_taskset_config(tmp_path):
    (tmp_path / "scripts").mkdir()
    (tmp_path / "work").mkdir()
    return {
        "general": {
            "valid_model_names": ["fesom", "oasis3mct", "oifs"],
            "thisrun_scripts_dir": f"{tmp_path}/scripts/",
            "thisrun_work_dir": f"{tmp_path}/work",
        },
        "computer": {
            "heterogeneous_parallelization": True,
            "taskset": True,
            "partitions": {"compute": {"cores_per_node": 8}},
        },
        "fesom": {
            "tasks": 3,
            "omp_num_threads": 1,
            "executable": "fesom.x",
            "execution_command": "fesom.x",
        },
        "oasis3mct": {},
        "oifs": {
            "tasks": 2,
            "omp_num_threads": 4,
            "executable": "oifs",
            "execution_command": "oifs -v ecmwf",
        },
    }


def test_write_het_par_wrappers_taskset(tmp_path):
    config = make_taskset_config(tmp_path)
    config = Slurm.write_het_par_wrappers(config)

    work = tmp_path / "work"
    # fesom on cores 0-2 of node 0, oifs on cores 4-7 of node 0 and 0-3 of node 1
    assert (work / "rank_placement").read_text() == "0 0\n0 1\n0 2\n0 4\n1 0\n"

    prog_oifs = (work / "prog_oifs.sh").read_text()
    assert "awk -v r=$((PMI_RANK + 1)) 'NR == r {print $2; exit}' rank_placement" in (
        prog_oifs
    )
    assert "taskset -c $slot-$((slot + 4 - 1)) ./script_oifs.sh" in prog_oifs
    assert "export OMP_NUM_THREADS=4" in (work / "script_oifs.sh").read_text()
    assert config["fesom"]["execution_command_het_par"] == "prog_fesom.sh"
    assert not (work / "prog_oasis3mct.sh").exists()


def test_add_hostlist_file_gen_lines_reads_placement(tmp_path):
    config = make_taskset_config(tmp_path)
    runfile = io.StringIO()
    Slurm.add_hostlist_file_gen_lines(config, runfile)
    lines = runfile.getvalue()
    assert f"export SLURM_HOSTFILE={tmp_path}/work/hostlist" in lines
    assert "    echo ${listnodes[${node_index}]} >> hostlist" in lines
    assert f"done < {tmp_path}/work/rank_placement" in lines
