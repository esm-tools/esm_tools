"""Tests for native runscript generation."""

import pytest

from esm_runscripts.pplan.plan import Workflow
from esm_runscripts.runscript import generation


def _config():
    return {
        "general": {
            "valid_model_names": ["fesom"],
            "submit_to_batch_system": True,
            "sh_interpreter": "/bin/bash",
            "thisrun_scripts_dir": "/tmp/scripts",
            "thisrun_work_dir": "/tmp/work",
            "expid": "test",
            "run_datestamp": "20260101",
        },
        "fesom": {"nproc": 128},
        "computer": {
            "batch_system": "slurm",
            "sh_interpreter": "/bin/bash",
            "header_start": "#SBATCH",
            "submit": "sbatch",
            "partition_flag": "--partition={{ partition }}",
            "time_flag": "--time=01:00:00",
            "tasks_flag": "--ntasks={{ tasks }}",
            "output_flags": "--output=slurm.out",
            "name_flag": "--job-name={{ jobtype }}",
            "partitions": {"compute": {"name": "compute", "cores_per_node": 32}},
        },
    }


def test_compute_requirements_aggregates_esm_multistep_nproc():
    config = _config()
    workflow = Workflow.default(config)

    requirements = generation.compute_requirements(workflow, "compute", config)

    assert requirements.tasks == 128
    assert requirements.nodes == 4


def test_compute_requirements_assigns_contiguous_mpmd_ranks_per_model():
    # Legacy's calculate_requirements side effect: each model gets a
    # contiguous start_proc/end_proc range, in valid_model_names order --
    # the ranges an MPMD hostfile maps each model's binary onto.
    config = _config()
    config["general"]["valid_model_names"] = ["oifs", "fesom", "oasis3mct"]
    config["oifs"] = {"nproc": 12, "omp_num_threads": 2}
    config["fesom"] = {"nproc": 128}
    config["oasis3mct"] = {}  # library component -- no cores of its own
    workflow = Workflow.default(config)

    generation.compute_requirements(workflow, "compute", config)

    assert config["oifs"]["start_proc"] == 0
    assert config["oifs"]["end_proc"] == 11
    assert config["fesom"]["start_proc"] == 12
    assert config["fesom"]["end_proc"] == 139
    assert "start_proc" not in config["oasis3mct"]


def test_compute_requirements_applies_mpi_num_groups_multiplier():
    config = _config()
    config["fesom"] = {"nproc": 10, "mpi_num_groups": 3}
    workflow = Workflow.default(config)

    generation.compute_requirements(workflow, "compute", config)

    assert config["fesom"]["nproc"] == 30
    assert config["fesom"]["tasks"] == 30
    assert config["fesom"]["end_proc"] == 29


def test_multiprog_hostfile_content_one_line_per_model_for_srun():
    config = _config()
    config["computer"]["launcher"] = "srun"
    config["general"]["valid_model_names"] = ["oifs", "fesom", "oasis3mct"]
    config["oifs"] = {"nproc": 12, "execution_command": "oifs_exe"}
    config["fesom"] = {"nproc": 128, "executable": "fesom_exe"}
    config["oasis3mct"] = {}
    workflow = Workflow.default(config)
    generation.compute_requirements(workflow, "compute", config)

    content = generation.multiprog_hostfile_content(config)

    assert content == "0-11  ./oifs_exe\n12-139  ./fesom_exe\n"


def test_multiprog_hostfile_content_colon_joined_for_mpirun():
    config = _config()
    config["computer"]["launcher"] = "mpirun"
    config["general"]["valid_model_names"] = ["oifs", "fesom"]
    config["oifs"] = {"nproc": 12, "execution_command": "oifs_exe"}
    config["fesom"] = {"nproc": 128, "executable": "fesom_exe"}
    workflow = Workflow.default(config)
    generation.compute_requirements(workflow, "compute", config)

    content = generation.multiprog_hostfile_content(config)

    assert content == " -np 12 ./oifs_exe : -np 128 ./fesom_exe"


def test_multiprog_hostfile_content_empty_when_no_model_has_ranks():
    config = _config()
    config["computer"]["launcher"] = "srun"
    config["general"]["valid_model_names"] = ["oasis3mct"]
    config["oasis3mct"] = {}

    assert generation.multiprog_hostfile_content(config) == ""


def test_compute_requirements_uses_execution_nproc_for_bash_kind():
    from esm_runscripts.pplan.plan import MultiStep, MultiStepExecution, MultiStepKind, Plan, PlanExecution

    config = _config()
    config["computer"]["partitions"]["pp"] = {"name": "pp", "cores_per_node": 16}
    workflow = Workflow(
        entry_point="postprocessing",
        plans={"postprocessing": Plan(name="postprocessing")},
        plan_execution={
            "postprocessing": PlanExecution(run_on_queue="pp"),
        },
        multi_steps={"postprocessing": MultiStep(name="postprocessing", step_of_plan="postprocessing")},
        multi_step_execution={
            "postprocessing": MultiStepExecution(nproc=33, kind=MultiStepKind.BASH),
        },
    )

    requirements = generation.compute_requirements(workflow, "postprocessing", config)

    assert requirements.tasks == 33
    assert requirements.nodes == 3  # ceil(33 / 16)


def test_cores_per_node_raises_for_an_unknown_queue():
    config = _config()

    with pytest.raises(ValueError, match="pp"):
        generation._cores_per_node(config, "pp")


def test_compute_requirements_raises_for_an_unknown_multi_step():
    config = _config()
    workflow = Workflow.default(config)

    with pytest.raises(KeyError, match="nonexistent"):
        generation.compute_requirements(workflow, "nonexistent", config)


def test_batch_header_lines_substitutes_tags():
    config = _config()
    workflow = Workflow.default(config)
    requirements = generation.ResourceRequirements(tasks=128, nodes=4)

    header = generation.batch_header_lines(workflow, "compute", requirements, config)

    assert "#!/bin/bash" in header
    assert "#SBATCH --partition=compute" in header
    assert "#SBATCH --ntasks=128" in header
    assert "#SBATCH --job-name=compute" in header


def test_batch_header_lines_raises_for_a_flag_referencing_an_unknown_variable():
    config = _config()
    config["computer"]["name_flag"] = "--job-name={{ typo }}"
    workflow = Workflow.default(config)
    requirements = generation.ResourceRequirements(tasks=128, nodes=4)

    with pytest.raises(Exception, match="typo"):
        generation.batch_header_lines(workflow, "compute", requirements, config)


def test_build_runscript_context_and_render_for_batch_executor(monkeypatch):
    config = _config()
    workflow = Workflow.default(config)
    monkeypatch.setattr(generation, "environment_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "extra_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "command_lines", lambda *a, **k: ["time ./fesom &"])

    context = generation.build_runscript_context(workflow, "compute", config)

    assert context.is_batch is True
    assert any("#SBATCH" in line for line in context.header_lines)

    text = generation.render(context)

    assert "#SBATCH" in text
    assert "time ./fesom &" in text
    assert text.strip().endswith("wait")


def test_build_runscript_context_for_in_process_plan_has_no_batch_header(monkeypatch):
    config = _config()
    workflow = Workflow.default(config)
    monkeypatch.setattr(generation, "environment_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "extra_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "command_lines", lambda *a, **k: [])

    context = generation.build_runscript_context(workflow, "prepcompute", config)

    assert context.is_batch is False
    assert not any("#SBATCH" in line for line in context.header_lines)


def test_command_lines_backgrounds_compute_and_appends_observe_call():
    import esm_calendar

    from esm_runscripts.pplan.plan import LaunchMode

    config = _config()
    config["computer"]["execution_command"] = "./fesom"
    config["general"].update(
        setup_name="awicm3",
        jobid="42",
        jobtype="compute",
        scriptname="my.yaml",
        run_number=3,
        current_date=esm_calendar.Date("2026-01-01", calendar="proleptic_gregorian"),
    )
    workflow = Workflow.default(config)
    assert workflow.multi_step_execution["compute"].launch_mode == LaunchMode.BACKGROUND

    lines = generation.command_lines(workflow, "compute", config, is_batch=True)

    assert lines[0] == "cd /tmp/work"
    assert lines[1] == "time ./fesom 2>&1 &"
    assert lines[2] == "process=$!"
    assert lines[3].startswith("esm_runscripts my.yaml -e test")
    assert "-t observe_compute" in lines[3]
    assert "-p ${process}" in lines[3]


def test_hostlist_gen_lines_empty_without_heterogeneous_taskset():
    config = _config()
    assert generation.hostlist_gen_lines(config) == []

    config["computer"]["heterogeneous_parallelization"] = True
    # taskset not set -> still empty (packjob/het-par-without-taskset path,
    # not ported, see runscript/batch_system/__init__.py).
    assert generation.hostlist_gen_lines(config) == []


def test_hostlist_gen_lines_builds_hostlist_shell_snippet():
    config = _config()
    config["general"]["thisrun_work_dir"] = "/tmp/work"
    config["general"]["valid_model_names"] = ["oifs", "fesom", "oasis3mct"]
    config["oifs"] = {"nproc": 12, "omp_num_threads": 2, "execution_command": "oifs_exe"}
    config["fesom"] = {"nproc": 128, "omp_num_threads": 1, "executable": "fesom_exe"}
    config["oasis3mct"] = {}
    config["computer"]["heterogeneous_parallelization"] = True
    config["computer"]["taskset"] = True

    lines = generation.hostlist_gen_lines(config)

    text = "\n".join(lines)
    assert "export SLURM_HOSTFILE=/tmp/work/hostlist" in text
    assert "mpi_tasks_oifs=12" in text
    assert "omp_threads_oifs=2" in text
    assert "mpi_tasks_fesom=128" in text
    assert "for model in oifs fesom ;do" in text
    # oasis3mct (no command) contributes nothing.
    assert "oasis3mct" not in text


def test_write_runscript_writes_file_and_chmods_executable(tmp_path):
    context = generation.RunscriptContext(header_lines=["#!/bin/bash"], command_lines=["echo hi"])
    path = tmp_path / "test.run"

    generation.write_runscript(context, path, executable=True)

    assert path.read_text().strip().startswith("#!/bin/bash")
    assert path.stat().st_mode & 0o100  # owner-executable bit set
