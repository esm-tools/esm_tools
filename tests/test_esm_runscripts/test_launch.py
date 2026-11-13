"""Tests for launching a MultiStep's runscript as a new OS process."""

from esm_runscripts.execution import launch
from esm_runscripts.pplan.plan import Workflow


def _config(tmp_path):
    scripts_dir = tmp_path / "scripts"
    scripts_dir.mkdir()
    return {
        "general": {
            "valid_model_names": ["fesom"],
            "submit_to_batch_system": True,
            "sh_interpreter": "/bin/bash",
            "thisrun_scripts_dir": str(scripts_dir),
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


def test_launch_local_writes_executable_script_and_runs_it(tmp_path, monkeypatch):
    from esm_runscripts.runscript import generation

    monkeypatch.setattr(generation, "environment_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "extra_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "command_lines", lambda *a, **k: [])

    config = _config(tmp_path)
    config["general"]["submit_to_batch_system"] = False
    workflow = Workflow.default(config)
    assert workflow.plan_execution["compute"].executor.value == "local"

    calls = []
    monkeypatch.setattr(
        launch.subprocess,
        "run",
        lambda args, cwd, check: calls.append((args, cwd, check)),
    )

    launch.launch_local(workflow, "compute", config)

    (args, cwd, check) = calls[0]
    assert cwd == config["general"]["thisrun_scripts_dir"]
    assert check is True
    assert args[0].suffix == ".run"
    assert args[0].stat().st_mode & 0o100


def test_launch_batch_writes_script_and_submits_it(tmp_path, monkeypatch):
    from esm_runscripts.runscript import generation

    monkeypatch.setattr(generation, "environment_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "extra_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "command_lines", lambda *a, **k: [])

    config = _config(tmp_path)
    workflow = Workflow.default(config)
    assert workflow.plan_execution["compute"].executor.value == "batch"

    calls = []
    monkeypatch.setattr(
        launch.subprocess,
        "run",
        lambda args, cwd, check: calls.append((args, cwd, check)),
    )

    launch.launch_batch(workflow, "compute", config)

    (args, cwd, check) = calls[0]
    assert args[0] == "sbatch"
    assert args[1].endswith(".run")
    assert cwd == config["general"]["thisrun_scripts_dir"]
    assert check is True
