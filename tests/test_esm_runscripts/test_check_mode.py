"""Tests for check-mode (-c/--check): build but never run/submit."""

from esm_runscripts.execution.check_mode import is_check_mode


def test_is_check_mode_false_by_default():
    assert is_check_mode({"general": {}}) is False


def test_is_check_mode_true_when_check_flag_set():
    assert is_check_mode({"general": {"check": True}}) is True


def test_run_in_process_does_not_run_steps_in_check_mode(monkeypatch):
    from esm_runscripts.execution.dispatch import run_in_process
    from esm_runscripts.pplan import plan as plan_module

    calls = []
    monkeypatch.setattr(
        plan_module, "run_steps", lambda multi_step, config: calls.append(multi_step.name) or config
    )

    config = {
        "general": {
            "check": True,
            "valid_model_names": ["fesom"],
            "submit_to_batch_system": True,
        },
        "fesom": {"nproc": 128},
        "computer": {"partitions": {"compute": {"name": "compute"}}},
    }
    workflow = plan_module.Workflow.default(config)

    run_in_process(workflow, "prepcompute", config)

    assert calls == []


def test_launch_local_writes_script_but_does_not_run_it_in_check_mode(tmp_path, monkeypatch):
    from esm_runscripts.execution import launch
    from esm_runscripts.pplan.plan import Workflow
    from esm_runscripts.runscript import generation

    monkeypatch.setattr(generation, "environment_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "extra_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "command_lines", lambda *a, **k: [])

    scripts_dir = tmp_path / "scripts"
    scripts_dir.mkdir()
    config = {
        "general": {
            "check": True,
            "valid_model_names": ["fesom"],
            "submit_to_batch_system": False,
            "sh_interpreter": "/bin/bash",
            "thisrun_scripts_dir": str(scripts_dir),
            "expid": "test",
            "run_datestamp": "20260101",
        },
        "fesom": {"nproc": 128},
        "computer": {
            "batch_system": "slurm",
            "sh_interpreter": "/bin/bash",
            "partitions": {"compute": {"name": "compute", "cores_per_node": 32}},
        },
    }
    workflow = Workflow.default(config)

    calls = []
    monkeypatch.setattr(launch.subprocess, "run", lambda *a, **k: calls.append((a, k)))

    launch.launch_local(workflow, "compute", config)

    assert calls == []
    written = list(scripts_dir.glob("*.run"))
    assert len(written) == 1


def test_launch_batch_writes_script_but_does_not_submit_it_in_check_mode(tmp_path, monkeypatch):
    from esm_runscripts.execution import launch
    from esm_runscripts.pplan.plan import Workflow
    from esm_runscripts.runscript import generation

    monkeypatch.setattr(generation, "environment_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "extra_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "command_lines", lambda *a, **k: [])

    scripts_dir = tmp_path / "scripts"
    scripts_dir.mkdir()
    config = {
        "general": {
            "check": True,
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
            "submit": "sbatch",
            "partitions": {"compute": {"name": "compute", "cores_per_node": 32}},
        },
    }
    workflow = Workflow.default(config)

    calls = []
    monkeypatch.setattr(launch.subprocess, "run", lambda *a, **k: calls.append((a, k)))

    launch.launch_batch(workflow, "compute", config)

    assert calls == []
    written = list(scripts_dir.glob("*.run"))
    assert len(written) == 1
