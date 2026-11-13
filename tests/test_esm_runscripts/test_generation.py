"""Tests for native runscript generation."""

from esm_runscripts.pplan.plan import Workflow
from esm_runscripts.runscript import generation


def _config():
    return {
        "general": {
            "valid_model_names": ["fesom"],
            "submit_to_batch_system": True,
            "sh_interpreter": "/bin/bash",
            "thisrun_scripts_dir": "/tmp/scripts",
            "expid": "test",
            "run_datestamp": "20260101",
        },
        "fesom": {"nproc": 128},
        "computer": {
            "batch_system": "slurm",
            "sh_interpreter": "/bin/bash",
            "header_start": "#SBATCH",
            "submit": "sbatch",
            "partition_flag": "--partition=@partition@",
            "time_flag": "--time=01:00:00",
            "tasks_flag": "--ntasks=@tasks@",
            "output_flags": "--output=slurm.out",
            "name_flag": "--job-name=@jobtype@",
            "partitions": {"compute": {"name": "compute", "cores_per_node": 32}},
        },
    }


def test_compute_requirements_aggregates_esm_multistep_nproc():
    config = _config()
    workflow = Workflow.default(config)

    requirements = generation.compute_requirements(workflow, "compute", config)

    assert requirements.tasks == 128
    assert requirements.nodes == 4


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


def test_batch_header_lines_substitutes_tags():
    config = _config()
    workflow = Workflow.default(config)
    requirements = generation.ResourceRequirements(tasks=128, nodes=4)

    header = generation.batch_header_lines(workflow, "compute", requirements, config)

    assert "#!/bin/bash" in header
    assert "#SBATCH --partition=compute" in header
    assert "#SBATCH --ntasks=128" in header
    assert "#SBATCH --job-name=compute" in header


def test_generate_runscript_and_render_for_batch_executor(monkeypatch):
    config = _config()
    workflow = Workflow.default(config)
    monkeypatch.setattr(generation, "environment_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "extra_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "command_lines", lambda *a, **k: ["time ./fesom &"])

    context = generation.generate_runscript(workflow, "compute", config)

    assert context.is_batch is True
    assert any("#SBATCH" in line for line in context.header_lines)

    text = generation.render(context)

    assert "#SBATCH" in text
    assert "time ./fesom &" in text
    assert text.strip().endswith("wait")


def test_generate_runscript_for_in_process_plan_has_no_batch_header(monkeypatch):
    config = _config()
    workflow = Workflow.default(config)
    monkeypatch.setattr(generation, "environment_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "extra_lines", lambda *a, **k: [])
    monkeypatch.setattr(generation, "command_lines", lambda *a, **k: [])

    context = generation.generate_runscript(workflow, "prepcompute", config)

    assert context.is_batch is False
    assert not any("#SBATCH" in line for line in context.header_lines)


def test_write_runscript_writes_file_and_chmods_executable(tmp_path):
    context = generation.RunscriptContext(header_lines=["#!/bin/bash"], command_lines=["echo hi"])
    path = tmp_path / "test.run"

    generation.write_runscript(context, str(path), executable=True)

    assert path.read_text().strip().startswith("#!/bin/bash")
    assert path.stat().st_mode & 0o100  # owner-executable bit set
