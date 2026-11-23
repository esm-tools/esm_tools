"""Tests for the --wf-engine=pplan CLI entry points."""

import pytest

from esm_runscripts import cli
from esm_runscripts.execution.errors import JobCrashedError


def test_run_observe_cancels_the_job_and_reraises_on_a_crash(monkeypatch):
    from esm_runscripts.execution import observe

    def fake_wait_and_observe(*args, **kwargs):
        raise JobCrashedError("nan", "NaN detected")

    monkeypatch.setattr(observe, "wait_and_observe", fake_wait_and_observe)

    from esm_runscripts.runscript import generation

    monkeypatch.setattr(
        generation, "observe_log_path", lambda config, multi_step_name: "/tmp/does-not-matter.log"
    )

    import subprocess

    calls = []
    monkeypatch.setattr(subprocess, "run", lambda args, **kwargs: calls.append(args))

    config = {"general": {"launcher_pid": 1, "jobid": "42", "experiment_log_file": "/tmp/does-not-matter.log"}}

    class _StubExecution:
        executor = None

    class _StubWorkflow:
        def execution_of(self, multi_step_name):
            return _StubExecution()

    with pytest.raises(JobCrashedError, match="NaN detected"):
        cli._run_observe("compute", workflow=_StubWorkflow(), config=config)

    assert calls == [["scancel", "42"]]


def test_run_observe_logs_an_execution_finished_event_on_success(monkeypatch):
    from esm_runscripts.execution import cycle, observe
    from esm_runscripts.pplan.plan import Executor
    from esm_runscripts.runscript import generation

    monkeypatch.setattr(observe, "wait_and_observe", lambda *a, **k: None)
    monkeypatch.setattr(
        generation, "observe_log_path", lambda config, multi_step_name: "/tmp/does-not-matter.log"
    )
    monkeypatch.setattr(cycle, "advance_date", lambda config: config)
    monkeypatch.setattr(cycle, "advance_run_number", lambda config: config)
    monkeypatch.setattr(cycle, "sync_command_line_config", lambda config: config)
    monkeypatch.setattr(cycle, "write_date_file", lambda config: None)
    monkeypatch.setattr(cycle, "is_experiment_over", lambda config: True)

    logged = []
    monkeypatch.setattr(
        "esm_runscripts.log_events.log_event",
        lambda config, event, multi_step_name, executor, **fields: logged.append(
            (event, multi_step_name, executor, fields)
        ),
    )

    config = {"general": {"launcher_pid": 1, "jobid": "42", "experiment_log_file": "/tmp/does-not-matter.log"}}

    class _StubExecution:
        executor = Executor.BATCH

    class _StubWorkflow:
        resubmit_triggers = {}
        exit_point = "tidy"

        def execution_of(self, multi_step_name):
            return _StubExecution()

    cli._run_observe("compute", workflow=_StubWorkflow(), config=config)

    assert len(logged) == 1
    event, multi_step_name, executor, fields = logged[0]
    assert event.value == "execution.finished"
    assert multi_step_name == "compute"
    assert executor == Executor.BATCH
    assert isinstance(fields["duration_ms"], int)
