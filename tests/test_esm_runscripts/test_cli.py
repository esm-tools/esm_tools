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

    config = {"general": {"launcher_pid": 1, "jobid": "42"}}

    with pytest.raises(JobCrashedError, match="NaN detected"):
        cli._run_observe("compute", workflow=object(), config=config)

    assert calls == [["scancel", "42"]]
