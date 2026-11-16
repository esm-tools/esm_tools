"""Tests for native job-crash detection."""

import pytest

from esm_runscripts.execution import observe
from esm_runscripts.execution.errors import JobCrashedError


def _config(**check_error):
    return {
        "general": {"jobid": "123"},
        "fesom": {"check_error": check_error} if check_error else {},
    }


def test_build_error_triggers_always_includes_generic_error_keyword():
    triggers = observe.build_error_triggers(_config(), "/tmp/log")

    assert triggers[0].keyword == "error"
    assert triggers[0].method == "warn"


def test_build_error_triggers_reads_component_check_error_block():
    triggers = observe.build_error_triggers(
        _config(nan={"method": "kill", "message": "NaN detected"}), "/tmp/log"
    )

    nan_trigger = next(t for t in triggers if t.keyword == "nan")
    assert nan_trigger.method == "kill"
    assert nan_trigger.message == "NaN detected"


def test_build_error_triggers_substitutes_jobid_in_custom_file():
    triggers = observe.build_error_triggers(
        _config(oops={"file": "/log/@jobid@.log"}), "/tmp/log"
    )

    oops_trigger = next(t for t in triggers if t.keyword == "oops")
    assert oops_trigger.search_file == "/log/123.log"


def test_check_for_errors_raises_job_crashed_error_on_kill_match(tmp_path):
    log_file = tmp_path / "run.log"
    log_file.write_text("everything fine\nNaN encountered\n")
    triggers = [
        observe.ErrorTrigger(
            keyword="nan",
            search_file=str(log_file),
            method="kill",
            message="NaN detected",
            frequency=60,
            next_check=0,
        )
    ]

    with pytest.raises(JobCrashedError, match="NaN detected"):
        observe.check_for_errors(triggers, elapsed=0)


def test_check_for_errors_warns_and_drops_trigger_without_raising(tmp_path, caplog):
    log_file = tmp_path / "run.log"
    log_file.write_text("a harmless warning line\n")
    triggers = [
        observe.ErrorTrigger(
            keyword="warning",
            search_file=str(log_file),
            method="warn",
            message="saw a warning",
            frequency=60,
            next_check=0,
        )
    ]

    remaining = observe.check_for_errors(triggers, elapsed=0)

    assert remaining == []


def test_check_for_errors_skips_triggers_not_yet_due():
    triggers = [
        observe.ErrorTrigger(
            keyword="nan", search_file="/does/not/exist", method="kill",
            message="x", frequency=60, next_check=60,
        )
    ]

    remaining = observe.check_for_errors(triggers, elapsed=0)

    assert remaining == triggers


def test_job_is_still_running_false_for_a_pid_that_does_not_exist():
    # PID 2**31-1 is never a real running process.
    assert observe.job_is_still_running(2**31 - 1) is False


def test_wait_and_observe_returns_once_pid_exits(tmp_path, monkeypatch):
    log_file = tmp_path / "run.log"
    log_file.write_text("")

    pids = iter([True, True, False])
    monkeypatch.setattr(observe, "job_is_still_running", lambda pid: next(pids))
    monkeypatch.setattr(observe.time, "sleep", lambda seconds: None)

    observe.wait_and_observe(pid=1, config={"general": {}}, log_file=str(log_file), poll_seconds=1)


def test_wait_and_observe_raises_when_a_kill_trigger_matches_before_pid_exits(tmp_path, monkeypatch):
    log_file = tmp_path / "run.log"
    log_file.write_text("NaN encountered\n")

    monkeypatch.setattr(observe, "job_is_still_running", lambda pid: True)
    monkeypatch.setattr(observe.time, "sleep", lambda seconds: None)

    config = {"general": {"jobid": "1"}, "fesom": {"check_error": {"nan": {"method": "kill", "message": "boom"}}}}

    with pytest.raises(JobCrashedError, match="boom"):
        observe.wait_and_observe(pid=1, config=config, log_file=str(log_file), poll_seconds=1)
