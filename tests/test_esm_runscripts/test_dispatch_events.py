"""Tests for the structured, native-pplan-only JSONL dispatch event log."""

import json

import pytest
from loguru import logger

from esm_runscripts.execution.dispatch import DispatchEvent, start
from esm_runscripts.logfiles import set_jsonl_event_sink
from esm_runscripts.pplan.plan import Executor


@pytest.fixture
def jsonl_sink(monkeypatch):
    """Tracks only the loguru sink(s) *this test* adds via
    ``set_jsonl_event_sink``, and removes just those afterward --
    ``logger.remove()`` with no args would also remove loguru's own
    default stderr sink, which would leak across the rest of the test
    session."""
    added_ids = []
    real_add = logger.add

    def _tracking_add(*args, **kwargs):
        sink_id = real_add(*args, **kwargs)
        added_ids.append(sink_id)
        return sink_id

    monkeypatch.setattr(logger, "add", _tracking_add)
    yield
    for sink_id in added_ids:
        logger.remove(sink_id)


def _default_config(tmp_path):
    return {
        "general": {
            "valid_model_names": ["fesom"],
            "submit_to_batch_system": True,
            "expid": "test",
            "run_number": 1,
            "current_date": "2000-01-01T00:00:00",
            "jobid": "42",
            "experiment_log_file": str(tmp_path / "test_awicm3.log"),
        },
        "fesom": {"nproc": 128},
        "computer": {"partitions": {"compute": {"name": "compute"}}},
    }


def _read_jsonl(path):
    return [json.loads(line) for line in path.read_text().splitlines() if line.strip()]


def test_set_jsonl_event_sink_writes_a_sibling_file(tmp_path, jsonl_sink):
    config = _default_config(tmp_path)

    jsonl_path = set_jsonl_event_sink(config)

    assert jsonl_path == config["general"]["experiment_log_file"] + ".jsonl"


def test_start_emits_dispatched_and_finished_events_for_each_cascaded_step(
    tmp_path, monkeypatch, jsonl_sink
):
    from esm_runscripts.execution import dispatch as dispatch_module
    from esm_runscripts.execution import steps as steps_module
    from esm_runscripts.pplan import plan as plan_module

    monkeypatch.setattr(steps_module, "run_steps", lambda config, job_type: config)
    monkeypatch.setitem(
        dispatch_module._DISPATCHERS,
        plan_module.Executor.BATCH,
        lambda workflow, multi_step_name, config: config,
    )

    config = _default_config(tmp_path)
    jsonl_path = set_jsonl_event_sink(config)
    workflow = plan_module.Workflow.default(config)

    start(workflow, config)

    events = _read_jsonl(tmp_path / "test_awicm3.log.jsonl")
    assert jsonl_path == str(tmp_path / "test_awicm3.log.jsonl")

    names = [(e["multi_step_name"], e["event"]) for e in events]
    # prepexp and prepcompute are IN_PROCESS -> dispatched+finished each;
    # compute is BATCH -> dispatched+launched, cascade stops there.
    assert names == [
        ("prepexp", "dispatch.dispatched"),
        ("prepexp", "dispatch.finished"),
        ("prepcompute", "dispatch.dispatched"),
        ("prepcompute", "dispatch.finished"),
        ("compute", "dispatch.dispatched"),
        ("compute", "dispatch.launched"),
    ]

    # The whole point: multi_step_name is the real step, never "unknown"
    # (config["general"]["jobtype"] is never even read here).
    assert all(name != "unknown" for name, _ in names)

    finished = next(e for e in events if e["event"] == "dispatch.finished")
    assert finished["expid"] == "test"
    assert finished["jobid"] == "42"
    assert finished["executor"] == Executor.IN_PROCESS.value
    assert isinstance(finished["duration_ms"], int)


def test_failed_dispatch_emits_a_failed_event_with_the_error(tmp_path, monkeypatch, jsonl_sink):
    from esm_runscripts.execution import steps as steps_module
    from esm_runscripts.pplan import plan as plan_module

    def _boom(config, job_type):
        raise RuntimeError("kaboom")

    monkeypatch.setattr(steps_module, "run_steps", _boom)

    config = _default_config(tmp_path)
    set_jsonl_event_sink(config)
    workflow = plan_module.Workflow.default(config)

    with pytest.raises(RuntimeError):
        start(workflow, config)

    events = _read_jsonl(tmp_path / "test_awicm3.log.jsonl")
    failed = [e for e in events if e["event"] == "dispatch.failed"]
    assert len(failed) == 1
    assert failed[0]["multi_step_name"] == "prepexp"
    assert failed[0]["error"] == "kaboom"


def test_dispatch_event_is_a_closed_vocabulary():
    assert set(DispatchEvent) == {
        DispatchEvent.DISPATCHED,
        DispatchEvent.FINISHED,
        DispatchEvent.LAUNCHED,
        DispatchEvent.FAILED,
    }
    assert DispatchEvent.FINISHED.value == "dispatch.finished"
