"""Tests for log_events.py (shared JSONL event vocabulary) and its two
emission points: Python (dispatch.py, cli.py's observe loop) and the
one shell-embedded line (generation.execution_started_line)."""

import json

import pytest
from loguru import logger

from esm_runscripts.log_events import DispatchEvent, ExecutionEvent, jsonl_path, log_event


@pytest.fixture
def jsonl_sink(monkeypatch, tmp_path):
    """Tracks only the sink(s) this test adds (see test_dispatch_events.py
    for why: logger.remove() with no args would also remove loguru's own
    default stderr sink)."""
    added_ids = []
    real_add = logger.add

    def _tracking_add(*args, **kwargs):
        sink_id = real_add(*args, **kwargs)
        added_ids.append(sink_id)
        return sink_id

    monkeypatch.setattr(logger, "add", _tracking_add)

    from esm_runscripts.logfiles import set_jsonl_event_sink

    config = {
        "general": {
            "expid": "test",
            "run_number": 1,
            "current_date": "2000-01-01T00:00:00",
            "jobid": "42",
            "experiment_log_file": str(tmp_path / "test.log"),
        }
    }
    path = set_jsonl_event_sink(config)
    yield config, path
    for sink_id in added_ids:
        logger.remove(sink_id)


def test_jsonl_path_is_a_sibling_of_experiment_log_file():
    config = {"general": {"experiment_log_file": "/tmp/foo.log"}}
    assert jsonl_path(config) == "/tmp/foo.log.jsonl"


def test_dispatch_event_is_a_closed_vocabulary():
    assert set(DispatchEvent) == {
        DispatchEvent.DISPATCHED,
        DispatchEvent.FINISHED,
        DispatchEvent.LAUNCHED,
        DispatchEvent.FAILED,
    }


def test_execution_event_is_a_closed_vocabulary():
    assert set(ExecutionEvent) == {
        ExecutionEvent.STARTED,
        ExecutionEvent.FINISHED,
        ExecutionEvent.FAILED,
    }
    assert ExecutionEvent.STARTED.value == "execution.started"


def test_log_event_writes_expected_fields(jsonl_sink):
    config, path = jsonl_sink

    log_event(config, ExecutionEvent.FINISHED, "compute", None, duration_ms=1234)

    events = [json.loads(line) for line in open(path) if line.strip()]
    assert len(events) == 1
    event = events[0]
    assert event["event"] == "execution.finished"
    assert event["expid"] == "test"
    assert event["jobid"] == "42"
    assert event["multi_step_name"] == "compute"
    assert event["duration_ms"] == 1234
    assert event["error"] is None
