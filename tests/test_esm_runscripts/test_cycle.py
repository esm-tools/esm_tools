"""Tests for native cycle advancement (date/run_number increment)."""

import pytest

from esm_runscripts.execution import cycle


class _FakeDate:
    """Minimal stand-in for esm_calendar.Date -- just enough for
    advance_date's own logic (key presence, += and .format()/.output()),
    without needing real calendar math."""

    def __init__(self, value):
        self.value = value

    def __add__(self, other):
        return _FakeDate(self.value + other)

    def __ge__(self, other):
        return self.value >= other.value

    def format(self, **kwargs):
        return f"formatted-{self.value}"

    def output(self):
        return f"output-{self.value}"


def _config():
    return {
        "general": {
            "run_number": 1,
            "current_date": _FakeDate(0),
            "delta_date": 1,
            "command_line_config": {},
        }
    }


def test_advance_date_increments_current_date():
    config = cycle.advance_date(_config())

    assert config["general"]["current_date"].value == 1
    assert config["general"]["run_number"] == 1  # untouched


def test_advance_date_raises_a_clear_error_for_a_missing_current_date():
    config = _config()
    del config["general"]["current_date"]

    with pytest.raises(KeyError, match="current_date"):
        cycle.advance_date(config)


def test_advance_date_raises_a_clear_error_for_a_missing_delta_date():
    config = _config()
    del config["general"]["delta_date"]

    with pytest.raises(KeyError, match="delta_date"):
        cycle.advance_date(config)


def test_advance_run_number_increments_run_number():
    config = cycle.advance_run_number(_config())

    assert config["general"]["run_number"] == 2
    assert config["general"]["current_date"].value == 0  # untouched


def test_advance_run_number_raises_a_clear_error_for_a_missing_run_number():
    config = _config()
    del config["general"]["run_number"]

    with pytest.raises(KeyError, match="run_number"):
        cycle.advance_run_number(config)


def test_sync_command_line_config_stashes_formatted_values(monkeypatch):
    from esm_runscripts import chunky_parts

    monkeypatch.setattr(chunky_parts, "update_command_line_config", lambda config: config)

    config = _config()
    config["general"]["current_date"] = _FakeDate(1)
    config["general"]["run_number"] = 2

    config = cycle.sync_command_line_config(config)

    assert config["general"]["command_line_config"]["current_date"] == "formatted-1"
    assert config["general"]["command_line_config"]["run_number"] == 2


def test_sync_command_line_config_calls_update_command_line_config(monkeypatch):
    from esm_runscripts import chunky_parts

    calls = []
    monkeypatch.setattr(
        chunky_parts, "update_command_line_config", lambda config: calls.append(config) or config
    )

    cycle.sync_command_line_config(_config())

    assert len(calls) == 1


def test_write_date_file_writes_date_and_run_number(tmp_path):
    config = _config()
    config["general"]["experiment_scripts_dir"] = str(tmp_path)
    config["general"]["expid"] = "myexp"
    config["general"]["setup_name"] = "awicm3"

    cycle.write_date_file(config)

    written = tmp_path / "myexp_awicm3.date"
    assert written.read_text() == "output-0 1"


def test_is_experiment_over_true_when_next_date_reached_final_date(monkeypatch):
    monkeypatch.setattr(cycle.logger, "progress", lambda msg: None, raising=False)
    config = {
        "general": {
            "next_date": _FakeDate(5),
            "final_date": _FakeDate(5),
        }
    }

    assert cycle.is_experiment_over(config) is True


def test_is_experiment_over_false_when_next_date_before_final_date():
    config = {
        "general": {
            "next_date": _FakeDate(4),
            "final_date": _FakeDate(5),
        }
    }

    assert cycle.is_experiment_over(config) is False
