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


def test_advance_date_increments_run_number_and_current_date(monkeypatch):
    from esm_runscripts import chunky_parts

    monkeypatch.setattr(chunky_parts, "update_command_line_config", lambda config: config)

    config = cycle.advance_date(_config())

    assert config["general"]["run_number"] == 2
    assert config["general"]["current_date"].value == 1


def test_advance_date_stashes_formatted_values_into_command_line_config(monkeypatch):
    from esm_runscripts import chunky_parts

    monkeypatch.setattr(chunky_parts, "update_command_line_config", lambda config: config)

    config = cycle.advance_date(_config())

    assert config["general"]["command_line_config"]["current_date"] == "formatted-1"
    assert config["general"]["command_line_config"]["run_number"] == 2


def test_advance_date_calls_update_command_line_config(monkeypatch):
    from esm_runscripts import chunky_parts

    calls = []
    monkeypatch.setattr(
        chunky_parts, "update_command_line_config", lambda config: calls.append(config) or config
    )

    cycle.advance_date(_config())

    assert len(calls) == 1


def _assert_raises_for_missing_key(missing):
    config = _config()
    del config["general"][missing]

    with pytest.raises(KeyError, match=missing):
        cycle.advance_date(config)


def test_advance_date_raises_a_clear_error_for_a_missing_run_number():
    _assert_raises_for_missing_key("run_number")


def test_advance_date_raises_a_clear_error_for_a_missing_current_date():
    _assert_raises_for_missing_key("current_date")


def test_advance_date_raises_a_clear_error_for_a_missing_delta_date():
    _assert_raises_for_missing_key("delta_date")


def test_write_date_file_writes_date_and_run_number(tmp_path):
    config = _config()
    config["general"]["experiment_scripts_dir"] = str(tmp_path)
    config["general"]["expid"] = "myexp"
    config["general"]["setup_name"] = "awicm3"

    cycle.write_date_file(config)

    written = tmp_path / "myexp_awicm3.date"
    assert written.read_text() == "output-0 1"
