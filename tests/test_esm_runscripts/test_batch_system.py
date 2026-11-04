import pytest

from esm_runscripts.batch_system import batch_system


def _make_config(patterns, thisrun_work_dir="/work/ab1234/run_01"):
    return {
        "computer": {"save_batch_env_patterns": patterns},
        "general": {"thisrun_work_dir": thisrun_work_dir},
    }


class TestGetEnvCaptureCommands:
    def test_slurm_pattern(self):
        config = _make_config(["SLURM"])
        result = batch_system.get_env_capture_commands(config)
        assert len(result) == 3
        assert "declare -p" in result[1]
        assert "SLURM" in result[1]
        assert "/work/ab1234/run_01/batch_system.env" in result[1]

    def test_pbs_pattern(self):
        config = _make_config(["PBS"])
        result = batch_system.get_env_capture_commands(config)
        assert "PBS" in result[1]

    def test_multiple_patterns(self):
        config = _make_config(["SLURM", "PBS"])
        result = batch_system.get_env_capture_commands(config)
        assert "SLURM|PBS" in result[1]

    def test_empty_patterns_returns_empty(self):
        config = _make_config([])
        assert batch_system.get_env_capture_commands(config) == []

    def test_missing_key_returns_empty(self):
        config = {"computer": {}, "general": {"thisrun_work_dir": "/tmp"}}
        assert batch_system.get_env_capture_commands(config) == []


class TestGetEnvRecoveryCommands:
    def test_returns_source_and_rm(self):
        config = _make_config(["SLURM"])
        result = batch_system.get_env_recovery_commands(config)
        assert len(result) == 2
        assert result[0] == "# Recover batch system environment variables"
        assert (
            result[1]
            == "source /work/ab1234/run_01/batch_system.env; rm /work/ab1234/run_01/batch_system.env"
        )

    def test_empty_patterns_returns_empty(self):
        config = _make_config([])
        assert batch_system.get_env_recovery_commands(config) == []

    def test_missing_key_returns_empty(self):
        config = {"computer": {}, "general": {"thisrun_work_dir": "/tmp"}}
        assert batch_system.get_env_recovery_commands(config) == []
