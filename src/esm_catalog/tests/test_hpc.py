"""Tests for hpc/detect.py and hpc/state.py."""

import subprocess
from pathlib import Path
from unittest.mock import patch

import pytest

from esm_catalog.hpc.detect import detect_hpc_storage
from esm_catalog.hpc.state import _dmattr_state, _scoutfs_state, get_hsm_state


# ---------------------------------------------------------------------------
# hpc/detect.py
# ---------------------------------------------------------------------------

class TestDetectHpcStorage:
    def test_albedo_path_returns_awi_lustre(self, tmp_path):
        path = Path("/albedo/work/user/exp/file.nc")
        result = detect_hpc_storage(path)
        assert result["hpc:facility"] == "AWI"
        assert result["hpc:system"] == "albedo"
        assert result["hpc:storage_type"] == "lustre"
        assert result["hpc:state"] == "online"

    def test_hpss_archive_path(self):
        path = Path("/arch/user/historical/file.nc")
        result = detect_hpc_storage(path)
        assert result["hpc:storage_type"] == "hpss"
        assert result["hpc:state"] == "offline"
        assert result["hpc:recall_time_estimate"] == 300

    def test_hpss_path_variant(self):
        path = Path("/hpss/data/file.nc")
        result = detect_hpc_storage(path)
        assert result["hpc:storage_type"] == "hpss"

    def test_unknown_path_returns_posix_fallback(self, tmp_path):
        path = tmp_path / "file.nc"
        path.touch()
        result = detect_hpc_storage(path)
        assert "hpc:storage_type" in result
        assert "hpc:state" in result

    def test_result_is_dict(self, tmp_path):
        result = detect_hpc_storage(Path("/albedo/work/test.nc"))
        assert isinstance(result, dict)


# ---------------------------------------------------------------------------
# hpc/state.py
# ---------------------------------------------------------------------------

class TestDmattrState:
    def test_online_state(self):
        mock_result = subprocess.CompletedProcess(
            args=[], returncode=0, stdout="state: REG\n", stderr=""
        )
        with patch("subprocess.run", return_value=mock_result):
            assert _dmattr_state(Path("/some/file.nc")) == "online"

    def test_migrating_state(self):
        mock_result = subprocess.CompletedProcess(
            args=[], returncode=0, stdout="state: MIG\n", stderr=""
        )
        with patch("subprocess.run", return_value=mock_result):
            assert _dmattr_state(Path("/some/file.nc")) == "migrating"

    def test_nearline_state(self):
        mock_result = subprocess.CompletedProcess(
            args=[], returncode=0, stdout="state: DUL\n", stderr=""
        )
        with patch("subprocess.run", return_value=mock_result):
            assert _dmattr_state(Path("/some/file.nc")) == "nearline"

    def test_offline_state(self):
        mock_result = subprocess.CompletedProcess(
            args=[], returncode=0, stdout="state: OFFL\n", stderr=""
        )
        with patch("subprocess.run", return_value=mock_result):
            assert _dmattr_state(Path("/some/file.nc")) == "offline"

    def test_dmattr_not_found_returns_unknown(self):
        with patch("subprocess.run", side_effect=FileNotFoundError):
            assert _dmattr_state(Path("/some/file.nc")) == "unknown"

    def test_nonzero_returncode_returns_unknown(self):
        mock_result = subprocess.CompletedProcess(
            args=[], returncode=1, stdout="", stderr="error"
        )
        with patch("subprocess.run", return_value=mock_result):
            assert _dmattr_state(Path("/some/file.nc")) == "unknown"


class TestScoutfsState:
    def test_released_means_nearline(self):
        mock_result = subprocess.CompletedProcess(
            args=[], returncode=0,
            stdout="/some/file.nc: (0x0000000d) released archived exists\n",
            stderr="",
        )
        with patch("subprocess.run", return_value=mock_result):
            assert _scoutfs_state(Path("/some/file.nc")) == "nearline"

    def test_no_released_means_online(self):
        mock_result = subprocess.CompletedProcess(
            args=[], returncode=0,
            stdout="/some/file.nc: (0x00000009) archived exists\n",
            stderr="",
        )
        with patch("subprocess.run", return_value=mock_result):
            assert _scoutfs_state(Path("/some/file.nc")) == "online"

    def test_lfs_not_found_returns_unknown(self):
        with patch("subprocess.run", side_effect=FileNotFoundError):
            assert _scoutfs_state(Path("/some/file.nc")) == "unknown"


class TestGetHsmState:
    def test_falls_back_to_online_when_no_tools(self):
        with patch("esm_catalog.hpc.state._dmattr_state", return_value="unknown"), \
             patch("esm_catalog.hpc.state._scoutfs_state", return_value="unknown"):
            result = get_hsm_state(Path("/albedo/work/file.nc"))
        assert result == "online"

    def test_uses_dmattr_result_when_available(self):
        with patch("esm_catalog.hpc.state._dmattr_state", return_value="nearline"):
            result = get_hsm_state(Path("/some/file.nc"))
        assert result == "nearline"

    def test_falls_back_to_scoutfs_when_dmattr_unknown(self):
        with patch("esm_catalog.hpc.state._dmattr_state", return_value="unknown"), \
             patch("esm_catalog.hpc.state._scoutfs_state", return_value="migrating"):
            result = get_hsm_state(Path("/some/file.nc"))
        assert result == "migrating"
