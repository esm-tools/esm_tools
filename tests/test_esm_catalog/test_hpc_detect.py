"""detect_hpc_storage matches paths against configs/machines/*.yaml and
configs/storage/*.yaml `storage:` entries, loaded via esm_parser."""

from __future__ import annotations

import textwrap

import pytest

from esm_catalog.hpc import detect


@pytest.fixture
def storage_config(tmp_path, monkeypatch):
    """Point esm_parser.CONFIG_PATH at a throwaway configs/ tree."""
    machines_dir = tmp_path / "machines"
    storage_dir = tmp_path / "storage"
    machines_dir.mkdir()
    storage_dir.mkdir()

    (machines_dir / "testmachine.yaml").write_text(
        textwrap.dedent(
            """\
            storage:
                testmachine-work:
                    path_str: "/testmachine/"
                    facility: "TESTFACILITY"
                    system: "testmachine"
                    storage_type: "lustre"
                    state: "online"
            """
        )
    )
    (storage_dir / "hpss.yaml").write_text(
        textwrap.dedent(
            """\
            storage:
                hpss:
                    path_str: "/arch/|/hpss/"
                    storage_type: "hpss"
                    state: "offline"
                    recall_time_estimate: 300
            """
        )
    )

    monkeypatch.setattr(detect.esm_parser, "CONFIG_PATH", str(tmp_path))
    detect._load_storage_entries.cache_clear()
    yield
    detect._load_storage_entries.cache_clear()


def test_matches_machine_storage_entry(storage_config):
    result = detect.detect_hpc_storage("/testmachine/work/exp/output.nc")
    assert result == {
        "hpc:facility": "TESTFACILITY",
        "hpc:system": "testmachine",
        "hpc:storage_type": "lustre",
        "hpc:state": "online",
    }


@pytest.mark.parametrize("path", ["/arch/ab1234/output.nc", "/hpss/ab1234/output.nc"])
def test_matches_generic_storage_entry_without_facility(storage_config, path):
    result = detect.detect_hpc_storage(path)
    assert result == {
        "hpc:storage_type": "hpss",
        "hpc:state": "offline",
        "hpc:recall_time_estimate": 300,
    }


def test_falls_back_to_statvfs_when_no_entry_matches(storage_config, monkeypatch):
    monkeypatch.setattr(
        detect,
        "_detect_from_statvfs",
        lambda path: {"hpc:storage_type": "stub", "hpc:state": "online"},
    )
    result = detect.detect_hpc_storage("/unrelated/path/output.nc")
    assert result == {"hpc:storage_type": "stub", "hpc:state": "online"}


def test_real_albedo_config_matches():
    result = detect.detect_hpc_storage("/albedo/work/projects/foo/output.nc")
    assert result == {
        "hpc:facility": "AWI",
        "hpc:system": "albedo",
        "hpc:storage_type": "lustre",
        "hpc:state": "online",
    }


def test_real_levante_config_matches():
    result = detect.detect_hpc_storage("/work/mh0033/m300000/output.nc")
    assert result == {
        "hpc:facility": "DKRZ",
        "hpc:system": "levante",
        "hpc:storage_type": "lustre",
        "hpc:state": "online",
    }


def test_real_levante_config_excludes_levante_substring_in_path():
    result = detect.detect_hpc_storage("/some/levante_home/work/output.nc")
    assert result.get("hpc:system") != "levante"


def test_real_hpss_config_matches():
    result = detect.detect_hpc_storage("/arch/ab1234/experiment/output.nc")
    assert result == {
        "hpc:storage_type": "hpss",
        "hpc:state": "offline",
        "hpc:recall_time_estimate": 300,
    }
