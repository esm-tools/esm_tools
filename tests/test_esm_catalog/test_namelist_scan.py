"""Namelist scanning + config-path resolution (scan layer)."""

from __future__ import annotations

from esm_catalog.scan.namelist import (
    get_namelist_config_path,
    scan_namelist_directory,
)


def test_get_namelist_config_path_standard_layout(tmp_path):
    cfg = tmp_path / "config" / "echam"
    cfg.mkdir(parents=True)
    assert get_namelist_config_path(tmp_path, "echam") == cfg


def test_get_namelist_config_path_missing(tmp_path):
    assert get_namelist_config_path(tmp_path, "echam") is None


def test_scan_namelist_directory(tmp_path):
    cfg = tmp_path / "config" / "echam"
    cfg.mkdir(parents=True)
    (cfg / "namelist.echam").write_text("&runctl\n    delta_time = 450\n    lcouple = .true.\n/\n")
    out = scan_namelist_directory(cfg, "echam")
    assert out["namelist.echam"]["runctl"]["delta_time"] == 450
    assert out["namelist.echam"]["runctl"]["lcouple"] is True
