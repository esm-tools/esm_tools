"""Tests for integration/config.py and integration/esm_tools.py."""

from __future__ import annotations

import textwrap
from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr

from esm_catalog.integration.config import (
    extract_stac_metadata,
    find_file_operations_log,
    find_finished_configs,
    find_vcs_info,
    get_outdata_files,
    get_outdata_from_file_operations,
    load_vcs_info,
)
from esm_catalog.integration.esm_tools import add_files
from esm_catalog.storage.duckdb import CatalogDB


# ---------------------------------------------------------------------------
# config.py — filesystem helpers (no YAML loading required)
# ---------------------------------------------------------------------------

def test_find_finished_configs_empty(tmp_path):
    assert find_finished_configs(tmp_path) == []


def test_find_finished_configs_found(tmp_path):
    config_dir = tmp_path / "config"
    config_dir.mkdir()
    (config_dir / "exp1_finished_config.yaml_20000101-20001231").write_text("")
    (config_dir / "exp1_finished_config.yaml_20010101-20011231").write_text("")
    (config_dir / "unrelated.txt").write_text("")

    results = find_finished_configs(tmp_path)
    assert len(results) == 2
    assert all("_finished_config.yaml_" in p.name for p in results)


def test_get_outdata_files_present(tmp_path):
    nc1 = tmp_path / "exp_185001.nc"
    nc1.write_text("")
    config = {
        "echam": {
            "outdata_targets": {
                "stream1": str(nc1),
            }
        }
    }
    result = get_outdata_files(config, "echam")
    assert len(result) == 1
    assert result[0] == nc1


def test_get_outdata_files_missing_component():
    assert get_outdata_files({}, "fesom") == []


def test_find_file_operations_log_not_found(tmp_path):
    result = find_file_operations_log(tmp_path, "echam", "19580101-19580131")
    assert result is None


def test_find_file_operations_log_found(tmp_path):
    log_dir = tmp_path / "log"
    log_dir.mkdir()
    log_file = log_dir / "exp-alpha_echam_file_operations_tidy_20000101-20001231.yaml"
    log_file.write_text("")
    (tmp_path).rename  # just a path reference

    result = find_file_operations_log(tmp_path, "echam", "20000101-20001231")
    assert result == log_file


# ---------------------------------------------------------------------------
# config.py — YAML parsing (requires ruamel.yaml)
# ---------------------------------------------------------------------------

def test_get_outdata_from_file_operations(tmp_path):
    yaml_content = textwrap.dedent("""\
        echam:
          files:
            outdata:
              tas_200001.nc:
                destination: /work/exp/outdata/echam/tas_200001.nc
                source: /work/exp/work/echam/tas_200001.nc
                checksum: abc123
                tidy_op: copy
            log:
              run.log:
                destination: /work/exp/log/run.log
                source: /work/exp/work/run.log
                checksum: null
                tidy_op: move
    """)
    log_path = tmp_path / "exp_echam_file_operations_tidy_20000101-20001231.yaml"
    log_path.write_text(yaml_content)

    records = get_outdata_from_file_operations(log_path)
    assert len(records) == 1
    assert records[0]["destination"] == Path("/work/exp/outdata/echam/tas_200001.nc")
    assert records[0]["checksum"] == "abc123"
    assert records[0]["tidy_op"] == "copy"


def test_extract_stac_metadata():
    config = {
        "general": {
            "expid": "piControl",
            "scenario": "preindustrial",
            "resolution": "T63L47",
            "setup_name": "AWIESM",
            "version": "2.1",
            "run_datestamp": "18500101-18500131",
            "lresume": False,
        },
        "echam": {
            "metadata": {
                "Institute": "AWI",
                "Authors": "AWI Team",
                "Description": "ECHAM6 atmosphere",
                "Publications": "doi:10.xxx",
            }
        },
        "fesom": {},  # no metadata block → excluded
    }
    result = extract_stac_metadata(config)
    assert result["expid"] == "piControl"
    assert result["scenario"] == "preindustrial"
    assert "echam" in result["components"]
    assert "fesom" not in result["components"]
    assert result["components"]["echam"]["institute"] == "AWI"


def test_extract_stac_metadata_merges_vcs_info():
    config = {
        "general": {"expid": "piControl"},
        "echam": {
            "metadata": {"Institute": "AWI", "Authors": "AWI Team"},
        },
    }
    vcs_info = {
        "echam": {
            "path": "/work/model_codes/echam-6.3.05p2",
            "hash": "abc1234",
            "branch_name": "release-awiesm-2.1",
            "diffs": "",
        }
    }
    result = extract_stac_metadata(config, vcs_info=vcs_info)
    echam = result["components"]["echam"]
    assert echam["hash"] == "abc1234"
    assert echam["branch_name"] == "release-awiesm-2.1"
    assert echam["path"] == "/work/model_codes/echam-6.3.05p2"


def test_extract_stac_metadata_ignores_non_git_vcs_entry():
    """A plain string value (e.g. 'Not a git-controlled model!') is skipped."""
    config = {
        "general": {"expid": "piControl"},
        "fesom": {"metadata": {"Institute": "AWI"}},
    }
    vcs_info = {"fesom": "Not a git-controlled model!"}
    result = extract_stac_metadata(config, vcs_info=vcs_info)
    assert "hash" not in result["components"]["fesom"]


def test_extract_stac_metadata_without_vcs_info_unchanged():
    config = {
        "general": {"expid": "piControl"},
        "echam": {"metadata": {"Institute": "AWI"}},
    }
    result = extract_stac_metadata(config)
    assert "hash" not in result["components"]["echam"]


def test_find_vcs_info_found(tmp_path):
    exp_dir = tmp_path / "experiments" / "exp-alpha"
    log_dir = exp_dir / "log"
    log_dir.mkdir(parents=True)
    vcs_file = log_dir / "exp-alpha_vcs_info.yaml"
    vcs_file.write_text("echam:\n  hash: abc1234\n")

    assert find_vcs_info(exp_dir) == vcs_file


def test_find_vcs_info_not_found(tmp_path):
    exp_dir = tmp_path / "experiments" / "exp-alpha"
    (exp_dir / "log").mkdir(parents=True)
    assert find_vcs_info(exp_dir) is None


def test_load_vcs_info(tmp_path):
    vcs_file = tmp_path / "exp-alpha_vcs_info.yaml"
    vcs_file.write_text(
        "echam:\n"
        "  path: /work/model_codes/echam-6.3.05p2\n"
        "  hash: abc1234\n"
        "  branch_name: release-awiesm-2.1\n"
    )
    info = load_vcs_info(vcs_file)
    assert info["echam"]["hash"] == "abc1234"
    assert info["echam"]["branch_name"] == "release-awiesm-2.1"


def test_load_vcs_info_none_path():
    assert load_vcs_info(None) == {}


# ---------------------------------------------------------------------------
# esm_tools.py — add_files() integration test
# ---------------------------------------------------------------------------

def test_add_files_basic(tmp_path):
    exp_dir = tmp_path / "experiments" / "exp-alpha"
    out = exp_dir / "outdata" / "echam"
    out.mkdir(parents=True)

    times = pd.date_range("2000-01-01", periods=1, freq="MS")
    xr.Dataset(
        {"tas": (("time", "lat", "lon"), np.zeros((1, 2, 2), "float32"))},
        coords={"time": times, "lat": [-45.0, 45.0], "lon": [0.0, 90.0]},
    ).to_netcdf(out / "tas_200001.nc")

    config = {"general": {"expid": "exp-alpha"}}
    db_path = tmp_path / "catalog.duckdb"

    n = add_files(db_path, [out / "tas_200001.nc"], config)
    assert n == 1

    with CatalogDB(db_path) as db:
        _, total = db.search_items()
        assert total == 1


def test_add_files_skips_zero_byte(tmp_path):
    exp_dir = tmp_path / "experiments" / "exp-alpha"
    out = exp_dir / "outdata" / "echam"
    out.mkdir(parents=True)
    (out / "empty.nc").write_bytes(b"")

    config = {"general": {"expid": "exp-alpha"}}
    db_path = tmp_path / "catalog.duckdb"

    n = add_files(db_path, [out / "empty.nc"], config)
    assert n == 0


def test_add_files_skips_missing(tmp_path):
    config = {"general": {"expid": "exp-alpha"}}
    db_path = tmp_path / "catalog.duckdb"
    missing = tmp_path / "does_not_exist.nc"

    n = add_files(db_path, [missing], config)
    assert n == 0


def test_add_files_with_checksum(tmp_path):
    exp_dir = tmp_path / "experiments" / "exp-alpha"
    out = exp_dir / "outdata" / "echam"
    out.mkdir(parents=True)

    times = pd.date_range("2000-01-01", periods=1, freq="MS")
    nc = out / "tas_200001.nc"
    xr.Dataset(
        {"tas": (("time", "lat", "lon"), np.zeros((1, 2, 2), "float32"))},
        coords={"time": times, "lat": [-45.0, 45.0], "lon": [0.0, 90.0]},
    ).to_netcdf(nc)

    config = {"general": {"expid": "exp-alpha"}}
    db_path = tmp_path / "catalog.duckdb"
    checksums = {str(nc.resolve()): "deadbeefdeadbeef"}

    n = add_files(db_path, [nc], config, checksums=checksums)
    assert n == 1

    with CatalogDB(db_path) as db:
        items, _ = db.search_items()
        asset = items[0]["assets"]["data"]
        assert asset.get("file:checksum") == "deadbeefdeadbeef"
