"""esm-catalog add/validate/rm asset -- single-file catalog edits without a
full scan (esm_catalog.cli).

Each writes exactly one local shard under <exp-root>/catalog/items/, never
auto-pushes. add reuses the schema cache scan/validate establish (proven via
genuinely unreadable "garbage" file content, same style as
test_scan_schema_freeze.py, so a wrongly-triggered real read surfaces as an
error instead of silently passing); validate always forces a real read; rm
writes a removed_assets tombstone.
"""

from __future__ import annotations

import numpy as np
import pytest
import xarray as xr
from click.testing import CliRunner
from ruamel.yaml import YAML
from upath import UPath

from esm_catalog.cli import main
from esm_catalog.scan.workspace import load_state
from esm_catalog.storage.geoparquet import read_shard

_EXPID = "demo"
_COORDS = {"lat": [-45.0, 0.0, 45.0], "lon": [0.0, 90.0, 180.0, 270.0]}


@pytest.fixture
def runner():
    return CliRunner()


def _build_experiment(tmp_path) -> UPath:
    """A minimal experiment with one real ECHAM file already scanned, so its
    (component, stream) schema is cached for `add` to shortcut against."""
    exp_root = UPath(tmp_path)
    ts_file = exp_root / "outdata" / "echam" / f"{_EXPID}_200001.01_echam"
    ts_file.parent.mkdir(parents=True, exist_ok=True)
    xr.Dataset(
        {"tas": (("time", "lat", "lon"), np.zeros((1, 3, 4)))},
        coords={"time": [np.datetime64("2000-01-15")], **_COORDS},
    ).to_netcdf(str(ts_file))

    config_dir = exp_root / "config"
    config_dir.mkdir(parents=True, exist_ok=True)
    doc = {
        "general": {
            "expid": _EXPID,
            "start_date": "2000-01-01",
            "end_date": "2000-12-31",
        },
        "echam": {
            "outdata_targets": {"echam": str(ts_file)},
            "metadata": {"Description": "d", "Authors": ["A"], "License": "CC-BY-4.0"},
        },
    }
    with (config_dir / f"{_EXPID}_finished_config.yaml").open("w") as f:
        YAML(typ="safe").dump(doc, f)
    return exp_root


def _scan(runner, exp_root) -> None:
    result = runner.invoke(main, ["scan", "--exp-root", str(exp_root)])
    assert result.exit_code == 0, result.output


def _only_shard(exp_root: UPath, name_contains: str):
    shards = list((exp_root / "catalog" / "items").glob(f"*{name_contains}*"))
    assert len(shards) == 1, shards
    return read_shard(shards[0])


def test_add_asset_shortcuts_via_cached_schema(runner, tmp_path):
    exp_root = _build_experiment(tmp_path)
    _scan(runner, exp_root)

    garbage = tmp_path / "demo_200002.01_echam"
    garbage.write_bytes(b"not a real netcdf file")

    result = runner.invoke(
        main,
        ["add", "asset", "echam-echam", str(garbage), "--exp-root", str(exp_root)],
    )

    assert result.exit_code == 0, result.output
    assert "added 1 asset" in result.output
    table = _only_shard(exp_root, "manual_echam-echam")
    row = table.to_pylist()[0]
    assert row["start_datetime"].strftime("%Y-%m") == "2000-02"
    assert row["variable"] == "tas"  # from the cached schema, not the garbage file


def test_add_asset_falls_back_to_real_read_for_an_unconventional_filename(
    runner, tmp_path
):
    exp_root = _build_experiment(tmp_path)
    _scan(runner, exp_root)

    real_file = tmp_path / "not_the_echam_naming_convention.nc"
    xr.Dataset(
        {"tas": (("time", "lat", "lon"), np.zeros((1, 3, 4)))},
        coords={"time": [np.datetime64("2000-03-15")], **_COORDS},
    ).to_netcdf(str(real_file))

    result = runner.invoke(
        main,
        ["add", "asset", "echam-echam", str(real_file), "--exp-root", str(exp_root)],
    )

    assert result.exit_code == 0, result.output
    table = _only_shard(exp_root, "manual_echam-echam")
    row = table.to_pylist()[0]
    assert row["start_datetime"].strftime("%Y-%m") == "2000-03"


def test_add_asset_reports_and_skips_unsupported_files(runner, tmp_path):
    exp_root = _build_experiment(tmp_path)
    _scan(runner, exp_root)

    good = tmp_path / "demo_200002.01_echam"
    good.write_bytes(b"not a real netcdf file")  # shortcuts via cache -- succeeds
    bad = tmp_path / "unrelated.txt"
    bad.write_text("nope")  # no facet match, no reader for this extension

    result = runner.invoke(
        main,
        [
            "add",
            "asset",
            "echam-echam",
            str(good),
            str(bad),
            "--exp-root",
            str(exp_root),
        ],
    )

    assert result.exit_code == 0, result.output
    assert "added 1 asset" in result.output
    assert "unrelated.txt" in result.output


def test_add_asset_fails_clearly_when_every_file_fails(runner, tmp_path):
    exp_root = _build_experiment(tmp_path)
    _scan(runner, exp_root)

    bad = tmp_path / "unrelated.txt"
    bad.write_text("nope")

    result = runner.invoke(
        main, ["add", "asset", "echam-echam", str(bad), "--exp-root", str(exp_root)]
    )

    assert result.exit_code != 0
    assert "no file could be added" in result.output


def test_add_asset_rejects_a_malformed_item_id(runner, tmp_path):
    exp_root = _build_experiment(tmp_path)
    _scan(runner, exp_root)
    some_file = tmp_path / "x.nc"
    some_file.write_bytes(b"x")

    # No "-" at all -- item ids are always "<component>-<stream>".
    result = runner.invoke(
        main, ["add", "asset", "nodash", str(some_file), "--exp-root", str(exp_root)]
    )

    assert result.exit_code != 0
    assert "not a valid item id" in result.output


def test_validate_always_forces_a_real_read(runner, tmp_path):
    exp_root = _build_experiment(tmp_path)
    _scan(runner, exp_root)

    garbage = tmp_path / "demo_200002.01_echam"
    garbage.write_bytes(b"not a real netcdf file")

    result = runner.invoke(
        main, ["validate", "echam-echam", str(garbage), "--exp-root", str(exp_root)]
    )

    # A real read on garbage content must fail -- proves force_read bypassed
    # the cached shortcut that test_add_asset_shortcuts... proved exists.
    assert result.exit_code != 0
    assert "unsupported" in result.output.lower()


def test_validate_refreshes_the_persisted_schema_cache(runner, tmp_path):
    exp_root = _build_experiment(tmp_path)
    _scan(runner, exp_root)

    real_file = tmp_path / "another_real_file.nc"
    xr.Dataset(
        {"tas": (("time", "lat", "lon"), np.zeros((1, 3, 4)))},
        coords={"time": [np.datetime64("2000-04-15")], **_COORDS},
    ).to_netcdf(str(real_file))

    result = runner.invoke(
        main, ["validate", "echam-echam", str(real_file), "--exp-root", str(exp_root)]
    )

    assert result.exit_code == 0, result.output
    state = load_state(exp_root / "catalog")
    assert "echam|echam" in state.schema_by_stream


def test_validate_never_touches_occurrences_since_check(runner, tmp_path):
    exp_root = _build_experiment(tmp_path)
    _scan(runner, exp_root)
    state_before = load_state(exp_root / "catalog")
    before = dict(state_before.occurrences_since_check)

    real_file = tmp_path / "another_real_file.nc"
    xr.Dataset(
        {"tas": (("time", "lat", "lon"), np.zeros((1, 3, 4)))},
        coords={"time": [np.datetime64("2000-04-15")], **_COORDS},
    ).to_netcdf(str(real_file))
    result = runner.invoke(
        main, ["validate", "echam-echam", str(real_file), "--exp-root", str(exp_root)]
    )
    assert result.exit_code == 0, result.output

    state_after = load_state(exp_root / "catalog")
    assert state_after.occurrences_since_check == before


def test_rm_asset_writes_a_tombstone_shard(runner, tmp_path):
    exp_root = _build_experiment(tmp_path)
    _scan(runner, exp_root)

    result = runner.invoke(
        main, ["rm", "asset", "echam-echam", "200001", "--exp-root", str(exp_root)]
    )

    assert result.exit_code == 0, result.output
    assert "marked" in result.output
    table = _only_shard(exp_root, "manual_echam-echam")
    row = table.to_pylist()[0]
    assert row.get("removed_assets") == ["200001"]
    assert row["id"] == "echam-echam"
    assert row["collection"]  # must be real -- push routes by it


def test_rm_asset_rejects_a_malformed_item_id(runner, tmp_path):
    exp_root = _build_experiment(tmp_path)
    _scan(runner, exp_root)

    result = runner.invoke(
        main, ["rm", "asset", "nodash", "somekey", "--exp-root", str(exp_root)]
    )

    assert result.exit_code != 0
    assert "not a valid item id" in result.output
