"""CLI scaffold tests: every command is registered and shows help; 'status'
reports the local catalog state without contacting the server.
"""

from __future__ import annotations

import json
from datetime import datetime, timezone

import pystac
import pytest
from click.testing import CliRunner
from upath import UPath

from esm_catalog.cli import main
from esm_catalog.scan.workspace import WorkspaceState, catalog_dir, save_state
from esm_catalog.storage.geoparquet import write_shard


@pytest.fixture
def runner():
    return CliRunner()


def test_help_lists_all_commands(runner):
    result = runner.invoke(main, ["--help"])
    assert result.exit_code == 0
    for command in ("auth", "scan", "push", "status", "distributed"):
        assert command in result.output


def test_auth_subcommands_registered(runner):
    result = runner.invoke(main, ["auth", "--help"])
    assert result.exit_code == 0
    assert "login" in result.output
    assert "logout" in result.output


def test_scan_missing_run_reports_clean_error(runner):
    """A directory with no finished_config yields a one-line error, no traceback."""
    result = runner.invoke(main, ["scan", "--exp-root", "/nonexistent/xyz"])
    assert result.exit_code != 0
    assert "scan requires a completed ESM-Tools run" in result.output
    assert "Traceback" not in result.output
    # The SourcingError was translated to a clean ClickException, not propagated.
    assert not isinstance(result.exception, Exception) or isinstance(
        result.exception, SystemExit
    )


# --------------------------------------------------------------------------- #
# status
# --------------------------------------------------------------------------- #


def test_status_before_scan_says_so(runner, tmp_path, monkeypatch):
    monkeypatch.delenv("ESM_CATALOG_SERVER_URL", raising=False)
    result = runner.invoke(main, ["status", "--exp-root", str(tmp_path)])
    assert result.exit_code == 0
    assert "Not yet scanned" in result.output
    assert "not configured" in result.output


def test_status_after_scan_reports_catalog_contents(runner, tmp_path, monkeypatch):
    monkeypatch.setenv("ESM_CATALOG_SERVER_URL", "https://stac.example.org")
    catalog = catalog_dir(UPath(tmp_path))
    items_dir = catalog / "items"
    items_dir.mkdir(parents=True)

    save_state(
        catalog,
        WorkspaceState(experiment_id="exp-alpha", scanned={"a.nc": "abc123"}),
    )
    (catalog / "collection.json").write_text(
        json.dumps({"type": "Collection", "id": "exp-alpha"})
    )
    item = pystac.Item(
        id="tas.2000",
        geometry={"type": "Point", "coordinates": [0.0, 0.0]},
        bbox=[0.0, 0.0, 0.0, 0.0],
        datetime=datetime(2000, 1, 1, tzinfo=timezone.utc),
        properties={},
        collection="exp-alpha",
    )
    item.add_asset("data", pystac.Asset(href="file:///tas.2000.nc"))
    write_shard([item], items_dir / "exp-alpha_stac_1.parquet")
    (catalog / "queryables.json").write_text(
        json.dumps({"properties": {"nml__a__b__c__d": {"type": "number"}}})
    )

    result = runner.invoke(main, ["status", "--exp-root", str(tmp_path)])
    assert result.exit_code == 0
    assert "exp-alpha" in result.output
    assert "collection: exp-alpha" in result.output
    assert "1 (1 item(s) total)" in result.output
    assert "tracked (incremental) files: 1" in result.output
    assert "queryables: 1" in result.output
    assert "https://stac.example.org" in result.output


# --------------------------------------------------------------------------- #
# distributed render-scripts
# --------------------------------------------------------------------------- #

_REQUIRED_FLAGS = [
    "--job-prefix", "catalog",
    "--scratch-dir", "/scratch/catalog",
    "--image-tag", "v6.68.0-rc.1-test-0.1.11",
    "--exp-root", "/exp/pi-ctrl-001",
    "--catalog-dir", "/exp/pi-ctrl-001/catalog",
    "--n-workers", "500",
]  # fmt: skip


def test_distributed_subcommand_registered(runner):
    result = runner.invoke(main, ["distributed", "--help"])
    assert result.exit_code == 0
    assert "render-scripts" in result.output


def test_render_scripts_missing_required_vars_reports_all_at_once(runner, tmp_path):
    result = runner.invoke(
        main, ["distributed", "render-scripts", "--out-dir", str(tmp_path)]
    )
    assert result.exit_code != 0
    for name in ("scratch_dir", "image_tag", "exp_root", "catalog_dir", "n_workers"):
        assert name in result.output


def test_render_scripts_writes_four_scripts_and_prints_submit_order(runner, tmp_path):
    result = runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(tmp_path), *_REQUIRED_FLAGS],
    )
    assert result.exit_code == 0, result.output
    for name in ("sched.sbatch", "worker.sbatch", "driver.sbatch", "cleanup.sbatch"):
        assert (tmp_path / name).exists()
    assert "SCHED_JOBID=$(sbatch --parsable" in result.output
    assert "--dependency=after:$SCHED_JOBID" in result.output
    assert "--dependency=afterany:$DRIVER_JOBID" in result.output


def test_render_scripts_worker_array_matches_n_workers(runner, tmp_path):
    runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(tmp_path), *_REQUIRED_FLAGS],
    )
    worker = (tmp_path / "worker.sbatch").read_text()
    assert "#SBATCH --array=1-500%500" in worker


def test_render_scripts_no_push_block_by_default(runner, tmp_path):
    runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(tmp_path), *_REQUIRED_FLAGS],
    )
    driver = (tmp_path / "driver.sbatch").read_text()
    assert "esm-catalog push" not in driver


def test_render_scripts_push_after_scan_flag_adds_push_block(runner, tmp_path):
    runner.invoke(
        main,
        [
            "distributed", "render-scripts", "--out-dir", str(tmp_path),
            *_REQUIRED_FLAGS,
            "--push-after-scan", "--server-url", "https://stac-dev.awi.de",
        ],  # fmt: skip
    )
    driver = (tmp_path / "driver.sbatch").read_text()
    assert "esm-catalog push" in driver
    assert "https://stac-dev.awi.de" in driver


def test_render_scripts_cli_flag_overrides_vars_file(runner, tmp_path):
    vars_file = tmp_path / "vars.yaml"
    vars_file.write_text(
        "\n".join(
            [
                "job_prefix: fromfile",
                "scratch_dir: /scratch/fromfile",
                "image_tag: v6.68.0-rc.1-test-0.1.11",
                "exp_root: /exp/fromfile",
                "catalog_dir: /exp/fromfile/catalog",
                "n_workers: 100",
            ]
        )
    )
    out_dir = tmp_path / "rendered"
    result = runner.invoke(
        main,
        [
            "distributed", "render-scripts", str(vars_file),
            "--out-dir", str(out_dir), "--n-workers", "999",
        ],  # fmt: skip
    )
    assert result.exit_code == 0, result.output
    worker = (out_dir / "worker.sbatch").read_text()
    assert "#SBATCH --array=1-999%999" in worker
    sched = (out_dir / "sched.sbatch").read_text()
    assert "fromfile-sched" in sched
    assert "/scratch/fromfile" in sched


def test_render_scripts_log_dir_defaults_from_state_dir(runner, tmp_path, monkeypatch):
    # state_dir() wraps platformdirs, whose XDG_STATE_HOME honouring is a Unix-only
    # convention (macOS ignores it by design) -- monkeypatch the function itself
    # rather than the env var, so this test is portable across dev machines.
    monkeypatch.setattr(
        "esm_catalog.xdg.state_dir", lambda: tmp_path / "state" / "esm-catalog"
    )
    out_dir = tmp_path / "rendered"
    result = runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(out_dir), *_REQUIRED_FLAGS],
    )
    assert result.exit_code == 0, result.output
    sched = (out_dir / "sched.sbatch").read_text()
    assert f"{tmp_path}/state/esm-catalog/logs" in sched
