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
    for command in ("auth", "scan", "push", "status"):
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
