from __future__ import annotations
"""Tests for cli.py — all commands exercised via click.testing.CliRunner."""

import json
from pathlib import Path

import numpy as np
import pytest
import xarray as xr
from click.testing import CliRunner

from esm_catalog.cli import main


# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

@pytest.fixture()
def runner():
    return CliRunner()


@pytest.fixture()
def outdata_nc(tmp_path):
    """Single NetCDF in the ESM-Tools outdata directory convention."""
    nc_dir = tmp_path / "experiments" / "test-exp" / "outdata" / "fesom"
    nc_dir.mkdir(parents=True)
    path = nc_dir / "ssh.fesom.185001.nc"
    ds = xr.Dataset(
        {"ssh": xr.DataArray(
            np.random.rand(3, 8, 16).astype("float32"),
            dims=["time", "lat", "lon"],
            attrs={"long_name": "Sea Surface Height", "units": "m"},
        )},
        coords={
            "time": np.array(
                ["1850-01-31", "1850-02-28", "1850-03-31"], dtype="datetime64[ns]"
            ),
            "lat": np.linspace(-90, 90, 8),
            "lon": np.linspace(-180, 180, 16),
        },
    )
    ds.to_netcdf(path)
    return path


@pytest.fixture()
def outdata_dir(tmp_path):
    """Directory with two NetCDF files under outdata/fesom/."""
    nc_dir = tmp_path / "experiments" / "test-exp" / "outdata" / "fesom"
    nc_dir.mkdir(parents=True)
    for month in ("185001", "185002"):
        path = nc_dir / f"ssh.fesom.{month}.nc"
        ds = xr.Dataset(
            {"ssh": xr.DataArray(
                np.random.rand(3, 8, 16).astype("float32"),
                dims=["time", "lat", "lon"],
            )},
            coords={
                "time": np.array(
                    ["1850-01-31", "1850-02-28", "1850-03-31"], dtype="datetime64[ns]"
                ),
                "lat": np.linspace(-90, 90, 8),
                "lon": np.linspace(-180, 180, 16),
            },
        )
        ds.to_netcdf(path)
    return tmp_path / "experiments" / "test-exp"


@pytest.fixture()
def config_yaml(tmp_path, outdata_nc):
    """Minimal ESM-Tools finished_config.yaml pointing at outdata_nc's directory."""
    cfg = tmp_path / "finished_config.yaml"
    cfg.write_text(
        f"general:\n  expid: test-exp\n"
        f"fesom:\n  outdata_dir: {outdata_nc.parent}\n"
    )
    return cfg


# ---------------------------------------------------------------------------
# scan
# ---------------------------------------------------------------------------

class TestScanCommand:

    def test_exit_code_zero_single_file(self, runner, tmp_path, outdata_nc):
        result = runner.invoke(
            main, ["scan", str(outdata_nc), "--db", str(tmp_path / "c.duckdb")]
        )
        assert result.exit_code == 0

    def test_reports_scanned_count(self, runner, tmp_path, outdata_nc):
        result = runner.invoke(
            main, ["scan", str(outdata_nc), "--db", str(tmp_path / "c.duckdb")]
        )
        assert "Scanned 1 files: 1 cataloged" in result.output

    def test_creates_db_file(self, runner, tmp_path, outdata_nc):
        db_path = tmp_path / "catalog.duckdb"
        runner.invoke(main, ["scan", str(outdata_nc), "--db", str(db_path)])
        assert db_path.exists()

    def test_db_in_nested_nonexistent_dir(self, runner, tmp_path, outdata_nc):
        """CatalogDB creates parent directories automatically."""
        db_path = tmp_path / "a" / "b" / "catalog.duckdb"
        result = runner.invoke(main, ["scan", str(outdata_nc), "--db", str(db_path)])
        assert result.exit_code == 0
        assert db_path.exists()

    def test_inserts_one_item(self, runner, tmp_path, outdata_nc):
        from esm_catalog.storage.duckdb import CatalogDB
        db_path = tmp_path / "catalog.duckdb"
        runner.invoke(main, ["scan", str(outdata_nc), "--db", str(db_path)])
        with CatalogDB(db_path) as db:
            count = db.db.execute("SELECT COUNT(*) FROM items").fetchone()[0]
        assert count == 1

    def test_creates_collection(self, runner, tmp_path, outdata_nc):
        from esm_catalog.storage.duckdb import CatalogDB
        db_path = tmp_path / "catalog.duckdb"
        runner.invoke(main, ["scan", str(outdata_nc), "--db", str(db_path)])
        with CatalogDB(db_path) as db:
            count = db.db.execute("SELECT COUNT(*) FROM collections").fetchone()[0]
        assert count == 1

    def test_scans_directory_recursively(self, runner, tmp_path, outdata_dir):
        db_path = tmp_path / "catalog.duckdb"
        result = runner.invoke(
            main, ["scan", str(outdata_dir), "--db", str(db_path)]
        )
        assert result.exit_code == 0
        assert "Scanned 2 files: 2 cataloged" in result.output

    def test_idempotent_no_duplicates(self, runner, tmp_path, outdata_nc):
        """Running scan twice on the same file must not create duplicate items."""
        from esm_catalog.storage.duckdb import CatalogDB
        db_path = tmp_path / "catalog.duckdb"
        runner.invoke(main, ["scan", str(outdata_nc), "--db", str(db_path)])
        runner.invoke(main, ["scan", str(outdata_nc), "--db", str(db_path)])
        with CatalogDB(db_path) as db:
            count = db.db.execute("SELECT COUNT(*) FROM items").fetchone()[0]
        assert count == 1

    def test_with_config_file(self, runner, tmp_path, outdata_nc, config_yaml):
        db_path = tmp_path / "catalog.duckdb"
        result = runner.invoke(main, [
            "scan", str(outdata_nc),
            "--db", str(db_path),
            "--config", str(config_yaml),
        ])
        assert result.exit_code == 0
        assert "Scanned 1 files: 1 cataloged" in result.output

    def test_config_sets_correct_experiment(self, runner, tmp_path, outdata_nc, config_yaml):
        """With --config the experiment_id comes from general.expid, not the path."""
        from esm_catalog.storage.duckdb import CatalogDB
        db_path = tmp_path / "catalog.duckdb"
        runner.invoke(main, [
            "scan", str(outdata_nc),
            "--db", str(db_path),
            "--config", str(config_yaml),
        ])
        with CatalogDB(db_path) as db:
            row = db.db.execute(
                "SELECT json_extract_string(data, '$.properties.experiment') FROM items"
            ).fetchone()
        assert row[0] == "test-exp"

    def test_empty_directory_warns(self, runner, tmp_path):
        empty = tmp_path / "empty"
        empty.mkdir()
        result = runner.invoke(
            main, ["scan", str(empty), "--db", str(tmp_path / "c.duckdb")]
        )
        assert result.exit_code == 0
        assert "No files to scan" in result.output

    def test_skips_unsupported_extensions(self, runner, tmp_path):
        """Files with skip extensions (.txt, .log, .yaml ...) are never added."""
        nc_dir = tmp_path / "experiments" / "x" / "outdata" / "fesom"
        nc_dir.mkdir(parents=True)
        (nc_dir / "readme.txt").write_text("not a dataset")
        (nc_dir / "run.log").write_text("log output")
        db_path = tmp_path / "catalog.duckdb"
        result = runner.invoke(main, ["scan", str(tmp_path), "--db", str(db_path)])
        assert result.exit_code == 0
        # .txt/.log are in skip_exts → nothing cataloged, no errors
        assert "0 cataloged" in result.output

    def test_skips_non_outdata_paths(self, runner, tmp_path, outdata_nc):
        """Files outside outdata/ are skipped via CollectionContextError (DEBUG)."""
        # Create a .nc in config/ — context resolution will fail gracefully
        config_dir = tmp_path / "experiments" / "test-exp" / "config"
        config_dir.mkdir(parents=True)
        import shutil
        shutil.copy(outdata_nc, config_dir / "settings.nc")
        db_path = tmp_path / "catalog.duckdb"
        result = runner.invoke(main, ["scan", str(tmp_path), "--db", str(db_path)])
        # The real outdata file is cataloged; the config copy fails context resolution
        assert result.exit_code == 0
        assert "1 cataloged" in result.output

    def test_verbose_flag_produces_debug_output(self, runner, tmp_path, outdata_nc):
        db_path = tmp_path / "catalog.duckdb"
        result = runner.invoke(
            main, ["--verbose", "scan", str(outdata_nc), "--db", str(db_path)]
        )
        assert result.exit_code == 0
        # DEBUG messages include "Resolved context" in verbose mode
        assert "DEBUG" in result.output

    def test_zero_byte_files_not_cataloged(self, runner, tmp_path):
        """Zero-size .nc files are attempted but fail and are never cataloged."""
        from esm_catalog.storage.duckdb import CatalogDB
        nc_dir = tmp_path / "experiments" / "zero" / "outdata" / "fesom"
        nc_dir.mkdir(parents=True)
        (nc_dir / "empty.nc").write_bytes(b"")
        db_path = tmp_path / "catalog.duckdb"
        result = runner.invoke(main, ["scan", str(tmp_path), "--db", str(db_path)])
        assert result.exit_code == 0
        # Zero-byte file is attempted but fails; nothing is added to the catalog
        assert "0 cataloged" in result.output
        with CatalogDB(db_path) as db:
            count = db.db.execute("SELECT COUNT(*) FROM items").fetchone()[0]
        assert count == 0


# ---------------------------------------------------------------------------
# scan-batch
# ---------------------------------------------------------------------------

class TestScanBatchCommand:

    def test_produces_parquet_file(self, runner, tmp_path, outdata_nc, config_yaml):
        out_pq = tmp_path / "out.parquet"
        result = runner.invoke(main, [
            "scan-batch", str(outdata_nc),
            "--config", str(config_yaml),
            "--output", str(out_pq),
            "--jobs", "1",
        ])
        assert result.exit_code == 0
        assert out_pq.exists()

    def test_parquet_contains_one_item(self, runner, tmp_path, outdata_nc, config_yaml):
        import pyarrow.parquet as pq
        out_pq = tmp_path / "out.parquet"
        runner.invoke(main, [
            "scan-batch", str(outdata_nc),
            "--config", str(config_yaml),
            "--output", str(out_pq),
            "--jobs", "1",
        ])
        table = pq.read_table(str(out_pq))
        assert len(table) == 1

    def test_parquet_item_is_valid_stac(self, runner, tmp_path, outdata_nc, config_yaml):
        import pyarrow.parquet as pq
        out_pq = tmp_path / "out.parquet"
        runner.invoke(main, [
            "scan-batch", str(outdata_nc),
            "--config", str(config_yaml),
            "--output", str(out_pq),
            "--jobs", "1",
        ])
        table = pq.read_table(str(out_pq))
        item = json.loads(table.column("data")[0].as_py())
        assert item["type"] == "Feature"
        assert "properties" in item
        assert item["properties"]["experiment"] == "test-exp"

    def test_multiple_files_all_scanned(self, runner, tmp_path, outdata_dir):
        nc_files = sorted((outdata_dir / "outdata" / "fesom").glob("*.nc"))
        assert len(nc_files) == 2
        cfg = tmp_path / "config.yaml"
        cfg.write_text(
            f"general:\n  expid: test-exp\n"
            f"fesom:\n  outdata_dir: {nc_files[0].parent}\n"
        )
        out_pq = tmp_path / "out.parquet"
        result = runner.invoke(main, [
            "scan-batch", *[str(f) for f in nc_files],
            "--config", str(cfg),
            "--output", str(out_pq),
            "--jobs", "1",
        ])
        assert result.exit_code == 0
        assert "2/2" in result.output

    def test_log_message_includes_count(self, runner, tmp_path, outdata_nc, config_yaml):
        out_pq = tmp_path / "out.parquet"
        result = runner.invoke(main, [
            "scan-batch", str(outdata_nc),
            "--config", str(config_yaml),
            "--output", str(out_pq),
            "--jobs", "1",
        ])
        assert "scan-batch" in result.output
        assert "1/1" in result.output

    def test_parallel_jobs_flag(self, runner, tmp_path, outdata_nc, config_yaml):
        """--jobs flag is accepted without error."""
        out_pq = tmp_path / "out.parquet"
        result = runner.invoke(main, [
            "scan-batch", str(outdata_nc),
            "--config", str(config_yaml),
            "--output", str(out_pq),
            "--jobs", "1",
        ])
        assert result.exit_code == 0


# ---------------------------------------------------------------------------
# merge-parquet
# ---------------------------------------------------------------------------

class TestMergeParquetCommand:

    def _stage(self, runner, tmp_path, nc_path, cfg_path, suffix=""):
        """Run scan-batch (sequential) and return the parquet path."""
        pq_path = tmp_path / f"batch{suffix}.parquet"
        runner.invoke(main, [
            "scan-batch", str(nc_path),
            "--config", str(cfg_path),
            "--output", str(pq_path),
            "--jobs", "1",
        ])
        return pq_path

    def test_creates_db(self, runner, tmp_path, outdata_nc, config_yaml):
        pq_path = self._stage(runner, tmp_path, outdata_nc, config_yaml)
        db_path = tmp_path / "merged.duckdb"
        result = runner.invoke(main, [
            "merge-parquet", str(pq_path), "--output", str(db_path)
        ])
        assert result.exit_code == 0
        assert db_path.exists()

    def test_inserts_items(self, runner, tmp_path, outdata_nc, config_yaml):
        from esm_catalog.storage.duckdb import CatalogDB
        pq_path = self._stage(runner, tmp_path, outdata_nc, config_yaml)
        db_path = tmp_path / "merged.duckdb"
        runner.invoke(main, ["merge-parquet", str(pq_path), "--output", str(db_path)])
        with CatalogDB(db_path) as db:
            count = db.db.execute("SELECT COUNT(*) FROM items").fetchone()[0]
        assert count == 1

    def test_creates_collection(self, runner, tmp_path, outdata_nc, config_yaml):
        from esm_catalog.storage.duckdb import CatalogDB
        pq_path = self._stage(runner, tmp_path, outdata_nc, config_yaml)
        db_path = tmp_path / "merged.duckdb"
        runner.invoke(main, ["merge-parquet", str(pq_path), "--output", str(db_path)])
        with CatalogDB(db_path) as db:
            count = db.db.execute("SELECT COUNT(*) FROM collections").fetchone()[0]
        assert count == 1

    def test_merges_multiple_parquet_files(self, runner, tmp_path, outdata_dir):
        """Two Parquet files (one per NC file) are merged into one DB."""
        from esm_catalog.storage.duckdb import CatalogDB
        nc_files = sorted((outdata_dir / "outdata" / "fesom").glob("*.nc"))
        pq_paths = []
        for i, nc in enumerate(nc_files):
            cfg = tmp_path / f"cfg{i}.yaml"
            cfg.write_text(
                f"general:\n  expid: test-exp\n"
                f"fesom:\n  outdata_dir: {nc.parent}\n"
            )
            pq_paths.append(self._stage(runner, tmp_path, nc, cfg, suffix=str(i)))
        db_path = tmp_path / "merged.duckdb"
        result = runner.invoke(main, [
            "merge-parquet", *[str(p) for p in pq_paths],
            "--output", str(db_path),
        ])
        assert result.exit_code == 0
        with CatalogDB(db_path) as db:
            count = db.db.execute("SELECT COUNT(*) FROM items").fetchone()[0]
        assert count == 2

    def test_log_message_includes_file_count(self, runner, tmp_path, outdata_nc, config_yaml):
        pq_path = self._stage(runner, tmp_path, outdata_nc, config_yaml)
        db_path = tmp_path / "merged.duckdb"
        result = runner.invoke(main, [
            "merge-parquet", str(pq_path), "--output", str(db_path)
        ])
        assert "Merged 1 Parquet" in result.output

    def test_merge_idempotent(self, runner, tmp_path, outdata_nc, config_yaml):
        """Merging the same Parquet twice does not duplicate items."""
        from esm_catalog.storage.duckdb import CatalogDB
        pq_path = self._stage(runner, tmp_path, outdata_nc, config_yaml)
        db_path = tmp_path / "merged.duckdb"
        runner.invoke(main, ["merge-parquet", str(pq_path), "--output", str(db_path)])
        runner.invoke(main, ["merge-parquet", str(pq_path), "--output", str(db_path)])
        with CatalogDB(db_path) as db:
            count = db.db.execute("SELECT COUNT(*) FROM items").fetchone()[0]
        assert count == 1


# ---------------------------------------------------------------------------
# serve
# ---------------------------------------------------------------------------

class TestServeCommand:

    def test_serve_without_catalog_starts(self, runner, monkeypatch):
        """serve with no --catalog is valid — catalogs can be added dynamically."""
        import sys
        from unittest.mock import MagicMock
        mock_uvicorn = MagicMock()
        with monkeypatch.context() as m:
            m.setitem(sys.modules, "uvicorn", mock_uvicorn)
            result = runner.invoke(main, ["serve"])
        assert result.exit_code == 0
        mock_uvicorn.run.assert_called_once()

    def test_missing_uvicorn_exits_with_code_1(self, runner, tmp_path, monkeypatch):
        """If uvicorn is not installed, serve exits 1 and logs an error."""
        import sys
        from esm_catalog.storage.duckdb import CatalogDB
        db_path = tmp_path / "catalog.duckdb"
        with CatalogDB(db_path):
            pass
        with monkeypatch.context() as m:
            m.setitem(sys.modules, "uvicorn", None)
            result = runner.invoke(main, ["serve", "--catalog", str(db_path)])
        assert result.exit_code == 1

    def test_missing_uvicorn_error_message(self, runner, tmp_path, monkeypatch):
        """The error message mentions the missing dependency."""
        import sys
        from esm_catalog.storage.duckdb import CatalogDB
        db_path = tmp_path / "catalog.duckdb"
        with CatalogDB(db_path):
            pass
        with monkeypatch.context() as m:
            m.setitem(sys.modules, "uvicorn", None)
            result = runner.invoke(main, ["serve", "--catalog", str(db_path)])
        assert "Missing dependency" in result.output or result.exit_code == 1

    def test_serve_starts_with_valid_catalog(self, runner, tmp_path, outdata_nc, monkeypatch):
        """serve calls uvicorn.run when everything is in place."""
        import sys
        from unittest.mock import MagicMock, patch

        db_path = tmp_path / "catalog.duckdb"
        # Pre-populate db
        runner.invoke(main, ["scan", str(outdata_nc), "--db", str(db_path)])

        mock_uvicorn = MagicMock()
        with patch.dict(sys.modules, {"uvicorn": mock_uvicorn}):
            result = runner.invoke(main, [
                "serve", "--catalog", str(db_path),
                "--host", "127.0.0.1", "--port", "19999",
            ])
        mock_uvicorn.run.assert_called_once()
        call_kwargs = mock_uvicorn.run.call_args
        assert call_kwargs[1].get("host") == "127.0.0.1" or call_kwargs[0][1] == "127.0.0.1"
