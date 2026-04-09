"""Tests for integration/config.py and integration/esm_tools.py."""

import pytest

from esm_catalog.integration.config import (
    extract_stac_metadata,
    find_file_operations_log,
    find_finished_configs,
    get_outdata_files,
    get_outdata_from_file_operations,
    load_config,
)
from esm_catalog.integration.esm_tools import add_files, add_run
from esm_catalog.storage.duckdb import CatalogDB


# ---------------------------------------------------------------------------
# integration/config.py — load_config
# ---------------------------------------------------------------------------

class TestLoadConfig:
    def test_returns_none_for_none_path(self):
        assert load_config(None) is None

    def test_loads_yaml_file(self, tmp_path):
        yaml_file = tmp_path / "config.yaml"
        yaml_file.write_text(
            "general:\n  expid: myexp\n  pi_name: Test User\n"
        )
        cfg = load_config(yaml_file)
        assert cfg["general"]["expid"] == "myexp"
        assert cfg["general"]["pi_name"] == "Test User"

    def test_loads_nested_config(self, tmp_path):
        yaml_file = tmp_path / "finished_config.yaml"
        yaml_file.write_text(
            "general:\n  expid: picontrol\nfesom:\n  outdata_dir: /work/exp/fesom\n"
        )
        cfg = load_config(yaml_file)
        assert cfg["fesom"]["outdata_dir"] == "/work/exp/fesom"

    def test_returns_dict(self, tmp_path):
        yaml_file = tmp_path / "config.yaml"
        yaml_file.write_text("key: value\n")
        result = load_config(yaml_file)
        assert isinstance(result, dict)

    def test_loads_file_without_yaml_extension(self, tmp_path):
        """ESM-Tools writes date-ranged configs without a .yaml extension."""
        no_ext = tmp_path / "basic-001_finished_config.yaml_18500101-18500131"
        no_ext.write_text("general:\n  expid: basic-001\n")
        cfg = load_config(no_ext)
        assert cfg["general"]["expid"] == "basic-001"


# ---------------------------------------------------------------------------
# integration/config.py — find_finished_configs
# ---------------------------------------------------------------------------

class TestFindFinishedConfigs:
    def _make_exp(self, tmp_path, expid="basic-001", n_runs=3):
        """Create a fake experiment directory with n_runs finished_config files."""
        exp_dir = tmp_path / "experiments" / expid
        config_dir = exp_dir / "config"
        config_dir.mkdir(parents=True)
        dates = [
            ("18500101", "18500131"),
            ("18500201", "18500228"),
            ("18500301", "18500331"),
        ]
        files = []
        for start, end in dates[:n_runs]:
            p = config_dir / f"{expid}_finished_config.yaml_{start}-{end}"
            p.write_text(f"general:\n  expid: {expid}\n  run_datestamp: {start}-{end}\n")
            files.append(p)
        # Symlink: points to latest (like ESM-Tools does)
        symlink = config_dir / f"{expid}_finished_config.yaml"
        symlink.symlink_to(files[-1])
        return exp_dir, files

    def test_finds_all_date_range_files(self, tmp_path):
        exp_dir, expected = self._make_exp(tmp_path, n_runs=3)
        found = find_finished_configs(exp_dir)
        assert len(found) == 3
        assert all(p in found for p in expected)

    def test_excludes_symlink(self, tmp_path):
        """The plain symlink (no date suffix) must not appear in the result."""
        exp_dir, _ = self._make_exp(tmp_path, n_runs=2)
        found = find_finished_configs(exp_dir)
        assert not any("_finished_config.yaml" == p.name for p in found)

    def test_returns_sorted_order(self, tmp_path):
        exp_dir, expected = self._make_exp(tmp_path, n_runs=3)
        found = find_finished_configs(exp_dir)
        assert found == sorted(found)

    def test_returns_empty_when_no_configs(self, tmp_path):
        exp_dir = tmp_path / "experiments" / "empty-exp"
        (exp_dir / "config").mkdir(parents=True)
        assert find_finished_configs(exp_dir) == []

    def test_accepts_config_dir_directly(self, tmp_path):
        """Passing the config/ directory directly should also work."""
        exp_dir, expected = self._make_exp(tmp_path, n_runs=2)
        config_dir = exp_dir / "config"
        found = find_finished_configs(config_dir)
        assert len(found) == 2


# ---------------------------------------------------------------------------
# integration/config.py — get_outdata_files
# ---------------------------------------------------------------------------

class TestGetOutdataFiles:
    def _make_config(self, tmp_path, component="echam"):
        """Return a config dict with outdata_targets pointing to real files."""
        outdata_dir = tmp_path / "outdata" / component
        outdata_dir.mkdir(parents=True)
        files = {}
        for name in ["basic-001_185001.01_echam", "basic-001_185001.01_g3bim"]:
            p = outdata_dir / name
            p.write_bytes(b"GRIB" + b"\x00" * 100)  # minimal GRIB magic
            files[name.replace(".", "_") + "_nc"] = str(p)
        config = {
            "general": {"expid": "basic-001"},
            component: {"outdata_targets": files},
        }
        return config, list(files.values())

    def test_returns_paths_from_outdata_targets(self, tmp_path):
        config, raw_paths = self._make_config(tmp_path)
        result = get_outdata_files(config, "echam")
        assert len(result) == 2
        assert all(str(p) in raw_paths for p in result)

    def test_returns_empty_for_missing_component(self, tmp_path):
        config = {"general": {"expid": "basic-001"}}
        assert get_outdata_files(config, "echam") == []

    def test_returns_empty_when_no_outdata_targets(self, tmp_path):
        config = {"general": {"expid": "basic-001"}, "echam": {}}
        assert get_outdata_files(config, "echam") == []

    def test_returns_path_objects(self, tmp_path):
        from pathlib import Path
        config, _ = self._make_config(tmp_path)
        result = get_outdata_files(config, "echam")
        assert all(isinstance(p, Path) for p in result)


# ---------------------------------------------------------------------------
# integration/config.py — extract_stac_metadata
# ---------------------------------------------------------------------------

class TestExtractStacMetadata:
    def _make_config(self):
        return {
            "general": {
                "expid": "basic-001",
                "scenario": "PI-CTRL",
                "resolution": "T63_CORE2",
                "setup_name": "awiesm",
                "version": "2.1",
                "run_datestamp": "18500101-18500131",
                "lresume": False,
            },
            "echam": {
                "version": "6.3.05p2-awiesm-2.1",
                "metadata": {
                    "Institute": "MPI-Met",
                    "Authors": "Bjorn Stevens (bjorn.stevens@mpimet.mpg.de)",
                    "Description": "ECHAM atmosphere model, major version 6",
                    "Publications": "https://doi.org/10.1002/jame.20015",
                },
            },
            "fesom": {
                "version": "2.0",
                "metadata": {
                    "Institute": "AWI",
                    "Authors": "Sergey Danilov (sergey.danilov@awi.de)",
                    "Description": "FESOM ocean model",
                    "Publications": None,
                },
            },
            "computer": {"cores": 256},  # should be skipped
        }

    def test_extracts_expid(self):
        cfg = self._make_config()
        meta = extract_stac_metadata(cfg)
        assert meta["expid"] == "basic-001"

    def test_extracts_scenario(self):
        meta = extract_stac_metadata(self._make_config())
        assert meta["scenario"] == "PI-CTRL"

    def test_extracts_resolution(self):
        meta = extract_stac_metadata(self._make_config())
        assert meta["resolution"] == "T63_CORE2"

    def test_extracts_setup_name_and_version(self):
        meta = extract_stac_metadata(self._make_config())
        assert meta["setup_name"] == "awiesm"
        assert meta["setup_version"] == "2.1"

    def test_extracts_run_datestamp_and_lresume(self):
        meta = extract_stac_metadata(self._make_config())
        assert meta["run_datestamp"] == "18500101-18500131"
        assert meta["lresume"] is False

    def test_extracts_component_metadata(self):
        meta = extract_stac_metadata(self._make_config())
        assert "echam" in meta["components"]
        ec = meta["components"]["echam"]
        assert ec["institute"] == "MPI-Met"
        assert "Bjorn Stevens" in ec["authors"]
        assert ec["version"] == "6.3.05p2-awiesm-2.1"

    def test_skips_general_and_computer(self):
        meta = extract_stac_metadata(self._make_config())
        assert "general" not in meta["components"]
        assert "computer" not in meta["components"]

    def test_both_components_found(self):
        meta = extract_stac_metadata(self._make_config())
        assert "echam" in meta["components"]
        assert "fesom" in meta["components"]

    def test_empty_config_returns_nones(self):
        meta = extract_stac_metadata({})
        assert meta["expid"] is None
        assert meta["components"] == {}


# ---------------------------------------------------------------------------
# scan/context.py — _find_component_for_path with experiment_outdata_dir
# ---------------------------------------------------------------------------

class TestFindComponentExperimentOutdataDir:
    """Ensure resolve_context() works with real finished_config.yaml structure.

    The real config uses experiment_outdata_dir (not outdata_dir, which is None).
    """

    def test_resolves_via_experiment_outdata_dir(self, tmp_path, fesom_nc):
        """Component resolved via experiment_outdata_dir key."""
        from esm_catalog.scan.context import resolve_context

        config = {
            "general": {"expid": "basic-001"},
            "fesom": {
                "outdata_dir": None,  # None in real finished_config
                "experiment_outdata_dir": str(fesom_nc.parent),
            },
        }
        ctx = resolve_context(fesom_nc, config=config)
        assert ctx.component == "fesom"
        assert ctx.experiment_id == "basic-001"

    def test_outdata_dir_still_works(self, tmp_path, fesom_nc):
        """Backwards-compat: test configs using outdata_dir still work."""
        from esm_catalog.scan.context import resolve_context

        config = {
            "general": {"expid": "basic-001"},
            "fesom": {"outdata_dir": str(fesom_nc.parent)},
        }
        ctx = resolve_context(fesom_nc, config=config)
        assert ctx.component == "fesom"


# ---------------------------------------------------------------------------
# integration/esm_tools.py
# ---------------------------------------------------------------------------

class TestAddFiles:
    def test_catalogs_single_file(self, tmp_path, fesom_nc):
        db_path = tmp_path / "catalog.duckdb"
        config = {
            "general": {"expid": "basic-001"},
            "fesom": {"outdata_dir": str(fesom_nc.parent)},
        }
        n = add_files(db_path, [fesom_nc], config)
        assert n == 1
        with CatalogDB(db_path) as db:
            _, total = db.search_items()
            assert total == 1

    def test_creates_collection_automatically(self, tmp_path, fesom_nc):
        db_path = tmp_path / "catalog.duckdb"
        config = {
            "general": {"expid": "basic-001"},
            "fesom": {"outdata_dir": str(fesom_nc.parent)},
        }
        add_files(db_path, [fesom_nc], config)
        with CatalogDB(db_path) as db:
            assert db.collection_exists("basic-001-fesom")

    def test_item_collection_is_not_null(self, tmp_path, fesom_nc):
        db_path = tmp_path / "catalog.duckdb"
        config = {
            "general": {"expid": "basic-001"},
            "fesom": {"outdata_dir": str(fesom_nc.parent)},
        }
        add_files(db_path, [fesom_nc], config)
        with CatalogDB(db_path) as db:
            row = db.db.execute("SELECT collection FROM items LIMIT 1").fetchone()
            assert row[0] is not None

    def test_creates_db_directory_automatically(self, tmp_path, fesom_nc):
        db_path = tmp_path / "deep" / "nested" / "catalog.duckdb"
        config = {
            "general": {"expid": "basic-001"},
            "fesom": {"outdata_dir": str(fesom_nc.parent)},
        }
        add_files(db_path, [fesom_nc], config)
        assert db_path.exists()

    def test_multiple_files_same_collection(self, tmp_path, fesom_nc, echam_nc):
        """Two FESOM files → one collection with two items."""
        import numpy as np
        import xarray as xr
        # Create a second FESOM file in the same outdata dir
        fesom2 = fesom_nc.parent / "sst.fesom.185002.01.nc"
        ds = xr.Dataset(
            {"sst": xr.DataArray(
                np.random.rand(3, 8, 16).astype("float32"),
                dims=["time", "lat", "lon"],
                attrs={"standard_name": "sea_surface_temperature", "units": "K"},
            )},
            coords={
                "time": np.array(["1850-04-30", "1850-05-31", "1850-06-30"],
                                 dtype="datetime64[ns]"),
                "lat": np.linspace(-90, 90, 8),
                "lon": np.linspace(-180, 180, 16),
            },
        )
        ds.to_netcdf(fesom2)

        db_path = tmp_path / "catalog.duckdb"
        config = {
            "general": {"expid": "basic-001"},
            "fesom": {"outdata_dir": str(fesom_nc.parent)},
        }
        n = add_files(db_path, [fesom_nc, fesom2], config)
        assert n == 2
        with CatalogDB(db_path) as db:
            _, total = db.search_items()
            assert total == 2
            _, n_cols = db.search_collections()
            assert n_cols == 1  # both files → same collection

    def test_bad_file_does_not_abort_batch(self, tmp_path, fesom_nc):
        """A broken file should be logged and skipped; valid files still inserted."""
        bad = tmp_path / "broken.nc"
        bad.write_text("this is not a netcdf file")
        db_path = tmp_path / "catalog.duckdb"
        config = {
            "general": {"expid": "basic-001"},
            "fesom": {"outdata_dir": str(fesom_nc.parent)},
        }
        n = add_files(db_path, [fesom_nc, bad], config)
        assert n == 1  # only the valid file
        with CatalogDB(db_path) as db:
            _, total = db.search_items()
            assert total == 1

    def test_returns_count_of_inserted_items(self, tmp_path, fesom_nc):
        db_path = tmp_path / "catalog.duckdb"
        config = {
            "general": {"expid": "basic-001"},
            "fesom": {"outdata_dir": str(fesom_nc.parent)},
        }
        result = add_files(db_path, [fesom_nc], config)
        assert isinstance(result, int)
        assert result >= 0

    def test_resolves_via_experiment_outdata_dir(self, tmp_path, fesom_nc):
        """add_files() works when config uses experiment_outdata_dir (real finished_config)."""
        db_path = tmp_path / "catalog.duckdb"
        config = {
            "general": {"expid": "basic-001"},
            "fesom": {
                "outdata_dir": None,
                "experiment_outdata_dir": str(fesom_nc.parent),
            },
        }
        n = add_files(db_path, [fesom_nc], config)
        assert n == 1

    def test_checksum_stored_in_asset(self, tmp_path, fesom_nc):
        """file:checksum is stored in the asset when checksums dict is provided."""
        db_path = tmp_path / "catalog.duckdb"
        config = {
            "general": {"expid": "basic-001"},
            "fesom": {"outdata_dir": str(fesom_nc.parent)},
        }
        fake_md5 = "aabbccdd11223344aabbccdd11223344"
        checksums = {str(fesom_nc.resolve()): fake_md5}
        add_files(db_path, [fesom_nc], config, checksums=checksums)
        with CatalogDB(db_path) as db:
            import json
            row = db.db.execute("SELECT data FROM items LIMIT 1").fetchone()
            item = json.loads(row[0])
            assert item["assets"]["data"]["file:checksum"] == fake_md5

    def test_file_extension_url_added_when_checksum_present(self, tmp_path, fesom_nc):
        """STAC file extension URL is added to stac_extensions when checksum provided."""
        db_path = tmp_path / "catalog.duckdb"
        config = {
            "general": {"expid": "basic-001"},
            "fesom": {"outdata_dir": str(fesom_nc.parent)},
        }
        checksums = {str(fesom_nc.resolve()): "deadbeef" * 4}
        add_files(db_path, [fesom_nc], config, checksums=checksums)
        with CatalogDB(db_path) as db:
            import json
            row = db.db.execute("SELECT data FROM items LIMIT 1").fetchone()
            item = json.loads(row[0])
            assert any("file" in url for url in item["stac_extensions"])

    def test_no_checksum_no_file_extension(self, tmp_path, fesom_nc):
        """Without checksums, file extension is not added."""
        db_path = tmp_path / "catalog.duckdb"
        config = {
            "general": {"expid": "basic-001"},
            "fesom": {"outdata_dir": str(fesom_nc.parent)},
        }
        add_files(db_path, [fesom_nc], config, checksums=None)
        with CatalogDB(db_path) as db:
            import json
            row = db.db.execute("SELECT data FROM items LIMIT 1").fetchone()
            item = json.loads(row[0])
            assert not any(
                "stac-extensions.github.io/file" in url
                for url in item["stac_extensions"]
            )


# ---------------------------------------------------------------------------
# integration/config.py — find_file_operations_log
# ---------------------------------------------------------------------------

class TestFindFileOperationsLog:
    def _make_log(self, tmp_path, expid="myexp", component="fesom",
                  datestamp="19580101-19580131"):
        exp_dir = tmp_path / "experiments" / expid
        log_dir = exp_dir / "log"
        log_dir.mkdir(parents=True)
        log_file = log_dir / f"{expid}_{component}_file_operations_tidy_{datestamp}.yaml"
        log_file.write_text(f"# file_operations_tidy for {datestamp}\n")
        return exp_dir, log_file

    def test_finds_log_by_canonical_name(self, tmp_path):
        exp_dir, expected = self._make_log(tmp_path)
        result = find_file_operations_log(exp_dir, "fesom", "19580101-19580131")
        assert result == expected

    def test_returns_none_when_log_missing(self, tmp_path):
        exp_dir = tmp_path / "experiments" / "myexp"
        (exp_dir / "log").mkdir(parents=True)
        result = find_file_operations_log(exp_dir, "fesom", "19580101-19580131")
        assert result is None

    def test_returns_none_when_log_dir_missing(self, tmp_path):
        exp_dir = tmp_path / "experiments" / "myexp"
        exp_dir.mkdir(parents=True)
        result = find_file_operations_log(exp_dir, "fesom", "19580101-19580131")
        assert result is None

    def test_glob_fallback_finds_different_expid(self, tmp_path):
        """If expid differs from dir name, glob fallback still finds the file."""
        exp_dir = tmp_path / "experiments" / "myexp"
        log_dir = exp_dir / "log"
        log_dir.mkdir(parents=True)
        # File uses a different prefix than the dir name
        log_file = log_dir / "other-expid_fesom_file_operations_tidy_19580101-19580131.yaml"
        log_file.write_text("")
        result = find_file_operations_log(exp_dir, "fesom", "19580101-19580131")
        assert result == log_file


# ---------------------------------------------------------------------------
# integration/config.py — get_outdata_from_file_operations
# ---------------------------------------------------------------------------

FILE_OPS_YAML = """\
fesom:
  files:
    log:
      fesom.clock:
        source: /run/work/fesom.clock
        destination: /exp/log/fesom/fesom.clock
        phase: tidy
        tidy_op: copy
        checksum: aaaabbbbccccdddd0000111122223333
    outdata:
      sst.fesom.1958.nc:
        source: /run/work/sst.fesom.1958.nc
        destination: /exp/outdata/fesom/sst.fesom.1958.nc
        phase: tidy
        tidy_op: copy
        checksum: e8f4eb4f9f4a3bff932d2bffc5a78d68
      ssh.fesom.1958.nc:
        source: /run/work/ssh.fesom.1958.nc
        destination: /exp/outdata/fesom/ssh.fesom.1958.nc
        phase: tidy
        tidy_op: move
        checksum: 7f7a0feab048cf611387c91511ebf5cc
    restart_out:
      fesom_mesh.nc:
        source: /run/work/fesom_mesh.nc
        destination: /exp/restart/fesom/fesom_mesh.nc
        phase: tidy
        tidy_op: copy
        checksum: deadbeefdeadbeefdeadbeefdeadbeef
"""


class TestGetOutdataFromFileOperations:
    def _write_yaml(self, tmp_path, content=FILE_OPS_YAML):
        p = tmp_path / "myexp_fesom_file_operations_tidy_19580101-19580131.yaml"
        p.write_text(content)
        return p

    def test_returns_only_outdata_entries(self, tmp_path):
        path = self._write_yaml(tmp_path)
        records = get_outdata_from_file_operations(path)
        # log and restart_out entries must be excluded
        dests = [r["destination"].name for r in records]
        assert "fesom.clock" not in dests
        assert "fesom_mesh.nc" not in dests
        assert len(records) == 2

    def test_destination_paths_are_path_objects(self, tmp_path):
        from pathlib import Path
        path = self._write_yaml(tmp_path)
        records = get_outdata_from_file_operations(path)
        assert all(isinstance(r["destination"], Path) for r in records)

    def test_checksum_is_preserved(self, tmp_path):
        path = self._write_yaml(tmp_path)
        records = get_outdata_from_file_operations(path)
        by_name = {r["destination"].name: r for r in records}
        assert by_name["sst.fesom.1958.nc"]["checksum"] == "e8f4eb4f9f4a3bff932d2bffc5a78d68"
        assert by_name["ssh.fesom.1958.nc"]["checksum"] == "7f7a0feab048cf611387c91511ebf5cc"

    def test_tidy_op_is_preserved(self, tmp_path):
        path = self._write_yaml(tmp_path)
        records = get_outdata_from_file_operations(path)
        by_name = {r["destination"].name: r for r in records}
        assert by_name["sst.fesom.1958.nc"]["tidy_op"] == "copy"
        assert by_name["ssh.fesom.1958.nc"]["tidy_op"] == "move"

    def test_source_path_is_set(self, tmp_path):
        from pathlib import Path
        path = self._write_yaml(tmp_path)
        records = get_outdata_from_file_operations(path)
        assert all(isinstance(r["source"], Path) for r in records)

    def test_raises_for_missing_file(self, tmp_path):
        """A missing file propagates FileNotFoundError — callers use find_file_operations_log first."""
        with pytest.raises(FileNotFoundError):
            get_outdata_from_file_operations(tmp_path / "nonexistent.yaml")

    def test_empty_outdata_section(self, tmp_path):
        yaml = "fesom:\n  files:\n    outdata: {}\n"
        path = tmp_path / "ops.yaml"
        path.write_text(yaml)
        records = get_outdata_from_file_operations(path)
        assert records == []


# ---------------------------------------------------------------------------
# integration/esm_tools.py — add_run
# ---------------------------------------------------------------------------

class TestAddRun:
    def _make_experiment(self, tmp_path, fesom_nc):
        """Create a minimal experiment directory structure."""
        exp_dir = fesom_nc.parent.parent.parent  # .../experiments/basic-001
        log_dir = exp_dir / "log"
        log_dir.mkdir(parents=True, exist_ok=True)
        return exp_dir, log_dir

    def _make_config(self, fesom_nc):
        return {
            "general": {"expid": "basic-001", "run_datestamp": "18500101-18500131"},
            "fesom": {
                "outdata_dir": None,
                "experiment_outdata_dir": str(fesom_nc.parent),
                "outdata_targets": {"ssh_nc": str(fesom_nc)},
            },
        }

    def _write_file_ops_log(self, log_dir, fesom_nc, expid="basic-001",
                             datestamp="18500101-18500131"):
        """Write a minimal file_operations_tidy YAML pointing at fesom_nc."""
        log_file = log_dir / f"{expid}_fesom_file_operations_tidy_{datestamp}.yaml"
        log_file.write_text(
            f"fesom:\n"
            f"  files:\n"
            f"    outdata:\n"
            f"      {fesom_nc.name}:\n"
            f"        source: /run/work/{fesom_nc.name}\n"
            f"        destination: {fesom_nc}\n"
            f"        phase: tidy\n"
            f"        tidy_op: copy\n"
            f"        checksum: cafebabe12345678cafebabe12345678\n"
        )
        return log_file

    def test_add_run_uses_file_ops_log_when_present(self, tmp_path, fesom_nc):
        """add_run picks file_operations_tidy when it exists."""
        exp_dir, log_dir = self._make_experiment(tmp_path, fesom_nc)
        self._write_file_ops_log(log_dir, fesom_nc)
        config = self._make_config(fesom_nc)
        db_path = tmp_path / "catalog.duckdb"
        n = add_run(db_path, exp_dir, "fesom", "18500101-18500131", config)
        assert n == 1

    def test_add_run_checksum_from_file_ops_log(self, tmp_path, fesom_nc):
        """Checksum from file_operations_tidy appears in cataloged item."""
        exp_dir, log_dir = self._make_experiment(tmp_path, fesom_nc)
        self._write_file_ops_log(log_dir, fesom_nc)
        config = self._make_config(fesom_nc)
        db_path = tmp_path / "catalog.duckdb"
        add_run(db_path, exp_dir, "fesom", "18500101-18500131", config)
        with CatalogDB(db_path) as db:
            import json
            row = db.db.execute("SELECT data FROM items LIMIT 1").fetchone()
            item = json.loads(row[0])
            assert item["assets"]["data"]["file:checksum"] == "cafebabe12345678cafebabe12345678"

    def test_add_run_falls_back_to_config_when_no_log(self, tmp_path, fesom_nc):
        """add_run falls back to outdata_targets when no file_operations_tidy log."""
        exp_dir, _ = self._make_experiment(tmp_path, fesom_nc)
        # No log file written
        config = self._make_config(fesom_nc)
        db_path = tmp_path / "catalog.duckdb"
        n = add_run(db_path, exp_dir, "fesom", "18500101-18500131", config)
        assert n == 1
        # No checksum in fallback path
        with CatalogDB(db_path) as db:
            import json
            row = db.db.execute("SELECT data FROM items LIMIT 1").fetchone()
            item = json.loads(row[0])
            assert "file:checksum" not in item["assets"]["data"]

    def test_add_run_returns_zero_for_no_files(self, tmp_path, fesom_nc):
        """add_run returns 0 when config has no outdata files for the component."""
        exp_dir, _ = self._make_experiment(tmp_path, fesom_nc)
        config = {"general": {"expid": "basic-001"}, "fesom": {}}
        db_path = tmp_path / "catalog.duckdb"
        n = add_run(db_path, exp_dir, "fesom", "18500101-18500131", config)
        assert n == 0
