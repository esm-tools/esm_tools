"""Tests for integration/config.py and integration/esm_tools.py."""

import pytest

from esm_catalog.integration.config import (
    extract_stac_metadata,
    find_finished_configs,
    get_outdata_files,
    load_config,
)
from esm_catalog.integration.esm_tools import add_files
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
