"""Tests for integration/config.py and integration/esm_tools.py."""

import pytest

from esm_catalog.integration.config import load_config
from esm_catalog.integration.esm_tools import add_files
from esm_catalog.storage.duckdb import CatalogDB


# ---------------------------------------------------------------------------
# integration/config.py
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
        import xarray as xr
        import numpy as np
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
