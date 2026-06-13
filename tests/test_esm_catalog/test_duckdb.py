"""Unit tests for CatalogDB and persist_tree()."""

from __future__ import annotations

from esm_catalog.storage.duckdb import CatalogDB, persist_tree


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _make_collection(col_id: str, experiment: str = "exp-alpha") -> dict:
    return {
        "type": "Collection",
        "id": col_id,
        "stac_version": "1.0.0",
        "description": f"Test {col_id}",
        "title": col_id,
        "experiment": experiment,
        "links": [],
        "extent": {
            "spatial": {"bbox": [[-180.0, -90.0, 180.0, 90.0]]},
            "temporal": {"interval": [[None, None]]},
        },
        "license": "proprietary",
        "components": ["echam"],
    }


def _make_item(
    item_id: str,
    col_id: str,
    experiment: str = "exp-alpha",
    variable: str = "tas",
    dt: str = "2000-01-01T00:00:00Z",
    bbox: list | None = None,
) -> dict:
    return {
        "type": "Feature",
        "stac_version": "1.0.0",
        "id": item_id,
        "collection": col_id,
        "bbox": bbox or [-180.0, -90.0, 180.0, 90.0],
        "geometry": None,
        "properties": {
            "datetime": dt,
            "experiment": experiment,
            "component": "echam",
            "variable": variable,
            "format": "netcdf",
        },
        "assets": {},
        "links": [],
    }


# ---------------------------------------------------------------------------
# CatalogDB CRUD
# ---------------------------------------------------------------------------

def test_insert_and_get_collection(tmp_path):
    col = _make_collection("exp-alpha-echam")
    with CatalogDB(tmp_path / "catalog.duckdb") as db:
        db.insert_collection(col)
        result = db.get_collection("exp-alpha-echam")
    assert result is not None
    assert result["id"] == "exp-alpha-echam"
    assert result["experiment"] == "exp-alpha"


def test_collection_exists(tmp_path):
    with CatalogDB(tmp_path / "catalog.duckdb") as db:
        assert not db.collection_exists("no-such-col")
        db.insert_collection(_make_collection("exp-alpha-echam"))
        assert db.collection_exists("exp-alpha-echam")


def test_insert_and_search_item(tmp_path):
    col = _make_collection("exp-alpha-echam")
    item = _make_item("item-001", "exp-alpha-echam")
    with CatalogDB(tmp_path / "catalog.duckdb") as db:
        db.insert_collection(col)
        db.insert_item(item)
        items, total = db.search_items({"collection": "exp-alpha-echam"})
    assert total == 1
    assert items[0]["id"] == "item-001"


def test_upsert_is_idempotent(tmp_path):
    item = _make_item("item-001", "exp-alpha-echam")
    with CatalogDB(tmp_path / "catalog.duckdb") as db:
        db.insert_collection(_make_collection("exp-alpha-echam"))
        db.insert_item(item)
        db.insert_item(item)  # second insert should not raise
        _, total = db.search_items({"collection": "exp-alpha-echam"})
    assert total == 1


def test_upsert_collection_item_props(tmp_path):
    col = _make_collection("exp-alpha-echam")
    item = _make_item("item-001", "exp-alpha-echam", variable="tas")
    with CatalogDB(tmp_path / "catalog.duckdb") as db:
        db.insert_collection(col)
        db.insert_item(item)
        db.upsert_collection_item_props("exp-alpha-echam", item)
        props = db.get_collection_item_props("exp-alpha-echam")
    assert "variable" in props
    assert "tas" in props["variable"]
    assert "experiment" in props
    assert "exp-alpha" in props["experiment"]


def test_update_collection_extent(tmp_path):
    col = _make_collection("exp-alpha-echam")
    item = _make_item(
        "item-001", "exp-alpha-echam",
        dt="2000-01-01T00:00:00Z",
        bbox=[-10.0, -20.0, 10.0, 20.0],
    )
    with CatalogDB(tmp_path / "catalog.duckdb") as db:
        db.insert_collection(col)
        db.insert_item(item)
        db.update_collection_extent("exp-alpha-echam", item)
        result = db.get_collection("exp-alpha-echam")
    bbox = result["extent"]["spatial"]["bbox"][0]
    assert bbox == [-10.0, -20.0, 10.0, 20.0]
    interval = result["extent"]["temporal"]["interval"][0]
    assert interval[0] == "2000-01-01T00:00:00Z"


def test_iter_collections(tmp_path):
    with CatalogDB(tmp_path / "catalog.duckdb") as db:
        db.insert_collection(_make_collection("col-a", "exp-alpha"))
        db.insert_collection(_make_collection("col-b", "exp-beta"))
        cols = list(db.iter_collections())
    ids = {c["id"] for c in cols}
    assert ids == {"col-a", "col-b"}


def test_search_collections_by_experiment(tmp_path):
    with CatalogDB(tmp_path / "catalog.duckdb") as db:
        db.insert_collection(_make_collection("col-a", "exp-alpha"))
        db.insert_collection(_make_collection("col-b", "exp-beta"))
        cols, total = db.search_collections({"experiment": "exp-alpha"})
    assert total == 1
    assert cols[0]["id"] == "col-a"


def test_iter_experiments(tmp_path):
    with CatalogDB(tmp_path / "catalog.duckdb") as db:
        db.insert_collection(_make_collection("col-a", "exp-alpha"))
        db.insert_collection(_make_collection("col-b", "exp-beta"))
        experiments = db.iter_experiments()
    assert experiments == ["exp-alpha", "exp-beta"]


# ---------------------------------------------------------------------------
# persist_tree()
# ---------------------------------------------------------------------------

def test_persist_tree_roundtrip(tmp_path):
    catalog = {
        "collections": [_make_collection("exp-alpha-echam")],
        "items": [_make_item("item-001", "exp-alpha-echam", variable="tas")],
    }
    db_path = tmp_path / "catalog.duckdb"
    persist_tree(catalog, db_path)

    with CatalogDB(db_path) as db:
        assert db.collection_exists("exp-alpha-echam")
        items, total = db.search_items({"collection": "exp-alpha-echam"})
        assert total == 1
        assert items[0]["id"] == "item-001"
        props = db.get_collection_item_props("exp-alpha-echam")
        assert "tas" in props.get("variable", set())


def test_persist_tree_multiple_collections(tmp_path):
    catalog = {
        "collections": [
            _make_collection("exp-alpha-echam", "exp-alpha"),
            _make_collection("exp-alpha-fesom", "exp-alpha"),
        ],
        "items": [
            _make_item("item-001", "exp-alpha-echam"),
            _make_item("item-002", "exp-alpha-fesom", variable="ssh"),
        ],
    }
    db_path = tmp_path / "catalog.duckdb"
    persist_tree(catalog, db_path)

    with CatalogDB(db_path) as db:
        _, total = db.search_items()
        assert total == 2
        _, col_total = db.search_collections()
        assert col_total == 2


def test_persist_tree_extent_updated(tmp_path):
    col = _make_collection("exp-alpha-echam")
    item = _make_item(
        "item-001", "exp-alpha-echam",
        dt="2001-06-15T00:00:00Z",
        bbox=[-5.0, -5.0, 5.0, 5.0],
    )
    persist_tree({"collections": [col], "items": [item]}, tmp_path / "catalog.duckdb")

    with CatalogDB(tmp_path / "catalog.duckdb") as db:
        result = db.get_collection("exp-alpha-echam")
    interval = result["extent"]["temporal"]["interval"][0]
    assert interval[0] == "2001-06-15T00:00:00Z"
    bbox = result["extent"]["spatial"]["bbox"][0]
    assert bbox == [-5.0, -5.0, 5.0, 5.0]


# ---------------------------------------------------------------------------
# CLI: --db flag
# ---------------------------------------------------------------------------

def test_cli_scan_with_db(tmp_path):
    """esm-catalog scan <path> --db <file> creates a DuckDB with items."""
    import numpy as np
    import pandas as pd
    import xarray as xr
    from click.testing import CliRunner

    from esm_catalog.cli import main

    out = tmp_path / "experiments" / "exp-alpha" / "outdata" / "echam"
    out.mkdir(parents=True)
    times = pd.date_range("2000-01-01", periods=1, freq="MS")
    xr.Dataset(
        {"tas": (("time", "lat", "lon"), np.zeros((1, 2, 2), "float32"))},
        coords={"time": times, "lat": [-45.0, 45.0], "lon": [0.0, 90.0]},
    ).to_netcdf(out / "tas_200001.nc")

    db_path = tmp_path / "catalog.duckdb"
    result = CliRunner().invoke(
        main, ["scan", str(tmp_path), "--db", str(db_path)]
    )
    assert result.exit_code == 0, result.output
    assert db_path.exists()

    with CatalogDB(db_path) as db:
        _, total = db.search_items()
        assert total == 1
        assert db.collection_exists("exp-alpha")
