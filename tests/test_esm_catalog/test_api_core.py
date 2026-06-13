"""Tests for the STAC API serve layer (PR-B1)."""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd
import pytest
import xarray as xr
from fastapi.testclient import TestClient

from esm_catalog.api.app import create_app
from esm_catalog.storage.duckdb import CatalogDB, persist_tree


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture()
def db_path(tmp_path: Path) -> Path:
    """A small DuckDB catalog pre-loaded with one collection and one item.

    Files must live under .../experiments/{exp}/outdata/{component}/ so that
    resolve_context() can derive the collection context from the path.
    """
    out = tmp_path / "experiments" / "exp-alpha" / "outdata" / "echam"
    out.mkdir(parents=True)
    nc = out / "tas_200001.nc"
    times = pd.date_range("2000-01-01", periods=1, freq="MS")
    xr.Dataset(
        {"tas": (("time", "lat", "lon"), np.zeros((1, 2, 2), dtype="float32"))},
        coords={"time": times, "lat": [-45.0, 45.0], "lon": [0.0, 90.0]},
    ).to_netcdf(nc)

    from esm_catalog.scan.ingest import scan_tree

    catalog = scan_tree(tmp_path)
    db = tmp_path / "catalog.duckdb"
    persist_tree(catalog, db)
    return db


@pytest.fixture()
def api_client(db_path: Path) -> TestClient:
    """TestClient backed by the test DuckDB catalog."""
    api = create_app(catalogs=[str(db_path)])
    return TestClient(api.app)


@pytest.fixture()
def empty_client() -> TestClient:
    """TestClient with no catalogs registered."""
    api = create_app(catalogs=[])
    return TestClient(api.app)


# ---------------------------------------------------------------------------
# System endpoints
# ---------------------------------------------------------------------------


def test_health(api_client: TestClient):
    r = api_client.get("/health")
    assert r.status_code == 200
    data = r.json()
    assert data["status"] == "ok"
    assert "catalogs_registered" in data


def test_readiness_with_catalog(api_client: TestClient):
    r = api_client.get("/readiness")
    assert r.status_code == 200
    data = r.json()
    assert data["ready"] is True
    assert data["catalogs_accessible"] >= 1


def test_readiness_no_catalogs(empty_client: TestClient):
    r = empty_client.get("/readiness")
    assert r.status_code == 200
    assert r.json()["ready"] is True


def test_format_probe(api_client: TestClient):
    r = api_client.post("/format", content="variable = 'ssh'",
                        headers={"Content-Type": "text/plain"})
    assert r.status_code == 200


# ---------------------------------------------------------------------------
# Landing page
# ---------------------------------------------------------------------------


def test_landing_page(api_client: TestClient):
    r = api_client.get("/")
    assert r.status_code == 200
    data = r.json()
    assert data.get("type") == "Catalog"
    rels = {lnk["rel"] for lnk in data.get("links", [])}
    assert "data" in rels or "self" in rels


# ---------------------------------------------------------------------------
# Collections
# ---------------------------------------------------------------------------


def test_list_collections_empty(empty_client: TestClient):
    r = empty_client.get("/collections")
    assert r.status_code == 200
    data = r.json()
    assert data["collections"] == []


def test_list_collections(api_client: TestClient):
    r = api_client.get("/collections")
    assert r.status_code == 200
    data = r.json()
    assert len(data["collections"]) >= 1
    col = data["collections"][0]
    assert "id" in col


def test_get_collection(api_client: TestClient, db_path: Path):
    with CatalogDB(db_path) as db:
        cols = list(db.iter_collections())
    assert cols, "Fixture catalog must have at least one collection"
    col_id = cols[0]["id"]

    r = api_client.get(f"/collections/{col_id}")
    assert r.status_code == 200
    assert r.json()["id"] == col_id


def test_get_collection_not_found(api_client: TestClient):
    r = api_client.get("/collections/does-not-exist")
    assert r.status_code == 404


# ---------------------------------------------------------------------------
# Items
# ---------------------------------------------------------------------------


def test_list_items(api_client: TestClient, db_path: Path):
    with CatalogDB(db_path) as db:
        cols = list(db.iter_collections())
    col_id = cols[0]["id"]

    r = api_client.get(f"/collections/{col_id}/items")
    assert r.status_code == 200
    data = r.json()
    assert data["type"] == "FeatureCollection"
    assert len(data["features"]) >= 1


def test_get_item(api_client: TestClient, db_path: Path):
    with CatalogDB(db_path) as db:
        items, _ = db.search_items(limit=1, offset=0)
    assert items
    item = items[0]
    col_id = item["collection"]
    item_id = item["id"]

    r = api_client.get(f"/collections/{col_id}/items/{item_id}")
    assert r.status_code == 200
    assert r.json()["id"] == item_id


def test_get_item_not_found(api_client: TestClient, db_path: Path):
    with CatalogDB(db_path) as db:
        cols = list(db.iter_collections())
    col_id = cols[0]["id"]

    r = api_client.get(f"/collections/{col_id}/items/no-such-item")
    assert r.status_code == 404


# ---------------------------------------------------------------------------
# Search
# ---------------------------------------------------------------------------


def test_search_get_no_filter(api_client: TestClient):
    r = api_client.get("/search")
    assert r.status_code == 200
    data = r.json()
    assert data["type"] == "FeatureCollection"
    assert data["numberMatched"] >= 1


def test_search_post_empty_body(api_client: TestClient):
    r = api_client.post("/search", json={})
    assert r.status_code == 200
    assert r.json()["type"] == "FeatureCollection"


def test_search_post_with_collections_filter(api_client: TestClient, db_path: Path):
    with CatalogDB(db_path) as db:
        cols = list(db.iter_collections())
    col_id = cols[0]["id"]

    r = api_client.post("/search", json={"collections": [col_id]})
    assert r.status_code == 200
    data = r.json()
    assert data["numberMatched"] >= 1
    for feature in data["features"]:
        assert feature["collection"] == col_id


def test_search_post_cql2_filter(api_client: TestClient, db_path: Path):
    with CatalogDB(db_path) as db:
        items, _ = db.search_items(limit=1, offset=0)
    assert items
    col_id = items[0]["collection"]

    cql2 = {"op": "=", "args": [{"property": "collection"}, col_id]}
    r = api_client.post("/search", json={"filter": cql2, "filter-lang": "cql2-json"})
    assert r.status_code == 200
    assert r.json()["numberMatched"] >= 1
