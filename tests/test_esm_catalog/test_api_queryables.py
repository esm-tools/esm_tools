"""Tests for the /queryables endpoints (PR-B2)."""

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
# Fixtures (same path structure as test_api_core.py)
# ---------------------------------------------------------------------------


@pytest.fixture()
def db_path(tmp_path: Path) -> Path:
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
    return TestClient(create_app(catalogs=[str(db_path)]).app)


@pytest.fixture()
def empty_client() -> TestClient:
    return TestClient(create_app(catalogs=[]).app)


# ---------------------------------------------------------------------------
# /queryables
# ---------------------------------------------------------------------------


def test_queryables_returns_200(api_client: TestClient):
    r = api_client.get("/queryables")
    assert r.status_code == 200


def test_queryables_json_schema_structure(api_client: TestClient):
    data = api_client.get("/queryables").json()
    assert data["$schema"] == "https://json-schema.org/draft/2019-09/schema"
    assert data["type"] == "object"
    assert "properties" in data
    assert "$id" in data


def test_queryables_always_has_datetime(api_client: TestClient):
    props = api_client.get("/queryables").json()["properties"]
    assert "datetime" in props
    assert props["datetime"]["type"] == "string"


def test_queryables_empty_catalog_still_valid(empty_client: TestClient):
    r = empty_client.get("/queryables")
    assert r.status_code == 200
    data = r.json()
    assert data["type"] == "object"
    assert "properties" in data


def test_queryables_base_id_matches_request(api_client: TestClient):
    data = api_client.get("/queryables").json()
    assert data["$id"].endswith("/queryables")


# ---------------------------------------------------------------------------
# /collections/{id}/queryables
# ---------------------------------------------------------------------------


def test_collection_queryables_returns_200(api_client: TestClient, db_path: Path):
    with CatalogDB(db_path) as db:
        cols = list(db.iter_collections())
    assert cols
    col_id = cols[0]["id"]

    r = api_client.get(f"/collections/{col_id}/queryables")
    assert r.status_code == 200


def test_collection_queryables_json_schema_structure(
    api_client: TestClient, db_path: Path
):
    with CatalogDB(db_path) as db:
        cols = list(db.iter_collections())
    col_id = cols[0]["id"]

    data = api_client.get(f"/collections/{col_id}/queryables").json()
    assert data["$schema"] == "https://json-schema.org/draft/2019-09/schema"
    assert data["type"] == "object"
    assert "properties" in data
    assert col_id in data["$id"]


def test_collection_queryables_not_found(api_client: TestClient):
    r = api_client.get("/collections/no-such-collection/queryables")
    assert r.status_code == 404


def test_collection_queryables_scoped_to_collection(
    api_client: TestClient, db_path: Path
):
    with CatalogDB(db_path) as db:
        cols = list(db.iter_collections())
    col_id = cols[0]["id"]

    data = api_client.get(f"/collections/{col_id}/queryables").json()
    # Should have datetime at minimum
    assert "datetime" in data["properties"]


# ---------------------------------------------------------------------------
# Links
# ---------------------------------------------------------------------------


def test_landing_page_advertises_queryables(api_client: TestClient):
    data = api_client.get("/").json()
    rels = {lnk["rel"] for lnk in data.get("links", [])}
    assert "http://www.opengis.net/def/rel/ogc/1.0/queryables" in rels


def test_collection_response_has_queryables_link(
    api_client: TestClient, db_path: Path
):
    with CatalogDB(db_path) as db:
        cols = list(db.iter_collections())
    col_id = cols[0]["id"]

    data = api_client.get(f"/collections/{col_id}").json()
    rels = {lnk["rel"] for lnk in data.get("links", [])}
    assert "http://www.opengis.net/def/rel/ogc/1.0/queryables" in rels
