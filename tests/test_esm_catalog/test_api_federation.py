"""Tests for catalog management (federation) API routes (PR-B4)."""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd
import pytest
import xarray as xr
from fastapi.testclient import TestClient

from esm_catalog.api.app import create_app
from esm_catalog.storage.duckdb import persist_tree


# ---------------------------------------------------------------------------
# Fixtures
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
    """Client with one pre-registered catalog."""
    return TestClient(create_app(catalogs=[str(db_path)]).app)


@pytest.fixture()
def empty_client() -> TestClient:
    """Client with no pre-registered catalogs."""
    return TestClient(create_app(catalogs=[]).app)


# ---------------------------------------------------------------------------
# GET /catalogs
# ---------------------------------------------------------------------------


def test_list_catalogs_empty(empty_client: TestClient):
    r = empty_client.get("/catalogs")
    assert r.status_code == 200
    data = r.json()
    assert data["catalogs"] == []
    assert data["total"] == 0


def test_list_catalogs_with_registered(api_client: TestClient):
    r = api_client.get("/catalogs")
    assert r.status_code == 200
    data = r.json()
    assert data["total"] == 1
    assert len(data["catalogs"]) == 1
    catalog = data["catalogs"][0]
    assert "id" in catalog
    assert "path" in catalog
    assert "status" in catalog


def test_list_catalogs_status_active_for_existing_file(api_client: TestClient):
    catalogs = api_client.get("/catalogs").json()["catalogs"]
    assert catalogs[0]["status"] == "active"


# ---------------------------------------------------------------------------
# GET /catalogs/{id}
# ---------------------------------------------------------------------------


def test_get_catalog(api_client: TestClient):
    catalog_id = api_client.get("/catalogs").json()["catalogs"][0]["id"]
    r = api_client.get(f"/catalogs/{catalog_id}")
    assert r.status_code == 200
    data = r.json()
    assert data["id"] == catalog_id
    assert "path" in data


def test_get_catalog_not_found(api_client: TestClient):
    r = api_client.get("/catalogs/nonexistent")
    assert r.status_code == 404


# ---------------------------------------------------------------------------
# POST /catalogs
# ---------------------------------------------------------------------------


def test_add_catalog(empty_client: TestClient, db_path: Path):
    r = empty_client.post(
        "/catalogs",
        json={"path": str(db_path), "name": "Test Catalog"},
    )
    assert r.status_code == 201
    data = r.json()
    assert "id" in data
    assert data["status"] == "active"
    assert data["name"] == "Test Catalog"


def test_add_catalog_duplicate_returns_409(empty_client: TestClient, db_path: Path):
    empty_client.post("/catalogs", json={"path": str(db_path)})
    r = empty_client.post("/catalogs", json={"path": str(db_path)})
    assert r.status_code == 409


def test_add_catalog_missing_file_is_allowed(empty_client: TestClient, tmp_path: Path):
    nonexistent = str(tmp_path / "ghost.duckdb")
    r = empty_client.post("/catalogs", json={"path": nonexistent})
    assert r.status_code == 201
    assert r.json()["status"] == "missing"


def test_add_catalog_appears_in_list(empty_client: TestClient, db_path: Path):
    empty_client.post("/catalogs", json={"path": str(db_path)})
    data = empty_client.get("/catalogs").json()
    assert data["total"] == 1


# ---------------------------------------------------------------------------
# PATCH /catalogs/{id}
# ---------------------------------------------------------------------------


def test_update_catalog_name(api_client: TestClient):
    catalog_id = api_client.get("/catalogs").json()["catalogs"][0]["id"]
    r = api_client.patch(
        f"/catalogs/{catalog_id}", json={"name": "Renamed Catalog"}
    )
    assert r.status_code == 200
    assert r.json()["name"] == "Renamed Catalog"


def test_update_catalog_not_found(api_client: TestClient):
    r = api_client.patch("/catalogs/nonexistent", json={"name": "X"})
    assert r.status_code == 404


# ---------------------------------------------------------------------------
# POST /catalogs/{id}/refresh
# ---------------------------------------------------------------------------


def test_refresh_catalog(api_client: TestClient):
    catalog_id = api_client.get("/catalogs").json()["catalogs"][0]["id"]
    r = api_client.post(f"/catalogs/{catalog_id}/refresh")
    assert r.status_code == 200
    data = r.json()
    assert data["refreshed"] is True
    assert "message" in data
    assert data["id"] == catalog_id


def test_refresh_catalog_not_found(api_client: TestClient):
    r = api_client.post("/catalogs/nonexistent/refresh")
    assert r.status_code == 404


# ---------------------------------------------------------------------------
# DELETE /catalogs/{id}
# ---------------------------------------------------------------------------


def test_delete_catalog(api_client: TestClient):
    catalog_id = api_client.get("/catalogs").json()["catalogs"][0]["id"]
    r = api_client.delete(f"/catalogs/{catalog_id}")
    assert r.status_code == 204


def test_delete_catalog_removes_from_list(api_client: TestClient):
    catalog_id = api_client.get("/catalogs").json()["catalogs"][0]["id"]
    api_client.delete(f"/catalogs/{catalog_id}")
    data = api_client.get("/catalogs").json()
    assert data["total"] == 0


def test_delete_catalog_not_found(api_client: TestClient):
    r = api_client.delete("/catalogs/nonexistent")
    assert r.status_code == 404
