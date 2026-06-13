"""Tests for experiment routes + paleo presets (PR-B3)."""

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
# /experiments
# ---------------------------------------------------------------------------


def test_list_experiments_empty(empty_client: TestClient):
    r = empty_client.get("/experiments")
    assert r.status_code == 200
    data = r.json()
    assert data["experiments"] == []
    assert data["numberMatched"] == 0


def test_list_experiments(api_client: TestClient):
    r = api_client.get("/experiments")
    assert r.status_code == 200
    data = r.json()
    assert data["numberMatched"] >= 1
    exp = data["experiments"][0]
    assert "id" in exp
    assert "href" in exp


def test_list_experiments_has_pagination_links(api_client: TestClient):
    data = api_client.get("/experiments").json()
    rels = {lnk["rel"] for lnk in data.get("links", [])}
    assert "self" in rels
    assert "first" in rels


def test_get_experiment(api_client: TestClient, db_path: Path):
    with CatalogDB(db_path) as db:
        cols = list(db.iter_collections())
    assert cols
    exp_id = cols[0].get("experiment") or cols[0]["id"]

    r = api_client.get(f"/experiments/{exp_id}")
    assert r.status_code == 200
    data = r.json()
    assert data["type"] == "Catalog"
    assert data["id"] == exp_id


def test_get_experiment_not_found(api_client: TestClient):
    r = api_client.get("/experiments/no-such-experiment")
    assert r.status_code == 404


def test_collection_experiment_shortcut(api_client: TestClient, db_path: Path):
    with CatalogDB(db_path) as db:
        cols = list(db.iter_collections())
    col_id = cols[0]["id"]

    r = api_client.get(f"/collections/{col_id}/experiment")
    assert r.status_code == 200
    assert r.json()["type"] == "Catalog"


def test_collection_experiment_not_found(api_client: TestClient):
    r = api_client.get("/collections/no-such-col/experiment")
    assert r.status_code == 404


# ---------------------------------------------------------------------------
# /paleo-presets
# ---------------------------------------------------------------------------


def test_get_paleo_presets(api_client: TestClient):
    r = api_client.get("/paleo-presets")
    assert r.status_code == 200
    data = r.json()
    assert "presets" in data
    assert len(data["presets"]) >= 1


def test_paleo_presets_structure(api_client: TestClient):
    presets = api_client.get("/paleo-presets").json()["presets"]
    for p in presets:
        assert "id" in p
        assert "name" in p
        assert "display" in p
        assert "years_bp" in p


def test_paleo_presets_contains_lgm(api_client: TestClient):
    presets = api_client.get("/paleo-presets").json()["presets"]
    ids = {p["id"] for p in presets}
    assert "lgm" in ids


def test_add_and_delete_paleo_preset(api_client: TestClient):
    body = {
        "id": "test-period",
        "name": "Test Period",
        "display": "42.0 ka",
        "years_bp": 42000,
        "description": "A test preset",
    }
    r = api_client.post("/paleo-presets", json=body)
    assert r.status_code == 200
    assert r.json()["preset"]["id"] == "test-period"

    presets = api_client.get("/paleo-presets").json()["presets"]
    assert any(p["id"] == "test-period" for p in presets)

    r = api_client.delete("/paleo-presets/test-period")
    assert r.status_code == 200


def test_delete_builtin_preset_fails(api_client: TestClient):
    r = api_client.delete("/paleo-presets/lgm")
    assert r.status_code == 404
