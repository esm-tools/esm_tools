from __future__ import annotations
"""Shared pytest fixtures for ESM-Catalog tests."""
import pytest
from fastapi.testclient import TestClient

from esm_catalog.api.app import create_app
from esm_catalog.storage.duckdb import CatalogDB


def make_col(collection_id: str, experiment: str) -> dict:
    """Minimal valid STAC Collection dict with experiment field."""
    return {
        "type": "Collection",
        "id": collection_id,
        "stac_version": "1.0.0",
        "description": f"Test {collection_id}",
        "title": collection_id,
        "experiment": experiment,
        "model": collection_id.split("-")[-1],
        "links": [{"rel": "parent", "href": f"#{experiment}",
                   "type": "application/json"}],
        "extent": {
            "spatial": {"bbox": [[-180, -90, 180, 90]]},
            "temporal": {"interval": [["2000-01-01T00:00:00Z", None]]},
        },
        "license": "proprietary",
    }


@pytest.fixture
def two_exp_db(tmp_path):
    """DB with exp-alpha (fesom+echam) and exp-beta (fesom)."""
    db_path = tmp_path / "catalog.duckdb"
    with CatalogDB(db_path) as db:
        db.insert_collection(make_col("exp-alpha-fesom", "exp-alpha"))
        db.insert_collection(make_col("exp-alpha-echam", "exp-alpha"))
        db.insert_collection(make_col("exp-beta-fesom", "exp-beta"))
    return db_path


@pytest.fixture
def api_client(two_exp_db):
    return TestClient(create_app(catalogs=[two_exp_db]).app)
