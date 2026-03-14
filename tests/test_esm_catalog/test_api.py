"""Tests for the stac-fastapi DuckDB backend (api/client.py + api/app.py)."""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from fastapi.testclient import TestClient

from esm_catalog.api.app import create_app
from esm_catalog.storage.duckdb import CatalogDB


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _minimal_item(item_id: str, collection: str, experiment: str = "basic") -> dict:
    return {
        "type": "Feature",
        "stac_version": "1.0.0",
        "stac_extensions": [],
        "id": item_id,
        "collection": collection,
        "geometry": {
            "type": "Polygon",
            "coordinates": [[[-180, -90], [180, -90], [180, 90], [-180, 90], [-180, -90]]],
        },
        "bbox": [-180.0, -90.0, 180.0, 90.0],
        "properties": {
            "datetime": "2000-01-01T00:00:00Z",
            "experiment": experiment,
            "component": "fesom",
            "variable": "temp",
        },
        "assets": {
            "data": {"href": "/work/exp/output.nc", "type": "application/x-netcdf"}
        },
        "links": [],
    }


def _minimal_collection(collection_id: str, experiment: str = "basic") -> dict:
    return {
        "type": "Collection",
        "id": collection_id,
        "stac_version": "1.0.0",
        "description": f"Test collection {collection_id}",
        "links": [],
        "title": collection_id,
        "extent": {
            "spatial": {"bbox": [[-180, -90, 180, 90]]},
            "temporal": {"interval": [["2000-01-01T00:00:00Z", None]]},
        },
        "license": "proprietary",
    }


@pytest.fixture
def catalog_db(tmp_path):
    """Populated CatalogDB fixture."""
    db_path = tmp_path / "catalog.duckdb"
    with CatalogDB(db_path) as db:
        col = _minimal_collection("basic-fesom", experiment="basic")
        db.insert_collection(col)

        item1 = _minimal_item("temp-fesom-2000", "basic-fesom")
        db.insert_item(item1)
        db.update_collection_extent("basic-fesom", item1)
        db.upsert_collection_item_props("basic-fesom", item1)

        item2 = _minimal_item("salt-fesom-2000", "basic-fesom")
        item2["properties"]["variable"] = "salt"
        db.insert_item(item2)
        db.upsert_collection_item_props("basic-fesom", item2)

    return db_path


@pytest.fixture
def client(catalog_db):
    """TestClient for the stac-fastapi app."""
    api = create_app(catalogs=[catalog_db])
    return TestClient(api.app)


# ---------------------------------------------------------------------------
# Landing page
# ---------------------------------------------------------------------------

class TestLandingPage:
    def test_returns_200(self, client):
        r = client.get("/")
        assert r.status_code == 200

    def test_type_is_catalog(self, client):
        data = client.get("/").json()
        assert data["type"] == "Catalog"

    def test_has_conformance_link(self, client):
        data = client.get("/").json()
        rels = [l["rel"] for l in data["links"]]
        assert "conformance" in rels


# ---------------------------------------------------------------------------
# Conformance
# ---------------------------------------------------------------------------

class TestConformance:
    def test_returns_200(self, client):
        r = client.get("/conformance")
        assert r.status_code == 200

    def test_has_core_conformance(self, client):
        data = client.get("/conformance").json()
        classes = data["conformsTo"]
        assert any("core" in c for c in classes)


# ---------------------------------------------------------------------------
# Collections
# ---------------------------------------------------------------------------

class TestCollections:
    def test_returns_200(self, client):
        r = client.get("/collections")
        assert r.status_code == 200

    def test_contains_seeded_collection(self, client):
        data = client.get("/collections").json()
        ids = [c["id"] for c in data["collections"]]
        assert "basic-fesom" in ids

    def test_number_matched(self, client):
        data = client.get("/collections").json()
        assert data["numberMatched"] == 1
        assert data["numberReturned"] == 1


class TestGetCollection:
    def test_returns_200_for_known(self, client):
        r = client.get("/collections/basic-fesom")
        assert r.status_code == 200

    def test_id_matches(self, client):
        data = client.get("/collections/basic-fesom").json()
        assert data["id"] == "basic-fesom"

    def test_returns_404_for_unknown(self, client):
        r = client.get("/collections/does-not-exist")
        assert r.status_code == 404


# ---------------------------------------------------------------------------
# Items
# ---------------------------------------------------------------------------

class TestItemCollection:
    def test_returns_200(self, client):
        r = client.get("/collections/basic-fesom/items")
        assert r.status_code == 200

    def test_returns_both_items(self, client):
        data = client.get("/collections/basic-fesom/items?limit=100").json()
        assert len(data["features"]) == 2

    def test_context_block(self, client):
        data = client.get("/collections/basic-fesom/items").json()
        assert "context" in data
        assert data["context"]["matched"] == 2

    def test_unknown_collection_returns_empty(self, client):
        data = client.get("/collections/nonexistent/items").json()
        assert data["features"] == []


class TestGetItem:
    def test_returns_200_for_known(self, client):
        r = client.get("/collections/basic-fesom/items/temp-fesom-2000")
        assert r.status_code == 200

    def test_item_id_matches(self, client):
        data = client.get("/collections/basic-fesom/items/temp-fesom-2000").json()
        assert data["id"] == "temp-fesom-2000"

    def test_returns_404_for_unknown(self, client):
        r = client.get("/collections/basic-fesom/items/no-such-item")
        assert r.status_code == 404

    def test_item_id_with_dots(self, tmp_path):
        """Item IDs with dots (like 'a_ice.fesom.185001.687e4c') should work correctly.

        This tests a regression where item IDs containing dots were not found
        when accessed directly via GET /collections/{col}/items/{item_id},
        even though they appeared in the item listing.
        """
        from fastapi.testclient import TestClient
        from esm_catalog.api.app import create_app
        from esm_catalog.storage.duckdb import CatalogDB

        db_path = tmp_path / "catalog.duckdb"
        collection_id = "test-001-fesom"
        item_id_with_dots = "a_ice.fesom.185001.687e4c"

        with CatalogDB(db_path) as db:
            db.insert_collection(_minimal_collection(collection_id))
            item = _minimal_item(item_id_with_dots, collection_id)
            db.insert_item(item)

        api = create_app(catalogs=[db_path])
        client = TestClient(api.app)

        # Item should appear in listing
        r = client.get(f"/collections/{collection_id}/items")
        assert r.status_code == 200
        items = r.json()["features"]
        assert len(items) == 1
        assert items[0]["id"] == item_id_with_dots

        # Item should be accessible directly
        r = client.get(f"/collections/{collection_id}/items/{item_id_with_dots}")
        assert r.status_code == 200, f"Expected 200, got {r.status_code}: {r.json()}"
        assert r.json()["id"] == item_id_with_dots


# ---------------------------------------------------------------------------
# Search (GET)
# ---------------------------------------------------------------------------

class TestSearchGet:
    def test_returns_200(self, client):
        r = client.get("/search")
        assert r.status_code == 200

    def test_returns_all_items_without_filter(self, client):
        data = client.get("/search?limit=100").json()
        assert len(data["features"]) == 2

    def test_filter_by_collection(self, client):
        data = client.get("/search?collections=basic-fesom&limit=100").json()
        assert len(data["features"]) == 2

    def test_filter_by_unknown_collection(self, client):
        data = client.get("/search?collections=missing&limit=100").json()
        assert data["features"] == []

    def test_datetime_range_filter(self, client):
        data = client.get("/search?datetime=1999-01-01T00:00:00Z/2001-01-01T00:00:00Z&limit=100").json()
        assert len(data["features"]) == 2

    def test_datetime_range_no_match(self, client):
        data = client.get("/search?datetime=2010-01-01T00:00:00Z/2020-01-01T00:00:00Z&limit=100").json()
        assert data["features"] == []


# ---------------------------------------------------------------------------
# Search (POST)
# ---------------------------------------------------------------------------

class TestSearchPost:
    def test_returns_200(self, client):
        r = client.post("/search", json={})
        assert r.status_code == 200

    def test_filter_by_collection(self, client):
        r = client.post("/search", json={"collections": ["basic-fesom"], "limit": 100})
        data = r.json()
        assert len(data["features"]) == 2

    def test_filter_by_datetime(self, client):
        r = client.post("/search", json={"datetime": "1999-01-01T00:00:00Z/2001-01-01T00:00:00Z", "limit": 100})
        data = r.json()
        assert len(data["features"]) == 2

    def test_limit_is_respected(self, client):
        r = client.post("/search", json={"limit": 1})
        data = r.json()
        assert len(data["features"]) == 1


# ---------------------------------------------------------------------------
# Multi-catalog federation
# ---------------------------------------------------------------------------

class TestMultiCatalog:
    def test_collections_from_both_catalogs(self, tmp_path):
        db1 = tmp_path / "cat1.duckdb"
        db2 = tmp_path / "cat2.duckdb"

        with CatalogDB(db1) as db:
            db.insert_collection(_minimal_collection("exp1-fesom"))
            db.insert_item(_minimal_item("item-exp1", "exp1-fesom", experiment="exp1"))

        with CatalogDB(db2) as db:
            db.insert_collection(_minimal_collection("exp2-echam"))
            db.insert_item(_minimal_item("item-exp2", "exp2-echam", experiment="exp2"))

        api = create_app([db1, db2])
        tc = TestClient(api.app)

        data = tc.get("/collections").json()
        ids = {c["id"] for c in data["collections"]}
        assert ids == {"exp1-fesom", "exp2-echam"}

    def test_search_across_catalogs(self, tmp_path):
        db1 = tmp_path / "cat1.duckdb"
        db2 = tmp_path / "cat2.duckdb"

        with CatalogDB(db1) as db:
            db.insert_collection(_minimal_collection("exp1-fesom"))
            db.insert_item(_minimal_item("item-exp1", "exp1-fesom", experiment="exp1"))

        with CatalogDB(db2) as db:
            db.insert_collection(_minimal_collection("exp2-echam"))
            db.insert_item(_minimal_item("item-exp2", "exp2-echam", experiment="exp2"))

        api = create_app([db1, db2])
        tc = TestClient(api.app)

        data = tc.post("/search", json={"limit": 100}).json()
        assert len(data["features"]) == 2


# ---------------------------------------------------------------------------
# CORS headers
# ---------------------------------------------------------------------------

class TestCORS:
    def test_cors_header_present_on_get(self, client):
        r = client.get("/", headers={"Origin": "https://radiantearth.github.io"})
        assert "access-control-allow-origin" in r.headers

    def test_preflight_options(self, client):
        r = client.options(
            "/search",
            headers={
                "Origin": "https://radiantearth.github.io",
                "Access-Control-Request-Method": "POST",
            },
        )
        assert r.status_code in (200, 204)


# ---------------------------------------------------------------------------
# DuckDBCatalogClient — unit
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# CQL2-JSON filter parsing
# ---------------------------------------------------------------------------

class TestCQL2Filter:
    """Tests that CQL2-JSON filters sent by STAC Browser are applied correctly."""

    def test_post_search_cql2_equality(self, client):
        """POST /search with CQL2-JSON equality filter returns matching items."""
        payload = {
            "filter-lang": "cql2-json",
            "filter": {"op": "=", "args": [{"property": "variable"}, "temp"]},
        }
        r = client.post("/search", json=payload)
        assert r.status_code == 200
        data = r.json()
        assert data["type"] == "FeatureCollection"
        # All returned items must have variable=temp
        for feat in data["features"]:
            assert feat["properties"].get("variable") == "temp"

    def test_post_search_cql2_no_match(self, client):
        """CQL2-JSON filter with non-existent value returns empty feature collection."""
        payload = {
            "filter-lang": "cql2-json",
            "filter": {"op": "=", "args": [{"property": "variable"}, "nonexistent_var"]},
        }
        r = client.post("/search", json=payload)
        assert r.status_code == 200
        assert r.json()["features"] == []

    def test_post_search_cql2_and(self, client):
        """CQL2-JSON AND filter combines predicates correctly."""
        payload = {
            "filter-lang": "cql2-json",
            "filter": {
                "op": "and",
                "args": [
                    {"op": "=", "args": [{"property": "variable"}, "temp"]},
                    {"op": "=", "args": [{"property": "experiment"}, "basic"]},
                ],
            },
        }
        r = client.post("/search", json=payload)
        assert r.status_code == 200
        for feat in r.json()["features"]:
            assert feat["properties"].get("variable") == "temp"
            assert feat["properties"].get("experiment") == "basic"

    def test_landing_page_has_ogc_queryables_rel(self, client):
        """Landing page must advertise rel=OGC-queryables for STAC Browser filters."""
        r = client.get("/")
        links = r.json()["links"]
        rels = [lnk["rel"] for lnk in links]
        assert "http://www.opengis.net/def/rel/ogc/1.0/queryables" in rels

    def test_queryables_endpoint(self, client):
        """GET /queryables returns valid JSON Schema with properties."""
        r = client.get("/queryables")
        assert r.status_code == 200
        body = r.json()
        assert "properties" in body
        assert "variable" in body["properties"]
        assert body["$schema"] == "https://json-schema.org/draft/2019-09/schema"

    def test_collection_queryables_endpoint(self, client):
        """GET /collections/{id}/queryables returns scoped JSON Schema for the
        'Additional Filters' CQL2 builder when browsing a collection's items."""
        r = client.get("/collections/basic-fesom/queryables")
        assert r.status_code == 200
        body = r.json()
        assert body["$schema"] == "https://json-schema.org/draft/2019-09/schema"
        assert "properties" in body
        assert "variable" in body["properties"]
        assert body["$id"].endswith("/collections/basic-fesom/queryables")

    def test_collection_queryables_404_for_unknown(self, client):
        """GET /collections/{id}/queryables returns 404 for unknown collections."""
        r = client.get("/collections/does-not-exist/queryables")
        assert r.status_code == 404

    def test_collections_response_has_queryables_link(self, client):
        """GET /collections must include a queryables link so STAC Browser shows
        'Additional filters' in the 'Search for Collections' tab."""
        r = client.get("/collections")
        assert r.status_code == 200
        links = r.json().get("links", [])
        rels = [lnk["rel"] for lnk in links]
        assert "http://www.opengis.net/def/rel/ogc/1.0/queryables" in rels
        queryables_link = next(lnk for lnk in links if lnk["rel"] == "http://www.opengis.net/def/rel/ogc/1.0/queryables")
        assert queryables_link["href"].endswith("/queryables")

    def test_ogc_cql2_conformance(self, client):
        """Conformance must include OGC CQL2 classes for STAC Browser filter UI."""
        r = client.get("/conformance")
        classes = r.json()["conformsTo"]
        assert "http://www.opengis.net/spec/cql2/1.0/conf/cql2-json" in classes
        assert "http://www.opengis.net/spec/ogcapi-features-3/1.0/conf/filter" in classes


class TestDuckDBCatalogClientInit:
    def test_raises_on_empty_catalogs(self):
        from esm_catalog.api.client import DuckDBCatalogClient
        with pytest.raises(ValueError, match="at least one catalog"):
            DuckDBCatalogClient(catalogs=[])

    def test_warns_on_missing_path(self, tmp_path, caplog):
        from esm_catalog.api.client import DuckDBCatalogClient
        import logging
        # Should not raise, just warn
        client = DuckDBCatalogClient(catalogs=[str(tmp_path / "missing.duckdb")])
        assert client is not None
