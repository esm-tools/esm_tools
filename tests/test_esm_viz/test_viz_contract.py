"""
Contract tests for the ESM Visualization Service API.

These tests verify the API contract that the STAC Browser frontend depends on.
The frontend (DataPreview.vue) expects specific endpoints, status codes, and
content types from the viz server.

Endpoint contract:
    GET /health                         -> 200, JSON {status, service, version}
    GET /preview/{item_id}.json?stac_api=...  -> 200, JSON {variables, dimensions, item_id, href}
    GET /preview/{item_id}.png?var=...&stac_api=... -> 200, image/png
    GET /preview/{item_id}/panel?stac_api=...  -> 307 redirect to /_panel/?item_id=...
    GET /_panel/?item_id=...&stac_api=...      -> 200, text/html (Panel app)
"""

from unittest.mock import MagicMock, patch

import pytest
import xarray as xr
import numpy as np
from fastapi.testclient import TestClient

from esm_viz.app import app


ITEM_ID = "test-item.echam.185001.abc123"
STAC_API = "http://localhost:23100"
COLLECTION_ID = "test-collection"


@pytest.fixture
def client():
    return TestClient(app)


@pytest.fixture
def mock_stac_and_data():
    """Mock STAC item fetching and dataset opening for isolated tests."""
    fake_item = {
        "type": "Feature",
        "id": ITEM_ID,
        "collection": COLLECTION_ID,
        "assets": {
            "data": {
                "href": "/fake/path/to/data.nc",
                "type": "application/x-netcdf",
            }
        },
    }

    ds = xr.Dataset(
        {
            "temp": (["time", "lat", "lon"], np.random.rand(3, 10, 20)),
            "precip": (["time", "lat", "lon"], np.random.rand(3, 10, 20)),
        },
        coords={
            "time": range(3),
            "lat": np.linspace(-90, 90, 10),
            "lon": np.linspace(-180, 180, 20),
        },
    )

    with (
        patch("esm_viz.app._fetch_stac_item", return_value=fake_item),
        patch("esm_viz.app.open_data", return_value=ds),
        patch("esm_viz.app.is_unstructured", return_value=False),
    ):
        yield


class TestHealthEndpoint:
    def test_returns_200(self, client):
        r = client.get("/health")
        assert r.status_code == 200

    def test_response_shape(self, client):
        data = client.get("/health").json()
        assert data["status"] == "healthy"
        assert "version" in data
        assert "service" in data


class TestPreviewMetadata:
    def test_returns_json_with_expected_fields(self, client, mock_stac_and_data):
        r = client.get(
            f"/preview/{ITEM_ID}.json",
            params={"stac_api": STAC_API, "collection_id": COLLECTION_ID},
        )
        assert r.status_code == 200
        data = r.json()
        assert "variables" in data
        assert "dimensions" in data
        assert data["item_id"] == ITEM_ID
        assert "href" in data

    def test_nonexistent_item_returns_404(self, client):
        with patch("esm_viz.app._fetch_stac_item", side_effect=Exception("not found")):
            r = client.get(
                "/preview/nonexistent.json",
                params={"stac_api": STAC_API},
            )
            assert r.status_code == 500


class TestPreviewPng:
    def test_returns_png_image(self, client, mock_stac_and_data):
        with patch(
            "esm_viz.app.generate_preview_png", return_value=b"\x89PNG fake"
        ):
            r = client.get(
                f"/preview/{ITEM_ID}.png",
                params={
                    "var": "temp",
                    "stac_api": STAC_API,
                    "time": 0,
                    "cmap": "viridis",
                    "collection_id": COLLECTION_ID,
                },
            )
            assert r.status_code == 200
            assert r.headers["content-type"] == "image/png"

    def test_invalid_variable_returns_400(self, client, mock_stac_and_data):
        r = client.get(
            f"/preview/{ITEM_ID}.png",
            params={"var": "nonexistent_var", "stac_api": STAC_API},
        )
        assert r.status_code == 400


class TestPreviewPanelRedirect:
    """The /preview/{item_id}/panel endpoint must redirect to /_panel/ with query params."""

    def test_redirects_to_internal_panel(self, client):
        r = client.get(
            f"/preview/{ITEM_ID}/panel",
            params={"stac_api": STAC_API, "collection_id": COLLECTION_ID},
            follow_redirects=False,
        )
        assert r.status_code == 307
        location = r.headers["location"]
        assert "/_panel" in location
        assert f"item_id={ITEM_ID}" in location or "item_id=" in location
        assert "stac_api=" in location
        assert "collection_id=" in location

    def test_redirect_without_optional_params(self, client):
        r = client.get(
            f"/preview/{ITEM_ID}/panel",
            params={"stac_api": STAC_API},
            follow_redirects=False,
        )
        assert r.status_code == 307
        location = r.headers["location"]
        assert "collection_id" not in location

    def test_does_not_return_json_404(self, client):
        """Regression: previously /panel/?item_id=... returned {"detail":"Not Found"}."""
        r = client.get(
            f"/preview/{ITEM_ID}/panel",
            params={"stac_api": STAC_API},
            follow_redirects=False,
        )
        assert r.status_code != 404


class TestCors:
    def test_cors_headers_on_health(self, client):
        r = client.get("/health", headers={"Origin": "http://localhost:8080"})
        assert "access-control-allow-origin" in r.headers

    def test_cors_headers_on_preview(self, client, mock_stac_and_data):
        r = client.get(
            f"/preview/{ITEM_ID}.json",
            params={"stac_api": STAC_API},
            headers={"Origin": "http://localhost:8080"},
        )
        assert "access-control-allow-origin" in r.headers


class TestCollectionPanelRedirect:
    """The /preview/collection/{id}/panel endpoint must redirect to /_panel with collection params."""

    def test_redirects_with_collection_id(self, client):
        r = client.get(
            f"/preview/collection/{COLLECTION_ID}/panel",
            params={"stac_api": STAC_API},
            follow_redirects=False,
        )
        assert r.status_code == 307
        location = r.headers["location"]
        assert "/_panel" in location
        assert f"collection_id={COLLECTION_ID}" in location
        assert "stac_api=" in location
        # Must NOT have item_id (this is collection-level)
        assert "item_id" not in location

    def test_collection_redirect_without_stac_api(self, client):
        r = client.get(
            f"/preview/collection/{COLLECTION_ID}/panel",
            follow_redirects=False,
        )
        assert r.status_code == 307
        location = r.headers["location"]
        assert f"collection_id={COLLECTION_ID}" in location


class TestCompareRedirect:
    """The /preview/compare/panel endpoint must redirect to /_panel with both collection params."""

    COLLECTION_A = "basic-001-echam"
    COLLECTION_B = "branchoff-002-fesom"

    def test_redirects_with_both_collections(self, client):
        r = client.get(
            "/preview/compare/panel",
            params={
                "collection_a": self.COLLECTION_A,
                "collection_b": self.COLLECTION_B,
                "stac_api": STAC_API,
            },
            follow_redirects=False,
        )
        assert r.status_code == 307
        location = r.headers["location"]
        assert "/_panel" in location
        assert "compare=1" in location
        assert f"collection_a={self.COLLECTION_A}" in location
        assert f"collection_b={self.COLLECTION_B}" in location
        assert "stac_api=" in location

    def test_requires_both_collections(self, client):
        """collection_a and collection_b are required query params."""
        r = client.get(
            "/preview/compare/panel",
            params={"collection_a": self.COLLECTION_A, "stac_api": STAC_API},
            follow_redirects=False,
        )
        # FastAPI returns 422 for missing required query params
        assert r.status_code == 422

    def test_does_not_conflict_with_item_panel(self, client):
        """Ensure /preview/compare/panel doesn't match /preview/{item_id}/panel."""
        r = client.get(
            f"/preview/{ITEM_ID}/panel",
            params={"stac_api": STAC_API},
            follow_redirects=False,
        )
        assert r.status_code == 307
        location = r.headers["location"]
        # Should be an item redirect, not a compare redirect
        assert "item_id=" in location
        assert "compare" not in location


class TestRootEndpoint:
    def test_lists_endpoints(self, client):
        data = client.get("/").json()
        endpoints = data["endpoints"]
        assert "/preview/{item_id}.png" in endpoints
        assert "/preview/{item_id}.json" in endpoints
        assert "/preview/{item_id}/panel" in endpoints
        assert "/preview/collection/{collection_id}/panel" in endpoints
        assert "/preview/compare/panel" in endpoints
        assert "/health" in endpoints
