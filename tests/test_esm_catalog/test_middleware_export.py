"""Tests for request logging middleware and export utilities (PR-C1)."""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from fastapi.testclient import TestClient

from esm_catalog.api.app import create_app


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture()
def client() -> TestClient:
    return TestClient(create_app(catalogs=[]).app)


# ---------------------------------------------------------------------------
# Middleware: CorrelationIdMiddleware / RequestLoggingMiddleware
# ---------------------------------------------------------------------------


def test_request_completes_through_middleware(client: TestClient):
    r = client.get("/health")
    assert r.status_code == 200


def test_middleware_does_not_break_post(client: TestClient):
    r = client.post("/format")
    assert r.status_code == 200


def test_middleware_does_not_break_stac_endpoints(client: TestClient):
    r = client.get("/")
    assert r.status_code == 200
    assert r.json()["type"] == "Catalog"


def test_middleware_does_not_break_collections(client: TestClient):
    r = client.get("/collections")
    assert r.status_code == 200


def test_middleware_does_not_break_search(client: TestClient):
    r = client.post("/search", json={"limit": 1})
    assert r.status_code == 200


# ---------------------------------------------------------------------------
# Middleware unit tests (CorrelationIdMiddleware directly)
# ---------------------------------------------------------------------------


def test_correlation_id_added_to_request_state():
    """CorrelationIdMiddleware sets request.state.correlation_id."""
    import asyncio

    from starlette.applications import Starlette
    from starlette.requests import Request
    from starlette.responses import PlainTextResponse
    from starlette.testclient import TestClient as StarletteTestClient

    from esm_catalog.api.middleware import CorrelationIdMiddleware

    captured = {}

    async def homepage(request: Request):
        captured["cid"] = getattr(request.state, "correlation_id", None)
        return PlainTextResponse("ok")

    app = Starlette()
    app.add_middleware(CorrelationIdMiddleware)
    app.add_route("/", homepage)

    sc = StarletteTestClient(app)
    sc.get("/")
    assert captured["cid"] is not None
    assert len(captured["cid"]) == 8


# ---------------------------------------------------------------------------
# Export utilities
# ---------------------------------------------------------------------------


@pytest.fixture()
def sample_items() -> list[dict]:
    return [
        {
            "id": "item-001",
            "type": "Feature",
            "collection": "exp-alpha_echam",
            "bbox": [0.0, -45.0, 90.0, 45.0],
            "geometry": None,
            "properties": {
                "datetime": "2000-01-01T00:00:00Z",
                "experiment": "exp-alpha",
                "variable": "tas",
            },
            "assets": {},
            "links": [],
        }
    ]


def test_export_json(tmp_path: Path, sample_items: list[dict]):
    from esm_catalog.storage.export import export_json

    out = tmp_path / "items.json"
    export_json(sample_items, out)
    assert out.exists()
    data = json.loads(out.read_text())
    assert data["type"] == "FeatureCollection"
    assert len(data["features"]) == 1
    assert data["features"][0]["id"] == "item-001"


def test_export_json_creates_parent_dirs(tmp_path: Path, sample_items: list[dict]):
    from esm_catalog.storage.export import export_json

    out = tmp_path / "nested" / "deep" / "items.json"
    export_json(sample_items, out)
    assert out.exists()


def test_export_parquet(tmp_path: Path, sample_items: list[dict]):
    pytest.importorskip("pyarrow", reason="pyarrow not installed")
    from esm_catalog.storage.export import export_parquet

    out = tmp_path / "items.parquet"
    export_parquet(sample_items, out)
    assert out.exists()


def test_export_parquet_readable(tmp_path: Path, sample_items: list[dict]):
    pq = pytest.importorskip("pyarrow.parquet", reason="pyarrow not installed")
    from esm_catalog.storage.export import export_parquet

    out = tmp_path / "items.parquet"
    export_parquet(sample_items, out)
    table = pq.read_table(str(out))
    assert table.num_rows == 1
    ids = table.column("id").to_pylist()
    assert ids[0] == "item-001"
