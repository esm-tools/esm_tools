"""StacClient request-building tests (no live server: httpx.MockTransport)."""

from __future__ import annotations

import httpx

from esm_catalog.client import StacClient


def _client_against(handler) -> StacClient:
    return StacClient(
        api_url="https://stac.example/api",
        token="test-token",
        transport=httpx.MockTransport(handler),
    )


def test_list_items_without_filter_omits_filter_params():
    seen = {}

    def handler(request: httpx.Request) -> httpx.Response:
        seen["params"] = dict(request.url.params)
        return httpx.Response(200, json={"features": []})

    with _client_against(handler) as client:
        client.list_items("asterix-001")

    assert "filter" not in seen["params"]
    assert "filter-lang" not in seen["params"]


def test_list_items_with_filter_passes_cql2_text():
    seen = {}

    def handler(request: httpx.Request) -> httpx.Response:
        seen["params"] = dict(request.url.params)
        return httpx.Response(200, json={"features": []})

    with _client_against(handler) as client:
        client.list_items("asterix-001", cql2_filter="variable='temp2'")

    assert seen["params"]["filter"] == "variable='temp2'"
    assert seen["params"]["filter-lang"] == "cql2-text"


def test_list_items_with_filter_and_limit():
    seen = {}

    def handler(request: httpx.Request) -> httpx.Response:
        seen["params"] = dict(request.url.params)
        return httpx.Response(200, json={"features": []})

    with _client_against(handler) as client:
        client.list_items("asterix-001", limit=5, cql2_filter="variable='u'")

    assert seen["params"]["limit"] == "5"
    assert seen["params"]["filter"] == "variable='u'"
