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


def test_iter_items_yields_every_feature_on_one_page():
    def handler(request: httpx.Request) -> httpx.Response:
        return httpx.Response(200, json={"features": [{"id": "a"}, {"id": "b"}]})

    with _client_against(handler) as client:
        assert [f["id"] for f in client.iter_items("asterix-001")] == ["a", "b"]


def test_iter_items_follows_next_link_across_pages():
    calls = []

    def handler(request: httpx.Request) -> httpx.Response:
        calls.append(str(request.url))
        if "page=2" not in str(request.url):
            return httpx.Response(
                200,
                json={
                    "features": [{"id": "a"}],
                    "links": [
                        {
                            "rel": "next",
                            "href": "https://stac.example/api/collections/asterix-001/items?page=2",
                        }
                    ],
                },
            )
        return httpx.Response(200, json={"features": [{"id": "b"}], "links": []})

    with _client_against(handler) as client:
        assert [f["id"] for f in client.iter_items("asterix-001")] == ["a", "b"]
    assert len(calls) == 2


def test_iter_items_passes_cql2_filter_on_first_page_only():
    seen = []

    def handler(request: httpx.Request) -> httpx.Response:
        seen.append(dict(request.url.params))
        return httpx.Response(200, json={"features": []})

    with _client_against(handler) as client:
        list(client.iter_items("asterix-001", cql2_filter="variable='temp2'"))

    assert seen[0]["filter"] == "variable='temp2'"
