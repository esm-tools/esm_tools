"""Unit tests for StacClient's read/delete methods (get/put/delete CLI design).

Mirrors test_push.py's pattern: an httpx MockTransport, no network.
"""

import httpx
import pytest

from esm_catalog.client import StacClient, StacClientError


def _recording_client(responder):
    calls: list[httpx.Request] = []

    def handler(request: httpx.Request) -> httpx.Response:
        calls.append(request)
        return responder(request)

    client = StacClient(
        "https://host/api", "tok", transport=httpx.MockTransport(handler)
    )
    return client, calls


def test_list_collections_gets_collections_path():
    client, calls = _recording_client(
        lambda r: httpx.Response(200, json={"collections": [{"id": "c1"}]})
    )
    result = client.list_collections()

    assert len(calls) == 1
    assert calls[0].method == "GET"
    assert calls[0].url.path == "/api/collections"
    assert result == {"collections": [{"id": "c1"}]}


def test_list_collections_passes_limit():
    client, calls = _recording_client(lambda r: httpx.Response(200, json={}))
    client.list_collections(limit=5)

    assert calls[0].url.params["limit"] == "5"


def test_get_collection_gets_single_path():
    client, calls = _recording_client(
        lambda r: httpx.Response(200, json={"id": "c1", "type": "Collection"})
    )
    result = client.get_collection("c1")

    assert calls[0].method == "GET"
    assert calls[0].url.path == "/api/collections/c1"
    assert result == {"id": "c1", "type": "Collection"}


def test_get_collection_returns_none_on_404():
    # This branch's get_collection is also used by push's merge-before-write
    # logic, which needs "not found yet" distinguishable from an error.
    client, _ = _recording_client(lambda r: httpx.Response(404, text="not found"))
    assert client.get_collection("missing") is None


def test_delete_collection_deletes_path():
    client, calls = _recording_client(lambda r: httpx.Response(200, json={}))
    client.delete_collection("c1")

    assert calls[0].method == "DELETE"
    assert calls[0].url.path == "/api/collections/c1"


def test_delete_collection_raises_on_error():
    client, _ = _recording_client(lambda r: httpx.Response(400, text="bad"))
    with pytest.raises(StacClientError) as exc:
        client.delete_collection("c1")
    assert exc.value.status == 400


def test_list_items_gets_items_path_with_limit():
    client, calls = _recording_client(
        lambda r: httpx.Response(200, json={"features": []})
    )
    client.list_items("c1", limit=10)

    assert calls[0].method == "GET"
    assert calls[0].url.path == "/api/collections/c1/items"
    assert calls[0].url.params["limit"] == "10"


def test_get_item_gets_single_item_path():
    client, calls = _recording_client(
        lambda r: httpx.Response(200, json={"id": "i1", "type": "Feature"})
    )
    result = client.get_item("c1", "i1")

    assert calls[0].method == "GET"
    assert calls[0].url.path == "/api/collections/c1/items/i1"
    assert result == {"id": "i1", "type": "Feature"}


def test_delete_item_deletes_path():
    client, calls = _recording_client(lambda r: httpx.Response(200, json={}))
    client.delete_item("c1", "i1")

    assert calls[0].method == "DELETE"
    assert calls[0].url.path == "/api/collections/c1/items/i1"


def test_delete_item_raises_on_error():
    client, _ = _recording_client(lambda r: httpx.Response(500, text="boom"))
    with pytest.raises(StacClientError) as exc:
        client.delete_item("c1", "i1")
    assert exc.value.status == 500
