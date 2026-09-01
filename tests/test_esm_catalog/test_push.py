"""Unit tests for the push path: STAC client, shard reading, classification.

The client is exercised against an httpx MockTransport (no network); the shard
reader roundtrips through the scanner's own writer.
"""

import json
from datetime import datetime, timezone

import httpx
import pystac
import pytest
from upath import UPath

from esm_catalog import push as pushmod
from esm_catalog.client import StacClient, StacClientError
from esm_catalog.storage.geoparquet import write_shard


# --------------------------------------------------------------------------- #
# Helpers.
# --------------------------------------------------------------------------- #


def _item(item_id: str, collection: str) -> pystac.Item:
    item = pystac.Item(
        id=item_id,
        geometry={"type": "Point", "coordinates": [0.0, 0.0]},
        bbox=[0.0, 0.0, 0.0, 0.0],
        datetime=datetime(2020, 1, 1, tzinfo=timezone.utc),
        properties={},
        collection=collection,
    )
    # A non-empty assets struct: parquet cannot serialise an empty one.
    item.add_asset("data", pystac.Asset(href=f"file:///{item_id}.nc"))
    return item


def _recording_client(responder):
    """A StacClient whose requests are captured; *responder* returns a Response."""
    calls: list[httpx.Request] = []

    def handler(request: httpx.Request) -> httpx.Response:
        calls.append(request)
        return responder(request)

    client = StacClient(
        "https://host/api", "tok", transport=httpx.MockTransport(handler)
    )
    return client, calls


# --------------------------------------------------------------------------- #
# StacClient.
# --------------------------------------------------------------------------- #


def test_bulk_items_posts_keyed_upsert_to_api_path():
    client, calls = _recording_client(lambda r: httpx.Response(200, json={}))
    client.bulk_items("c1", [{"id": "a"}, {"id": "b"}], method="upsert")

    assert len(calls) == 1
    req = calls[0]
    assert req.method == "POST"
    # The /api prefix must survive (the base_url join gotcha).
    assert req.url.path == "/api/collections/c1/bulk_items"
    body = json.loads(req.content)
    assert body["method"] == "upsert"
    assert set(body["items"]) == {"a", "b"}
    assert body["items"]["a"] == {"id": "a"}
    assert req.headers["authorization"] == "Bearer tok"


def test_bulk_items_empty_is_noop():
    client, calls = _recording_client(lambda r: httpx.Response(200, json={}))
    client.bulk_items("c1", [])
    assert calls == []


def test_bulk_items_raises_on_error():
    client, _ = _recording_client(lambda r: httpx.Response(400, text="bad"))
    with pytest.raises(StacClientError) as exc:
        client.bulk_items("c1", [{"id": "a"}])
    assert exc.value.status == 400


def test_upsert_collection_falls_back_to_put_on_conflict():
    def responder(request: httpx.Request) -> httpx.Response:
        return httpx.Response(409 if request.method == "POST" else 200, json={})

    client, calls = _recording_client(responder)
    client.upsert_collection({"id": "c1", "type": "Collection"})

    methods = [(c.method, c.url.path) for c in calls]
    assert methods == [
        ("POST", "/api/collections"),
        ("PUT", "/api/collections/c1"),
    ]


def test_upsert_item_requires_collection():
    client, _ = _recording_client(lambda r: httpx.Response(200, json={}))
    with pytest.raises(ValueError):
        client.upsert_item({"id": "i1", "type": "Feature"})


# --------------------------------------------------------------------------- #
# Shard reading (roundtrip through the scanner's writer).
# --------------------------------------------------------------------------- #


def test_shard_items_grouped_by_collection(tmp_path):
    shard = tmp_path / "s.parquet"
    write_shard(
        [_item("a", "c1"), _item("b", "c1"), _item("c", "c2")],
        UPath(shard),
    )
    grouped = pushmod.shard_items_by_collection(shard)
    assert {k: sorted(i["id"] for i in v) for k, v in grouped.items()} == {
        "c1": ["a", "b"],
        "c2": ["c"],
    }
    assert pushmod.count_items(shard) == 3


# --------------------------------------------------------------------------- #
# Classification and ordering.
# --------------------------------------------------------------------------- #


def test_classify_file(tmp_path):
    coll = tmp_path / "coll.json"
    coll.write_text(json.dumps({"type": "Collection", "id": "c"}))
    item = tmp_path / "item.json"
    item.write_text(json.dumps({"type": "Feature", "id": "i", "collection": "c"}))
    shard = tmp_path / "s.geoparquet"
    shard.write_bytes(b"")
    junk = tmp_path / "x.txt"
    junk.write_text("nope")

    assert pushmod.classify_file(coll) == "collection"
    assert pushmod.classify_file(item) == "item"
    assert pushmod.classify_file(shard) == "shard"
    assert pushmod.classify_file(junk) == "unknown"


def test_expand_paths_orders_collections_first(tmp_path):
    (tmp_path / "s.parquet").write_bytes(b"")
    (tmp_path / "item.json").write_text(json.dumps({"type": "Feature", "id": "i"}))
    (tmp_path / "coll.json").write_text(json.dumps({"type": "Collection", "id": "c"}))

    kinds = [pushmod.classify_file(p) for p in pushmod.expand_paths([tmp_path])]
    assert kinds == ["collection", "item", "shard"]


def test_expand_paths_recurses_and_skips_workspace_state(tmp_path):
    # The scanner's on-disk layout: collection.json at the catalog root, shards
    # under items/, and the esm-catalog.json workspace-state file alongside.
    catalog = tmp_path / "catalog"
    (catalog / "items").mkdir(parents=True)
    (catalog / "collection.json").write_text(
        json.dumps({"type": "Collection", "id": "c"})
    )
    (catalog / "esm-catalog.json").write_text(
        json.dumps({"experiment_id": "c", "scanned": {}})
    )
    (catalog / "items" / "s.parquet").write_bytes(b"")

    names = [p.name for p in pushmod.expand_paths([catalog])]
    assert "collection.json" in names          # top-level collection
    assert "s.parquet" in names                # shard found in items/ subdir
    assert "esm-catalog.json" not in names     # workspace state skipped, not an error


def test_push_paths_over_catalog_layout(tmp_path):
    # End-to-end over the scanner's real layout: collection.json at the root,
    # shard under items/, esm-catalog.json state file alongside. Drives the whole
    # push and observes the HTTP calls (this is the scenario that failed on Albedo).
    catalog = tmp_path / "catalog"
    (catalog / "items").mkdir(parents=True)
    (catalog / "collection.json").write_text(
        json.dumps({"id": "c", "type": "Collection"})
    )
    (catalog / "esm-catalog.json").write_text(
        json.dumps({"experiment_id": "c", "scanned": {}})
    )
    write_shard([_item("a", "c"), _item("b", "c")], UPath(catalog / "items" / "s.parquet"))

    calls: list[tuple[str, str]] = []

    def responder(request: httpx.Request) -> httpx.Response:
        calls.append((request.method, request.url.path))
        return httpx.Response(201, json={})

    client = StacClient("https://host/api", "tok", transport=httpx.MockTransport(responder))
    summary = pushmod.push_paths([catalog], client)

    assert summary.errors == []
    assert (summary.collections, summary.shards, summary.items) == (1, 1, 2)
    paths = [p for _, p in calls]
    assert "/api/collections" in paths                    # collection upserted
    assert "/api/collections/c/bulk_items" in paths       # shard items bulk-pushed
    assert not any("esm-catalog" in p for p in paths)     # state file never sent


def test_upsert_reports_redirect_actionably():
    # A bare "HTTP 308" is unhelpful; a redirect must hint at the http/https or
    # trailing-slash cause (the real-world failure from an http:// server_url).
    def responder(request: httpx.Request) -> httpx.Response:
        return httpx.Response(308, headers={"location": "https://host/api/collections"})

    client, _ = _recording_client(responder)
    with pytest.raises(StacClientError) as exc:
        client.upsert_collection({"id": "c", "type": "Collection"})
    assert exc.value.status == 308
    msg = str(exc.value).lower()
    assert "redirect" in msg and "https" in msg
