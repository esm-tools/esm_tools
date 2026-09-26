"""Unit tests for the push path: STAC client, shard reading, classification.

The client is exercised against an httpx MockTransport (no network); the shard
reader roundtrips through the scanner's own writer.
"""

import json
from datetime import datetime, timezone
from pathlib import Path

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
    assert "collection.json" in names  # top-level collection
    assert "s.parquet" in names  # shard found in items/ subdir
    assert "esm-catalog.json" not in names  # workspace state skipped, not an error


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
    write_shard(
        [_item("a", "c"), _item("b", "c")], UPath(catalog / "items" / "s.parquet")
    )

    calls: list[tuple[str, str]] = []

    def responder(request: httpx.Request) -> httpx.Response:
        calls.append((request.method, request.url.path))
        return httpx.Response(201, json={})

    client = StacClient(
        "https://host/api", "tok", transport=httpx.MockTransport(responder)
    )
    summary = pushmod.push_paths([catalog], client)

    assert summary.errors == []
    assert (summary.collections, summary.shards, summary.items) == (1, 1, 2)
    paths = [p for _, p in calls]
    assert "/api/collections" in paths  # collection upserted
    assert "/api/collections/c" in paths  # collection extent fetched
    assert "/api/collections/c/items/a" in paths  # merge-fetch + upsert, per item id
    assert "/api/collections/c/items/b" in paths
    assert not any("esm-catalog" in p for p in paths)  # state file never sent


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


# --------------------------------------------------------------------------- #
# Queryables delta (conditional registration).
# --------------------------------------------------------------------------- #


def _write_queryables(catalog: Path, properties: dict) -> None:
    (catalog).mkdir(parents=True, exist_ok=True)
    (catalog / "queryables.json").write_text(json.dumps({"properties": properties}))


def test_queryable_delta_only_new_keys(tmp_path, monkeypatch):
    catalog = tmp_path / "catalog"
    _write_queryables(
        catalog,
        {
            "nml__echam__namelist_echam__radctl__co2vmr": {"type": "number"},
            "nml__echam__namelist_echam__radctl__yr_perp": {"type": "integer"},
        },
    )
    # server already has co2vmr registered -> only yr_perp is new
    import esm_catalog.push as p

    monkeypatch.setattr(
        p,
        "registered_queryables",
        lambda url, verify: {"nml__echam__namelist_echam__radctl__co2vmr", "datetime"},
    )
    delta = p.queryable_delta(catalog, "https://host/api", True)
    assert delta is not None and delta.name == "queryables-delta.json"
    written = json.loads(delta.read_text())["properties"]
    assert set(written) == {"nml__echam__namelist_echam__radctl__yr_perp"}


def test_queryable_delta_none_when_all_registered(tmp_path, monkeypatch):
    catalog = tmp_path / "catalog"
    _write_queryables(catalog, {"nml__a__b__c__d": {"type": "number"}})
    import esm_catalog.push as p

    monkeypatch.setattr(
        p, "registered_queryables", lambda url, verify: {"nml__a__b__c__d"}
    )
    assert p.queryable_delta(catalog, "https://host/api", True) is None
    assert not (catalog / "queryables-delta.json").exists()


def test_queryable_delta_full_set_when_server_unreachable(tmp_path, monkeypatch):
    catalog = tmp_path / "catalog"
    _write_queryables(catalog, {"nml__a__b__c__d": {"type": "number"}})
    import esm_catalog.push as p

    def _boom(url, verify):
        raise RuntimeError("unreachable")

    monkeypatch.setattr(p, "registered_queryables", _boom)
    delta = p.queryable_delta(catalog, "https://host/api", True)
    assert delta is not None  # emits the full set rather than skipping silently
    assert set(json.loads(delta.read_text())["properties"]) == {"nml__a__b__c__d"}


def test_queryable_delta_absent_when_no_sidecar(tmp_path, monkeypatch):
    import esm_catalog.push as p

    monkeypatch.setattr(p, "registered_queryables", lambda url, verify: set())
    assert p.queryable_delta(tmp_path, "https://host/api", True) is None


def test_registered_queryables_parses_properties():
    def handler(request):
        return httpx.Response(
            200, json={"properties": {"a": {}, "b": {}, "datetime": {}}}
        )

    import esm_catalog.push as p

    # patch httpx.get used inside registered_queryables via a MockTransport client
    import httpx as _httpx

    real_get = _httpx.get

    def fake_get(url, **kw):
        return httpx.Response(
            200,
            json={"properties": {"a": {}, "b": {}}},
            request=httpx.Request("GET", url),
        )

    _httpx.get = fake_get
    try:
        assert p.registered_queryables("https://host/api", True) == {"a", "b"}
    finally:
        _httpx.get = real_get


def test_expand_paths_skips_queryables_sidecar(tmp_path):
    catalog = tmp_path / "catalog"
    catalog.mkdir()
    (catalog / "collection.json").write_text(
        json.dumps({"type": "Collection", "id": "c"})
    )
    (catalog / "queryables.json").write_text(json.dumps({"properties": {}}))
    (catalog / "esm-catalog.json").write_text(json.dumps({"scanned": {}}))

    names = [p.name for p in pushmod.expand_paths([catalog])]
    assert "collection.json" in names
    assert "queryables.json" not in names
    assert "esm-catalog.json" not in names


def _asset_item(item_id, collection, asset_key, dt="2020-01-01T00:00:00Z"):
    """A single-asset shard-row shape, as pushed for a growing (Item=stream) id."""
    return {
        "type": "Feature",
        "id": item_id,
        "collection": collection,
        "properties": {"start_datetime": dt, "end_datetime": dt, "datetime": dt},
        "assets": {asset_key: {"href": f"file:///{asset_key}.nc"}},
    }


def test_merge_item_accumulates_assets_from_existing_and_incoming():
    existing = _asset_item(
        "fesom-restart", "c", "oce_restart_2000", dt="2000-01-01T00:00:00Z"
    )
    incoming = [
        _asset_item("fesom-restart", "c", "ice_restart_2000", dt="2000-01-01T00:00:00Z")
    ]

    merged = pushmod.merge_item(existing, incoming)

    assert set(merged["assets"]) == {"oce_restart_2000", "ice_restart_2000"}
    assert merged["id"] == "fesom-restart"


def test_merge_item_widens_start_end_across_checkpoints():
    existing = _asset_item(
        "fesom-restart", "c", "oce_restart_2000", dt="2000-01-01T00:00:00Z"
    )
    incoming = [
        _asset_item("fesom-restart", "c", "oce_restart_2001", dt="2001-01-01T00:00:00Z")
    ]

    merged = pushmod.merge_item(existing, incoming)

    assert merged["properties"]["start_datetime"] == "2000-01-01T00:00:00+00:00"
    assert merged["properties"]["end_datetime"] == "2001-01-01T00:00:00+00:00"


def test_merge_item_first_push_has_no_existing():
    incoming = [_asset_item("fesom-restart", "c", "oce_restart_2000")]

    merged = pushmod.merge_item(None, incoming)

    assert merged["assets"] == incoming[0]["assets"]


def test_merge_item_repush_same_asset_key_is_idempotent():
    existing = _asset_item("fesom-restart", "c", "oce_restart_2000")
    incoming = [
        _asset_item("fesom-restart", "c", "oce_restart_2000")
    ]  # same key, re-pushed

    merged = pushmod.merge_item(existing, incoming)

    assert set(merged["assets"]) == {"oce_restart_2000"}  # not duplicated


def test_merge_item_drops_null_assets_from_columnar_round_trip():
    # Confirmed live against a real DKRZ push: stac_table_to_items reads a
    # shard back from ONE shared Arrow table, whose "assets" struct column is
    # the union of every asset key seen anywhere in the shard -- a row that
    # only ever had its own single key comes back with every *other* row's
    # key as a null placeholder. Pushing those straight through fails
    # pgstac's schema validation. merge_item must drop them, not merge them
    # in as if they were real data.
    row_a = _asset_item("echam-runoff", "c", "200001")
    row_a["assets"]["200002"] = None  # another row's key, seen only as null here
    row_a["assets"]["200003"] = None
    row_b = _asset_item("echam-runoff", "c", "200002")
    row_b["assets"]["200001"] = None
    row_b["assets"]["200003"] = None

    merged = pushmod.merge_item(None, [row_a, row_b])

    assert merged["assets"] == {
        "200001": row_a["assets"]["200001"],
        "200002": row_b["assets"]["200002"],
    }
    assert None not in merged["assets"].values()


def test_merge_item_removed_assets_tombstone_deletes_from_existing():
    existing = _asset_item("echam-ts", "c", "200001")
    existing["assets"]["200002"] = {"href": "file:///200002.nc"}
    tombstone = _asset_item(
        "echam-ts", "c", "200001"
    )  # placeholder row, real work is removed_assets
    tombstone["removed_assets"] = ["200002"]

    merged = pushmod.merge_item(existing, [tombstone])

    assert set(merged["assets"]) == {"200001"}
    assert "removed_assets" not in merged  # bookkeeping, never a real STAC field


def test_merge_item_removed_assets_tombstone_beats_an_add_in_the_same_batch():
    # Confirmed the resolution deliberately chosen: within one merge_item
    # call, removal always wins over addition of the same key, regardless of
    # which incoming row comes first -- add-then-rm and rm-then-add both net
    # out to "not present".
    add_row = _asset_item("echam-ts", "c", "200002")
    rm_row = _asset_item("echam-ts", "c", "200001")
    rm_row["removed_assets"] = ["200002"]

    merged_add_then_rm = pushmod.merge_item(None, [add_row, rm_row])
    merged_rm_then_add = pushmod.merge_item(None, [rm_row, add_row])

    assert "200002" not in merged_add_then_rm["assets"]
    assert "200002" not in merged_rm_then_add["assets"]


def test_merge_item_removed_assets_is_a_noop_for_a_key_not_present():
    existing = _asset_item("echam-ts", "c", "200001")
    tombstone = _asset_item("echam-ts", "c", "200001")
    tombstone["removed_assets"] = ["some-key-never-added"]

    merged = pushmod.merge_item(existing, [tombstone])

    assert set(merged["assets"]) == {"200001"}


def test_merge_item_preserves_existing_geometry_on_a_tombstone_only_push():
    """A real bug this session: base = dict(incoming[0]) unconditionally
    meant a tombstone-only row (no meaningful geometry/properties of its
    own, e.g. from `rm asset`) would silently overwrite the item's real
    published geometry with placeholder garbage. existing (when it looks
    like a real Item -- has a "collection") must win as the template."""
    existing = _asset_item("echam-ts", "c", "200001")
    existing["geometry"] = {
        "type": "Polygon",
        "coordinates": [[[0, 0], [1, 0], [1, 1], [0, 0]]],
    }
    existing["properties"]["frequency"] = "mon"

    tombstone = {
        "type": "Feature",
        "id": "echam-ts",
        "collection": "c",
        "geometry": {
            "type": "Point",
            "coordinates": [0.0, 0.0],
        },  # placeholder, not real
        "properties": {},
        "assets": {},
        "removed_assets": ["200001"],
    }

    merged = pushmod.merge_item(existing, [tombstone])

    assert merged["geometry"] == existing["geometry"]
    assert merged["properties"]["frequency"] == "mon"
    assert merged["assets"] == {}
