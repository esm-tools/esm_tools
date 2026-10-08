"""reroot_experiment: exact-substring href rewrite, scoped to assets only."""

from __future__ import annotations

import json
from datetime import datetime, timezone

import pystac
from upath import UPath

from esm_catalog.storage.geoparquet import write_shard
from esm_catalog.workflow_reroot import reroot_experiment


def _write_collection(catalog_dir, old_root):
    collection_dict = {
        "type": "Collection",
        "id": "exp1",
        "description": f"notes mentioning {old_root} in prose, left untouched",
        "assets": {
            "runscript": {"href": f"{old_root}/config/exp1_finished_config.yaml"},
            "readme": {"href": "README.md"},
        },
        "extent": {
            "spatial": {"bbox": [[0, 0, 0, 0]]},
            "temporal": {"interval": [[None, None]]},
        },
        "links": [],
        "license": "CC-BY-4.0",
    }
    (catalog_dir / "collection.json").write_text(json.dumps(collection_dict))
    return collection_dict


def _write_item_shard(catalog_dir, old_root):
    items_dir = catalog_dir / "items"
    items_dir.mkdir()
    item = pystac.Item(
        id="echam-outdata",
        geometry={
            "type": "Polygon",
            "coordinates": [[[0, 0], [1, 0], [1, 1], [0, 1], [0, 0]]],
        },
        bbox=[0, 0, 1, 1],
        datetime=datetime(2000, 1, 1, tzinfo=timezone.utc),
        properties={},
    )
    item.collection_id = "exp1"
    item.add_asset(
        "data",
        pystac.Asset(href=f"file://{old_root}/outdata/echam/sst.nc"),
    )
    shard_path = UPath(str(items_dir / "shard.parquet"))
    write_shard([item], shard_path)
    return shard_path


def test_dry_run_reports_without_writing(tmp_path):
    old_root, new_root = "/old/storehouse/exp1", "/new/storehouse/exp1"
    catalog_dir = UPath(str(tmp_path))
    original = _write_collection(catalog_dir, old_root)

    report = reroot_experiment(catalog_dir, old_root, new_root, dry_run=True)

    assert report.collection_assets_changed == 1
    on_disk = json.loads((catalog_dir / "collection.json").read_text())
    assert on_disk == original  # untouched


def test_rewrites_collection_assets(tmp_path):
    old_root, new_root = "/old/storehouse/exp1", "/new/storehouse/exp1"
    catalog_dir = UPath(str(tmp_path))
    _write_collection(catalog_dir, old_root)

    report = reroot_experiment(catalog_dir, old_root, new_root)

    assert report.collection_assets_changed == 1
    rewritten = json.loads((catalog_dir / "collection.json").read_text())
    assert rewritten["assets"]["runscript"]["href"] == f"{new_root}/config/exp1_finished_config.yaml"
    assert rewritten["assets"]["readme"]["href"] == "README.md"  # unaffected, no match
    assert old_root in rewritten["description"]  # prose is never touched


def test_rewrites_item_asset_hrefs_in_shards(tmp_path):
    old_root, new_root = "/old/storehouse/exp1", "/new/storehouse/exp1"
    catalog_dir = UPath(str(tmp_path))
    shard_path = _write_item_shard(catalog_dir, old_root)

    report = reroot_experiment(catalog_dir, old_root, new_root)

    assert report.item_assets_changed == 1
    assert shard_path in report.shards_touched

    from esm_catalog.storage.geoparquet import read_shard
    from stac_geoparquet.arrow import stac_table_to_items

    rewritten_items = list(stac_table_to_items(read_shard(shard_path)))
    assert rewritten_items[0]["assets"]["data"]["href"] == f"file://{new_root}/outdata/echam/sst.nc"


def test_no_match_reports_zero_and_writes_nothing(tmp_path):
    catalog_dir = UPath(str(tmp_path))
    _write_collection(catalog_dir, "/old/storehouse/exp1")

    report = reroot_experiment(catalog_dir, "/no/such/path", "/new/path")

    assert report.total_changed == 0
    assert report.shards_touched == []
