"""make_item builds a STAC Item from scan metadata + a CollectionContext."""

from __future__ import annotations

from datetime import datetime, timezone

from esm_catalog.context import CollectionContext
from esm_catalog.stac.item import make_item


def test_make_item_basic(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    ctx = CollectionContext(
        experiment_id="exp-alpha", component="echam", collection_id="exp-alpha"
    )
    metadata = {
        "variable": "temp",
        "format": "netcdf",
        "file_size": 1,
        "datetime_start": datetime(2000, 1, 1, tzinfo=timezone.utc),
        "datetime_end": datetime(2000, 1, 1, tzinfo=timezone.utc),
    }
    item = make_item(f, metadata, ctx)
    assert item["type"] == "Feature"
    assert item["properties"]["variable"] == "temp"
    assert item["collection"] == "exp-alpha"
    assert item["assets"]["data"]["href"].startswith("file://")
