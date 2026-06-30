"""Tests for make_item."""

from __future__ import annotations

from datetime import datetime, timezone

from pystac.item import Item as PySTACItem

from esm_catalog.context import CollectionContext
from esm_catalog.item import make_item


def _ctx():
    return CollectionContext(
        experiment_id="exp-alpha", component="echam", collection_id="exp-alpha"
    )


def _metadata(**kwargs):
    base = {
        "variable": "temp",
        "format": "netcdf",
        "datetime_start": datetime(2000, 1, 1, tzinfo=timezone.utc),
        "datetime_end": datetime(2000, 1, 1, tzinfo=timezone.utc),
    }
    base.update(kwargs)
    return base


def test_item_is_pystacitem(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_item(f, _metadata(), _ctx())
    assert isinstance(item, PySTACItem)


def test_item_basic_fields(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_item(f, _metadata(), _ctx())
    assert item.extra_fields["type"] == "Feature"
    assert item.properties["variable"] == "temp"
    assert item.properties["format"] == "netcdf"
    assert item.collection_id == "exp-alpha"
    assert item.assets["data"].href.startswith("file://")


def test_item_single_time_sets_datetime(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_item(f, _metadata(), _ctx())
    assert item.datetime is not None
    assert "start_datetime" not in dir(item)


def test_item_time_range_sets_interval(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    meta = _metadata(
        datetime_start=datetime(2000, 1, 1, tzinfo=timezone.utc),
        datetime_end=datetime(2000, 12, 31, tzinfo=timezone.utc),
    )
    item = make_item(f, meta, _ctx())
    assert item.datetime is None
    assert item.properties["start_datetime"] == "2000-01-01T00:00:00Z"
    assert item.properties["end_datetime"] == "2000-12-31T00:00:00Z"
