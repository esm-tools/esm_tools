"""Tests for ESMItem."""

from __future__ import annotations

from datetime import datetime, timezone

from esm_catalog.context import CollectionContext
from esm_catalog.item import ESMItem


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


def test_item_is_dict(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = ESMItem(f, _metadata(), _ctx())
    assert isinstance(item, dict)


def test_item_basic_fields(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = ESMItem(f, _metadata(), _ctx())
    assert item["type"] == "Feature"
    assert item["properties"]["variable"] == "temp"
    assert item["properties"]["format"] == "netcdf"
    assert item["collection"] == "exp-alpha"
    assert item["assets"]["data"]["href"].startswith("file://")


def test_item_single_time_sets_datetime(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = ESMItem(f, _metadata(), _ctx())
    assert item["properties"]["datetime"] is not None
    assert "start_datetime" not in item["properties"]


def test_item_time_range_sets_interval(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    meta = _metadata(
        datetime_start=datetime(2000, 1, 1, tzinfo=timezone.utc),
        datetime_end=datetime(2000, 12, 31, tzinfo=timezone.utc),
    )
    item = ESMItem(f, meta, _ctx())
    assert item["properties"]["datetime"] is None
    assert item["properties"]["start_datetime"] is not None
    assert item["properties"]["end_datetime"] is not None
