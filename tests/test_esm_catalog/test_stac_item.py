"""Tests for make_item."""

from __future__ import annotations

from datetime import datetime, timezone

from pystac import Item

from esm_catalog.context import CollectionContext
from esm_catalog.item import _to_href, make_item


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
    assert isinstance(item, Item)


def test_item_to_dict_and_stac_version_and_type(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_item(f, _metadata(), _ctx())
    item_dict = item.to_dict()
    assert item_dict["stac_version"] == "1.0.0"
    assert item_dict["type"] == "Feature"


def test_item_basic_fields(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_item(f, _metadata(), _ctx())
    assert item.properties["variable"] == "temp"
    assert item.properties["format"] == "netcdf"
    assert item.collection_id == "exp-alpha"
    assert item.assets["data"].href.startswith("file://")
    assert item.links[0].rel == "collection"
    assert item.links[0].target == "#exp-alpha"


def test_item_single_time_sets_datetime(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_item(f, _metadata(), _ctx())
    assert item.datetime == datetime(2000, 1, 1, tzinfo=timezone.utc)
    assert item.properties["start_datetime"] == "2000-01-01T00:00:00Z"
    assert item.properties["end_datetime"] == "2000-01-01T00:00:00Z"


def test_to_href_local_path_is_file_uri(tmp_path):
    f = tmp_path / "data.nc"
    f.write_bytes(b"x")
    assert _to_href(f) == f.as_uri()


def test_to_href_remote_path_includes_host():
    from upath import UPath

    path = UPath("ssh://albedo0.dmawi.de/work/user/exp/file.nc")
    assert _to_href(path) == "ssh://albedo0.dmawi.de/work/user/exp/file.nc"


def test_to_href_remote_path_missing_host_raises():
    import pytest

    class _HostlessPath:
        protocol = "ssh"
        path = "/work/data.nc"
        storage_options = {}

        def __str__(self):
            return "ssh:///work/data.nc"

    with pytest.raises(ValueError, match="no host"):
        _to_href(_HostlessPath())


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
