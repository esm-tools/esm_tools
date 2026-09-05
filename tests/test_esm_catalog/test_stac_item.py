"""Tests for make_item."""

from __future__ import annotations

import re
from datetime import datetime, timezone
from pathlib import Path

import pytest
from pystac import Item, STACError
from upath import UPath

from esm_catalog.item import _to_href, make_item

from .helpers import make_exp_metadata, make_file_metadata


@pytest.fixture
def item(temp_nc):
    """An Item built from the default file + experiment metadata (overrides the
    bare-object conftest fixture; these tests exercise a built item)."""
    return make_item(temp_nc, make_file_metadata(), make_exp_metadata())


def test_item_is_pystacitem(item):
    assert isinstance(item, Item)


def test_item_to_dict_and_stac_version_and_type(item):
    item_dict = item.to_dict()
    assert item_dict["stac_version"] == "1.0.0"
    assert item_dict["type"] == "Feature"


def test_item_basic_fields(item):
    assert item.properties["variable"] == "temp"
    assert item.properties["format"] == "netcdf"
    assert item.properties["component"] == "echam"
    assert item.properties["experiment"] == "exp-alpha"
    assert item.collection_id.startswith("exp-alpha-")  # name + path hash
    assert item.assets["data"].href.startswith("file://")
    assert item.links[0].rel == "collection"
    assert item.links[0].target.startswith("#exp-alpha-")


def test_item_bbox_and_geometry_passthrough(temp_nc):
    # A scanned footprint must be forwarded onto the Item for update_extent to use.
    geometry = {"type": "Point", "coordinates": [10.0, 20.0]}
    file_metadata = make_file_metadata(bbox=[0.0, 0.0, 10.0, 20.0], geometry=geometry)
    item = make_item(temp_nc, file_metadata, make_exp_metadata())
    assert item.bbox == [0.0, 0.0, 10.0, 20.0]
    assert item.geometry == geometry


def test_item_single_time_sets_datetime(item):
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


def test_item_time_range_sets_interval(temp_nc):
    file_metadata = make_file_metadata(
        datetime_start=datetime(2000, 1, 1, tzinfo=timezone.utc),
        datetime_end=datetime(2000, 12, 31, tzinfo=timezone.utc),
    )
    item = make_item(temp_nc, file_metadata, make_exp_metadata())
    assert item.datetime is None
    assert item.properties["start_datetime"] == "2000-01-01T00:00:00Z"
    assert item.properties["end_datetime"] == "2000-12-31T00:00:00Z"


# --- edge cases: paths, datetimes, ids, properties ---


def test_make_item_accepts_local_string_path(temp_nc):
    item = make_item(str(temp_nc), make_file_metadata(), make_exp_metadata())
    href = item.assets["data"].href
    assert href.startswith("file://")
    assert href.endswith("/temp.nc")


def test_make_item_accepts_uri_string():
    uri = "ssh://hpc.example.org/data/temp.nc"
    item = make_item(uri, make_file_metadata(), make_exp_metadata())
    assert item.assets["data"].href == uri


def test_make_item_resolves_relative_path():
    # A relative Path must still yield an absolute file:// href, not raise; no
    # file/chdir needed since make_item does no I/O.
    item = make_item(Path("sub/temp.nc"), make_file_metadata(), make_exp_metadata())
    href = item.assets["data"].href
    assert href.startswith("file://")
    assert href.endswith("/sub/temp.nc")


def test_naive_datetimes_normalized_to_utc(temp_nc):
    file_metadata = make_file_metadata(
        datetime_start=datetime(2000, 1, 1), datetime_end=datetime(2000, 1, 1)
    )
    item = make_item(temp_nc, file_metadata, make_exp_metadata())
    assert item.datetime == datetime(2000, 1, 1, tzinfo=timezone.utc)
    assert item.datetime.tzinfo is not None


def test_open_ended_range_keeps_datetime(temp_nc):
    item = make_item(
        temp_nc, make_file_metadata(datetime_end=None), make_exp_metadata()
    )
    assert item.datetime == datetime(2000, 1, 1, tzinfo=timezone.utc)
    assert item.common_metadata.end_datetime is None


def test_missing_datetimes_raise(temp_nc):
    file_metadata = make_file_metadata(datetime_start=None, datetime_end=None)
    with pytest.raises(STACError):
        make_item(temp_nc, file_metadata, make_exp_metadata())


def test_output_frequency_property(temp_nc):
    with_freq = make_item(
        temp_nc, make_file_metadata(output_frequency="mon"), make_exp_metadata()
    )
    without = make_item(temp_nc, make_file_metadata(), make_exp_metadata())
    assert with_freq.properties["output_frequency"] == "mon"
    assert "output_frequency" not in without.properties


def test_multiple_variables_listed(temp_nc):
    variables = [{"name": "temp"}, {"name": "prec"}, {"name": "unknown"}, {}]
    item = make_item(
        temp_nc, make_file_metadata(variables=variables), make_exp_metadata()
    )
    assert item.properties["variables"] == ["temp", "prec"]


def test_single_variable_no_variables_key(temp_nc):
    item = make_item(
        temp_nc, make_file_metadata(variables=[{"name": "temp"}]), make_exp_metadata()
    )
    assert "variables" not in item.properties


def test_item_id_format(item):
    assert re.fullmatch(r"temp\.echam\.000000\.[0-9a-f]{6}", item.id)


def test_item_id_uses_datetime_str(temp_nc):
    # A scanner-supplied nominal timestamp lands in the id's datetime segment.
    item = make_item(
        temp_nc, make_file_metadata(datetime_str="20000101"), make_exp_metadata()
    )
    assert re.fullmatch(r"temp\.echam\.20000101\.[0-9a-f]{6}", item.id)


def test_item_id_distinct_for_different_paths(tmp_path):
    f1 = tmp_path / "a" / "temp.nc"
    f2 = tmp_path / "b" / "temp.nc"
    for f in (f1, f2):
        f.parent.mkdir()
        f.write_bytes(b"x")
    id1 = make_item(f1, make_file_metadata(), make_exp_metadata()).id
    id2 = make_item(f2, make_file_metadata(), make_exp_metadata()).id
    assert id1 != id2


def test_grib_media_type(tmp_path):
    f = tmp_path / "temp.grb"
    f.write_bytes(b"x")
    item = make_item(f, make_file_metadata(format="grib"), make_exp_metadata())
    assert item.assets["data"].media_type == "application/x-grib2"


def test_to_href_bucket_protocol_uri():
    p = UPath("memory://experiments/data/temp.nc")
    assert _to_href(p) == "memory://experiments/data/temp.nc"


def test_item_id_defaults_to_unknown_variable(temp_nc):
    file_metadata = make_file_metadata(variable=None)
    item = make_item(temp_nc, file_metadata, make_exp_metadata())
    assert re.fullmatch(r"unknown\.echam\.000000\.[0-9a-f]{6}", item.id)


def test_netcdf_media_type_default(item):
    assert item.assets["data"].media_type == "application/x-netcdf"
