"""Tests for make_item."""

from __future__ import annotations

import re
from datetime import datetime, timezone

import pytest
from pystac import Item, STACError
from upath import UPath

from esm_catalog.item import _to_href, make_item
from esm_catalog.models import Contact
from esm_catalog.registry import EXTENSION_URLS

from .helpers import make_ctx, metadata


def test_item_is_pystacitem(temp_nc):
    item = make_item(temp_nc, metadata(), make_ctx())
    assert isinstance(item, Item)


def test_item_to_dict_and_stac_version_and_type(temp_nc):
    item = make_item(temp_nc, metadata(), make_ctx())
    item_dict = item.to_dict()
    assert item_dict["stac_version"] == "1.0.0"
    assert item_dict["type"] == "Feature"


def test_item_basic_fields(temp_nc):
    item = make_item(temp_nc, metadata(), make_ctx())
    assert item.properties["variable"] == "temp"
    assert item.properties["format"] == "netcdf"
    assert item.collection_id == "exp-alpha"
    assert item.assets["data"].href.startswith("file://")
    assert item.links[0].rel == "collection"
    assert item.links[0].target == "#exp-alpha"


def test_item_single_time_sets_datetime(temp_nc):
    item = make_item(temp_nc, metadata(), make_ctx())
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
    meta = metadata(
        datetime_start=datetime(2000, 1, 1, tzinfo=timezone.utc),
        datetime_end=datetime(2000, 12, 31, tzinfo=timezone.utc),
    )
    item = make_item(temp_nc, meta, make_ctx())
    assert item.datetime is None
    assert item.properties["start_datetime"] == "2000-01-01T00:00:00Z"
    assert item.properties["end_datetime"] == "2000-12-31T00:00:00Z"


# --- edge cases: paths, datetimes, ids, properties ---


def test_make_item_accepts_local_string_path(temp_nc):
    item = make_item(str(temp_nc), metadata(), make_ctx())
    href = item.assets["data"].href
    assert href.startswith("file://")
    assert href.endswith("/temp.nc")


def test_make_item_accepts_uri_string():
    uri = "ssh://hpc.example.org/data/temp.nc"
    item = make_item(uri, metadata(), make_ctx())
    assert item.assets["data"].href == uri


def test_naive_datetimes_normalized_to_utc(temp_nc):
    meta = metadata(
        datetime_start=datetime(2000, 1, 1), datetime_end=datetime(2000, 1, 1)
    )
    item = make_item(temp_nc, meta, make_ctx())
    assert item.datetime == datetime(2000, 1, 1, tzinfo=timezone.utc)
    assert item.datetime.tzinfo is not None


def test_open_ended_range_keeps_datetime(temp_nc):
    item = make_item(temp_nc, metadata(datetime_end=None), make_ctx())
    assert item.datetime == datetime(2000, 1, 1, tzinfo=timezone.utc)
    assert item.common_metadata.end_datetime is None


def test_missing_datetimes_raise(temp_nc):
    meta = metadata(datetime_start=None, datetime_end=None)
    with pytest.raises(STACError):
        make_item(temp_nc, meta, make_ctx())


def test_output_frequency_property(temp_nc):
    with_freq = make_item(temp_nc, metadata(output_frequency="mon"), make_ctx())
    without = make_item(temp_nc, metadata(), make_ctx())
    assert with_freq.properties["output_frequency"] == "mon"
    assert "output_frequency" not in without.properties


def test_multiple_variables_listed(temp_nc):
    variables = [{"name": "temp"}, {"name": "prec"}, {"name": "unknown"}, {}]
    item = make_item(temp_nc, metadata(variables=variables), make_ctx())
    assert item.properties["variables"] == ["temp", "prec"]


def test_single_variable_no_variables_key(temp_nc):
    item = make_item(temp_nc, metadata(variables=[{"name": "temp"}]), make_ctx())
    assert "variables" not in item.properties


def test_item_id_format(temp_nc):
    item = make_item(temp_nc, metadata(), make_ctx())
    assert re.fullmatch(r"temp\.echam\.000000\.[0-9a-f]{6}", item.id)


def test_item_id_distinct_for_different_paths(tmp_path):
    f1 = tmp_path / "a" / "temp.nc"
    f2 = tmp_path / "b" / "temp.nc"
    for f in (f1, f2):
        f.parent.mkdir()
        f.write_bytes(b"x")
    id1 = make_item(f1, metadata(), make_ctx()).id
    id2 = make_item(f2, metadata(), make_ctx()).id
    assert id1 != id2


def test_grib_media_type(tmp_path):
    f = tmp_path / "temp.grb"
    f.write_bytes(b"x")
    item = make_item(f, metadata(format="grib"), make_ctx())
    assert item.assets["data"].media_type == "application/x-grib2"


def test_to_href_bucket_protocol_uri():
    p = UPath("memory://experiments/data/temp.nc")
    assert _to_href(p) == "memory://experiments/data/temp.nc"


def test_item_id_defaults_to_unknown_variable(temp_nc):
    meta = metadata()
    del meta["variable"]
    item = make_item(temp_nc, meta, make_ctx())
    assert re.fullmatch(r"unknown\.echam\.000000\.[0-9a-f]{6}", item.id)


def test_netcdf_media_type_default(temp_nc):
    item = make_item(temp_nc, metadata(), make_ctx())
    assert item.assets["data"].media_type == "application/x-netcdf"


# --- contacts ---


def test_item_no_contacts_no_extension(temp_nc):
    item = make_item(temp_nc, metadata(), make_ctx())
    assert "contacts" not in item.properties
    assert item.stac_extensions == []


def test_item_contacts_in_properties(temp_nc):
    ctx = make_ctx(
        contacts=[
            Contact(name="Jane Doe", orcid="0000-0001-2345-6789", institution="AWI")
        ]
    )
    item = make_item(temp_nc, metadata(), ctx)
    contacts = item.properties["contacts"]
    assert len(contacts) == 1
    assert contacts[0]["name"] == "Jane Doe"
    assert contacts[0]["identifier"] == "https://orcid.org/0000-0001-2345-6789"
    assert contacts[0]["organization"] == "AWI"


def test_item_contacts_orcid_full_url_passthrough(temp_nc):
    ctx = make_ctx(
        contacts=[
            Contact(
                name="Jane Doe",
                orcid="https://orcid.org/0000-0001-2345-6789",
                institution="AWI",
            )
        ]
    )
    item = make_item(temp_nc, metadata(), ctx)
    assert (
        item.properties["contacts"][0]["identifier"]
        == "https://orcid.org/0000-0001-2345-6789"
    )


def test_item_contacts_registers_extension_url(temp_nc):
    ctx = make_ctx(contacts=[Contact(name="Jane", institution="AWI")])
    item = make_item(temp_nc, metadata(), ctx)
    assert EXTENSION_URLS["contacts"] in item.stac_extensions


def test_item_contacts_orcid_optional(temp_nc):
    ctx = make_ctx(contacts=[Contact(name="Jane Doe", institution="AWI")])
    item = make_item(temp_nc, metadata(), ctx)
    assert "identifier" not in item.properties["contacts"][0]


def test_item_multiple_contacts(temp_nc):
    ctx = make_ctx(
        contacts=[
            Contact(name="Jane Doe", institution="AWI"),
            Contact(name="John Smith", institution="DKRZ"),
        ]
    )
    item = make_item(temp_nc, metadata(), ctx)
    assert len(item.properties["contacts"]) == 2
