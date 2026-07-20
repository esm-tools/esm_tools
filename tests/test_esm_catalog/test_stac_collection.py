"""Tests for make_collection and update_extent."""

from __future__ import annotations

from datetime import datetime, timezone

from pystac import Collection, Item

from esm_catalog.collection import make_collection, update_extent
from esm_catalog.context import CollectionContext


def _ctx():
    return CollectionContext(
        experiment_id="exp-alpha", component="echam", collection_id="exp-alpha"
    )


def _item(dt, bbox=None, dt_end=None):
    return Item(
        id="test",
        geometry=None,
        bbox=bbox,
        datetime=dt if dt_end is None or dt == dt_end else None,
        properties={},
        start_datetime=dt if dt_end is not None and dt != dt_end else None,
        end_datetime=dt_end if dt_end is not None and dt != dt_end else None,
    )


def test_collection_is_pystac():
    col = make_collection(_ctx())
    assert isinstance(col, Collection)


def test_collection_minimal_skeleton():
    col = make_collection(_ctx())
    assert col.id == "exp-alpha"
    assert col.extra_fields["components"] == ["echam"]
    assert col.extra_fields["experiment"] == "exp-alpha"


def test_update_extent_expands_temporal():
    col = make_collection(_ctx())
    dt = datetime(2000, 1, 1, tzinfo=timezone.utc)
    update_extent(col, _item(dt, bbox=[0, 0, 1, 1]))
    assert col.extent.temporal.intervals[0][0] == dt


def test_update_extent_expands_to_wider_range():
    col = make_collection(_ctx())
    dt1 = datetime(2000, 6, 1, tzinfo=timezone.utc)
    dt2 = datetime(1990, 1, 1, tzinfo=timezone.utc)
    update_extent(col, _item(dt1, bbox=[0, 0, 1, 1]))
    update_extent(col, _item(dt2, bbox=[0, 0, 1, 1]))
    assert col.extent.temporal.intervals[0][0] == dt2
    assert col.extent.temporal.intervals[0][1] == dt1


# --- collection metadata ---


def test_collection_license_defaults_to_proprietary():
    assert make_collection(_ctx()).license == "proprietary"


def test_collection_license_from_context():
    ctx = CollectionContext(
        experiment_id="exp-alpha",
        component="echam",
        collection_id="exp-alpha",
        data_license="CC-BY-4.0",
    )
    assert make_collection(ctx).license == "CC-BY-4.0"


def test_collection_parent_link():
    # pystac auto-adds a "root" link at construction, so search by rel.
    col = make_collection(_ctx())
    parent_links = [link for link in col.links if link.rel == "parent"]
    assert len(parent_links) == 1
    assert parent_links[0].target == "#exp-alpha"


# --- spatial extent ---


def test_spatial_extent_replaces_default_with_first_bbox():
    col = make_collection(_ctx())
    dt = datetime(2000, 1, 1, tzinfo=timezone.utc)
    update_extent(col, _item(dt, bbox=[0, 0, 10, 10]))
    assert col.extent.spatial.bboxes == [[0, 0, 10, 10]]


def test_spatial_extent_merges_second_bbox():
    col = make_collection(_ctx())
    dt = datetime(2000, 1, 1, tzinfo=timezone.utc)
    update_extent(col, _item(dt, bbox=[0, 0, 10, 10]))
    update_extent(col, _item(dt, bbox=[-20, 5, 5, 30]))
    assert col.extent.spatial.bboxes == [[-20, 0, 10, 30]]


def test_item_without_bbox_leaves_spatial_untouched():
    col = make_collection(_ctx())
    dt = datetime(2000, 1, 1, tzinfo=timezone.utc)
    update_extent(col, _item(dt))
    assert col.extent.spatial.bboxes == [[-180.0, -90.0, 180.0, 90.0]]


# --- temporal extent ---


def test_temporal_extent_not_shrunk_by_inner_item():
    col = make_collection(_ctx())
    dt_lo = datetime(1990, 1, 1, tzinfo=timezone.utc)
    dt_hi = datetime(2010, 1, 1, tzinfo=timezone.utc)
    dt_mid = datetime(2000, 1, 1, tzinfo=timezone.utc)
    update_extent(col, _item(dt_lo, bbox=[0, 0, 1, 1], dt_end=dt_hi))
    update_extent(col, _item(dt_mid, bbox=[0, 0, 1, 1]))
    assert col.extent.temporal.intervals[0] == [dt_lo, dt_hi]
