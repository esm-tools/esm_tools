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
