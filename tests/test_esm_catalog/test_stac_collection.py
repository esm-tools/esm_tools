"""Tests for ESMCollection."""

from __future__ import annotations

from esm_catalog.collection import ESMCollection
from esm_catalog.context import CollectionContext


def _ctx():
    return CollectionContext(
        experiment_id="exp-alpha", component="echam", collection_id="exp-alpha"
    )


def test_collection_minimal_skeleton():
    col = ESMCollection(_ctx())
    assert col["type"] == "Collection"
    assert col["id"] == "exp-alpha"
    assert col["components"] == ["echam"]


def test_collection_is_dict():
    col = ESMCollection(_ctx())
    assert isinstance(col, dict)


def test_update_extent_expands_temporal():
    col = ESMCollection(_ctx())
    col.update_extent({"bbox": [0, 0, 1, 1], "properties": {"datetime": "2000-01-01T00:00:00"}})
    assert col["extent"]["temporal"]["interval"][0][0] == "2000-01-01T00:00:00"


def test_update_extent_expands_to_wider_range():
    col = ESMCollection(_ctx())
    col.update_extent({"bbox": [0, 0, 1, 1], "properties": {"datetime": "2000-06-01T00:00:00"}})
    col.update_extent({"bbox": [0, 0, 1, 1], "properties": {"datetime": "1990-01-01T00:00:00"}})
    assert col["extent"]["temporal"]["interval"][0][0] == "1990-01-01T00:00:00"
    assert col["extent"]["temporal"]["interval"][0][1] == "2000-06-01T00:00:00"
