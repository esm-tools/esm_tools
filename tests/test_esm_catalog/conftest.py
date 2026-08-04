"""Shared fixtures for the esm_catalog STAC tests.

Generic builders live here so sibling test modules can drop their private
_ctx()/bare-object copies onto a single source of truth. Extension-specific
fixtures (e.g. shipped_namelist) stay in their own test module.
"""

from __future__ import annotations

from datetime import datetime, timezone

import pytest
from pystac import Collection, Extent, Item, SpatialExtent, TemporalExtent

from esm_catalog.context import CollectionContext


@pytest.fixture
def bare_collection():
    """An empty pystac Collection, to exercise collection-level extensions directly."""
    return Collection(
        id="exp",
        description="test collection",
        extent=Extent(
            spatial=SpatialExtent(bboxes=[[-180.0, -90.0, 180.0, 90.0]]),
            temporal=TemporalExtent(intervals=[[None, None]]),
        ),
    )


@pytest.fixture
def bare_item():
    """A minimal pystac Item, to exercise item-level extensions directly."""
    return Item(
        id="i",
        geometry=None,
        bbox=None,
        datetime=datetime(2000, 1, 1, tzinfo=timezone.utc),
        properties={},
    )


@pytest.fixture
def make_ctx():
    """Factory for a CollectionContext — the end-to-end builder tests need one."""

    def _make(**kwargs):
        return CollectionContext(
            experiment_id="exp", component="echam", collection_id="exp", **kwargs
        )

    return _make
