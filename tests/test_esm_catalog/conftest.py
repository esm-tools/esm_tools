"""Shared fixtures for the esm_catalog STAC tests.

Zero-arg object builders (item, collection) and temp_nc live here as fixtures.
The parameterized builders (make_exp_metadata, make_file_metadata) and assert_valid are plain
functions in helpers.py, imported and called inline with per-test arguments.
"""

from __future__ import annotations

from datetime import datetime, timezone

import pytest
from pystac import Collection, Extent, Item, SpatialExtent, TemporalExtent


@pytest.fixture
def collection() -> Collection:
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
def item() -> Item:
    """A minimal pystac Item, to exercise item-level extensions directly."""
    return Item(
        id="i",
        geometry=None,
        bbox=None,
        datetime=datetime(2000, 1, 1, tzinfo=timezone.utc),
        properties={},
    )


@pytest.fixture
def temp_nc(tmp_path):
    """A throwaway .nc file on disk; make_item only needs the path to exist."""
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    return f
