"""Shared fixtures for the esm_catalog test package."""

from __future__ import annotations

from datetime import datetime, timezone
from pathlib import Path

import pytest
from pystac import Asset, Item


@pytest.fixture
def data_file(tmp_path) -> Path:
    """An existing on-disk source file to build items from / stat().

    Content is irrelevant to the catalog layer (metadata comes from the
    scanner, not the bytes); the file only has to exist.
    """
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    return f


def _base_item() -> Item:
    return Item(
        id="x",
        geometry=None,
        bbox=None,
        datetime=datetime(2000, 1, 1, tzinfo=timezone.utc),
        properties={},
    )


@pytest.fixture
def item() -> Item:
    """A bare STAC Item with no assets."""
    return _base_item()


@pytest.fixture
def item_with_asset() -> Item:
    """A bare STAC Item carrying a single 'data' asset."""
    it = _base_item()
    it.add_asset("data", Asset(href="file:///tmp/x.nc"))
    return it
