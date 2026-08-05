"""Shared, importable test helpers for the esm_catalog STAC tests.

Plain functions (not fixtures) so they compose inline, e.g.

    make_item(temp_nc, metadata(), make_ctx())

conftest.py wraps the bare-object builders + make_ctx as fixtures for tests
that prefer the injection style; both share this single implementation.
"""

from __future__ import annotations

from datetime import datetime, timezone

import jsonschema
from pystac import Collection, Extent, Item, SpatialExtent, TemporalExtent

from esm_catalog.models import ExperimentMetadata


def make_ctx(**kwargs) -> ExperimentMetadata:
    """An ExperimentMetadata with sensible defaults; override any field via kwargs."""
    return ExperimentMetadata(**{"experiment_id": "exp-alpha", **kwargs})


def metadata(**kwargs) -> dict:
    """Default scan metadata for one file; override any key via kwargs."""
    return {
        "variable": "temp",
        "component": "echam",
        "format": "netcdf",
        "datetime_start": datetime(2000, 1, 1, tzinfo=timezone.utc),
        "datetime_end": datetime(2000, 1, 1, tzinfo=timezone.utc),
        **kwargs,
    }


def bare_collection() -> Collection:
    """An empty pystac Collection, to exercise collection-level extensions directly."""
    return Collection(
        id="exp",
        description="test collection",
        extent=Extent(
            spatial=SpatialExtent(bboxes=[[-180.0, -90.0, 180.0, 90.0]]),
            temporal=TemporalExtent(intervals=[[None, None]]),
        ),
    )


def bare_item() -> Item:
    """A minimal pystac Item, to exercise item-level extensions directly."""
    return Item(
        id="i",
        geometry=None,
        bbox=None,
        datetime=datetime(2000, 1, 1, tzinfo=timezone.utc),
        properties={},
    )


def assert_valid(obj, schema) -> None:
    """Assert *obj*.to_dict() validates against *schema* (a loaded JSON Schema)."""
    jsonschema.validate(instance=obj.to_dict(), schema=schema)
