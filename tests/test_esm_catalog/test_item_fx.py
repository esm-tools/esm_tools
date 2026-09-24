"""Tests for time-invariant (fx) item support in make_item."""

from __future__ import annotations

from datetime import datetime, timezone

from esm_catalog.item import make_item

from .helpers import make_exp_metadata, make_file_metadata

utc = timezone.utc


def test_fx_item_uses_run_span(temp_nc):
    # A file with no per-file datetime is placed across the experiment's run
    # span, with a null instant and the fx frequency marker.
    file_metadata = make_file_metadata(datetime_start=None, datetime_end=None)
    exp_metadata = make_exp_metadata(
        run_start=datetime(2000, 1, 1, tzinfo=utc),
        run_end=datetime(2010, 1, 1, tzinfo=utc),
    )
    item = make_item(temp_nc, file_metadata, exp_metadata)

    assert item.datetime is None
    assert item.common_metadata.start_datetime == datetime(2000, 1, 1, tzinfo=utc)
    assert item.common_metadata.end_datetime == datetime(2010, 1, 1, tzinfo=utc)
    assert item.properties["frequency"] == "fx"


def test_fx_item_is_valid_stac(temp_nc):
    file_metadata = make_file_metadata(datetime_start=None, datetime_end=None)
    exp_metadata = make_exp_metadata(
        run_start=datetime(2000, 1, 1, tzinfo=utc),
        run_end=datetime(2010, 1, 1, tzinfo=utc),
    )
    item = make_item(temp_nc, file_metadata, exp_metadata)

    item.validate()
    item_dict = item.to_dict()
    assert item_dict["properties"]["datetime"] is None
    assert item_dict["properties"]["start_datetime"] == "2000-01-01T00:00:00Z"
    assert item_dict["properties"]["end_datetime"] == "2010-01-01T00:00:00Z"


def test_time_varying_file_keeps_own_datetime(temp_nc):
    # A file that carries its own datetime is unaffected by a run span and is
    # never marked fx.
    file_metadata = make_file_metadata(
        datetime_start=datetime(2005, 6, 1, tzinfo=utc),
        datetime_end=datetime(2005, 6, 1, tzinfo=utc),
    )
    exp_metadata = make_exp_metadata(
        run_start=datetime(2000, 1, 1, tzinfo=utc),
        run_end=datetime(2010, 1, 1, tzinfo=utc),
    )
    item = make_item(temp_nc, file_metadata, exp_metadata)

    assert item.datetime == datetime(2005, 6, 1, tzinfo=utc)
    assert "frequency" not in item.properties
