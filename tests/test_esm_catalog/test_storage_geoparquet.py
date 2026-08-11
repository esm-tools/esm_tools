"""Round-trip tests for the stac-geoparquet shard storage."""

from __future__ import annotations

from datetime import datetime, timezone

import pystac
import pytest
from upath import UPath

from esm_catalog.item import make_item
from esm_catalog.storage.geoparquet import (
    fx_shard_name,
    item_ids,
    read_shard,
    ts_shard_name,
    write_shard,
)

from .helpers import make_exp_metadata, make_file_metadata


def _make_items(tmp_path) -> list[pystac.Item]:
    """Three real Items with distinct ids, bboxes, and datetimes."""
    specs = [
        ("tas", (-10.0, -5.0, 10.0, 5.0), datetime(2000, 1, 1, tzinfo=timezone.utc)),
        ("pr", (0.0, 0.0, 20.0, 15.0), datetime(2001, 6, 15, tzinfo=timezone.utc)),
        ("psl", (30.0, 40.0, 50.0, 60.0), datetime(2002, 12, 31, tzinfo=timezone.utc)),
    ]
    items = []
    for variable, bbox, moment in specs:
        source = tmp_path / f"{variable}.nc"
        source.write_bytes(b"")
        item = make_item(
            source,
            make_file_metadata(
                variable=variable,
                bbox=list(bbox),
                geometry={
                    "type": "Polygon",
                    "coordinates": [
                        [
                            [bbox[0], bbox[1]],
                            [bbox[2], bbox[1]],
                            [bbox[2], bbox[3]],
                            [bbox[0], bbox[3]],
                            [bbox[0], bbox[1]],
                        ]
                    ],
                },
                datetime_start=moment,
                datetime_end=moment,
            ),
            make_exp_metadata(),
        )
        items.append(item)
    return items


def test_write_read_round_trips_ids_and_rows(tmp_path):
    items = _make_items(tmp_path)
    shard = UPath(tmp_path) / fx_shard_name("awiesm")

    write_shard(items, shard)
    table = read_shard(shard)

    assert table.num_rows == len(items)
    assert set(item_ids(table)) == {item.id for item in items}


def test_shard_table_has_core_columns(tmp_path):
    items = _make_items(tmp_path)
    shard = UPath(tmp_path) / ts_shard_name("awiesm", "20000101-20021231")

    write_shard(items, shard)
    columns = set(read_shard(shard).column_names)

    assert {"id", "geometry", "datetime"} <= columns


def test_empty_list_writes_readable_empty_shard(tmp_path):
    shard = UPath(tmp_path) / fx_shard_name("awiesm")

    write_shard([], shard)
    table = read_shard(shard)

    assert table.num_rows == 0
    assert item_ids(table) == []


def test_fx_shard_name():
    assert fx_shard_name("awiesm") == "awiesm_stac_fx.parquet"


def test_ts_shard_name():
    assert ts_shard_name("awiesm", "20000101-20001231") == "awiesm_stac_20000101-20001231.parquet"


@pytest.mark.parametrize(
    "run_stamp, expected",
    [
        ("18500101-18591231", "awiesm_stac_18500101-18591231.parquet"),
        ("20500101-20501231", "awiesm_stac_20500101-20501231.parquet"),
    ],
)
def test_ts_shard_name_parametrized(run_stamp, expected):
    assert ts_shard_name("awiesm", run_stamp) == expected
