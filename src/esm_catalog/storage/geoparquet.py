"""stac-geoparquet shard storage for the catalog.

A scanned experiment is persisted as a set of `stac-geoparquet
<https://github.com/stac-utils/stac-geoparquet>`_ shards rather than a database.
Two kinds of shard live side by side:

- ``<expid>_stac_fx.parquet`` holds the time-invariant Items and is *rewritten
  in full* on every scan, pinned to the current run span. There is exactly one
  per catalog.
- ``<expid>_stac_<run_stamp>.parquet`` shards hold the per-run-segment
  time-series Items and are *append-only*: each finished run segment writes its
  own shard and earlier shards are never touched.

The ``<expid>`` prefix keeps shards from different experiments distinct when
pooled into a shared directory.

The STAC Item to Arrow to Parquet mapping is delegated to
``stac_geoparquet.arrow`` (the shard columns, geometry encoding, and schema
follow the stac-geoparquet spec); this module never hand-rolls the schema.

Because the shards are plain GeoParquet, DuckDB, polars, and pyarrow can query
them directly on disk or over object storage. No separate database is
maintained: the parquet files *are* the catalog.
"""

from __future__ import annotations

import pyarrow as pa
import pyarrow.parquet as pq
import pystac
from stac_geoparquet.arrow import parse_stac_items_to_arrow, to_parquet
from upath import UPath

from esm_catalog.scan.types import RunStamp
from esm_catalog.types import ExperimentId

_EMPTY_SHARD_SCHEMA = pa.schema([("id", pa.string())])
"""Minimal schema for an itemless shard: enough to round-trip an empty catalog."""


def ts_shard_name(experiment_id: ExperimentId, run_stamp: RunStamp) -> str:
    """Filename of the append-only time-series shard for one run segment.

    The experiment id prefixes the name so shards from different experiments
    stay distinguishable when copied into a shared location.

    Parameters
    ----------
    experiment_id : ExperimentId
        The owning experiment, e.g. ``'awiesm'``.
    run_stamp : RunStamp
        The run-segment stamp, e.g. ``'20000101-20001231'``.

    Returns
    -------
    str
        The shard filename, e.g. ``'awiesm_stac_20000101-20001231.parquet'``.
    """
    return f"{experiment_id}_stac_{run_stamp}.parquet"


def fx_shard_name(experiment_id: ExperimentId) -> str:
    """Filename of the single time-invariant shard, rewritten in full each scan.

    Parameters
    ----------
    experiment_id : ExperimentId
        The owning experiment, e.g. ``'awiesm'``.

    Returns
    -------
    str
        The shard filename, e.g. ``'awiesm_stac_fx.parquet'``.
    """
    return f"{experiment_id}_stac_fx.parquet"


def write_shard(items: list[pystac.Item], path: UPath) -> None:
    """Serialize STAC Items to a stac-geoparquet shard.

    Parameters
    ----------
    items : list of pystac.Item
        The Items to persist. An empty list writes a valid, itemless shard that
        :func:`read_shard` reads back as a zero-row table.
    path : UPath
        Destination path of the shard file.

    Returns
    -------
    None
    """
    target, filesystem = _fs_target(path)
    if not items:
        pq.write_table(_EMPTY_SHARD_SCHEMA.empty_table(), target, filesystem=filesystem)
        return

    to_parquet(parse_stac_items_to_arrow(items), target, filesystem=filesystem)


def read_shard(path: UPath) -> pa.Table:
    """Read a stac-geoparquet shard back into an Arrow table.

    Parameters
    ----------
    path : UPath
        Path to the shard file to read.

    Returns
    -------
    pyarrow.Table
        The shard contents; an itemless shard yields a zero-row table.
    """
    target, filesystem = _fs_target(path)
    return pq.read_table(target, filesystem=filesystem)


def item_ids(table: pa.Table) -> list[str]:
    """Extract the Item ids from a shard table.

    Parameters
    ----------
    table : pyarrow.Table
        A table read via :func:`read_shard`.

    Returns
    -------
    list of str
        The ``id`` column as Python strings, in row order.
    """
    return table.column("id").to_pylist()


def _fs_target(path: UPath):
    """Split a UPath into the (target, filesystem) pyarrow.parquet expects.

    Local paths pass straight through with pyarrow's native filesystem
    (``filesystem=None``). Remote/virtual UPaths (``memory://``, ``s3://``, ...)
    carry a scheme pyarrow's URI resolver does not recognize, so they are routed
    through their fsspec filesystem (which pyarrow wraps) with the scheme-less
    path.
    """
    if path.protocol in ("", "file", "local"):
        return str(path), None
    return path.path, path.fs
