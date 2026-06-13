"""Tests for merge_shards() and 'esm-catalog merge' CLI."""

from __future__ import annotations

from pathlib import Path

from click.testing import CliRunner

from esm_catalog.cli import main
from esm_catalog.storage.duckdb import CatalogDB, persist_tree
from esm_catalog.storage.federation import merge_shards


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _col(col_id: str, experiment: str) -> dict:
    return {
        "type": "Collection",
        "id": col_id,
        "stac_version": "1.0.0",
        "description": f"Test {col_id}",
        "title": col_id,
        "experiment": experiment,
        "links": [],
        "extent": {
            "spatial": {"bbox": [[-180.0, -90.0, 180.0, 90.0]]},
            "temporal": {"interval": [[None, None]]},
        },
        "license": "proprietary",
        "components": ["echam"],
    }


def _item(item_id: str, col_id: str, experiment: str, variable: str = "tas") -> dict:
    return {
        "type": "Feature",
        "stac_version": "1.0.0",
        "id": item_id,
        "collection": col_id,
        "bbox": [-180.0, -90.0, 180.0, 90.0],
        "geometry": None,
        "properties": {
            "datetime": "2000-01-01T00:00:00Z",
            "experiment": experiment,
            "component": "echam",
            "variable": variable,
            "format": "netcdf",
        },
        "assets": {},
        "links": [],
    }


def _make_shard(path: Path, col_id: str, experiment: str, item_ids: list[str]) -> Path:
    catalog = {
        "collections": [_col(col_id, experiment)],
        "items": [_item(iid, col_id, experiment) for iid in item_ids],
    }
    persist_tree(catalog, path)
    return path


# ---------------------------------------------------------------------------
# merge_shards()
# ---------------------------------------------------------------------------

def test_merge_two_shards(tmp_path):
    shard_a = _make_shard(tmp_path / "a.duckdb", "exp-alpha-echam", "exp-alpha",
                          ["item-a1", "item-a2"])
    shard_b = _make_shard(tmp_path / "b.duckdb", "exp-beta-echam", "exp-beta",
                          ["item-b1"])
    global_db = tmp_path / "global.duckdb"

    n_cols, n_items = merge_shards([shard_a, shard_b], global_db)

    assert n_cols == 2
    assert n_items == 3

    with CatalogDB(global_db) as db:
        _, total_items = db.search_items()
        assert total_items == 3
        _, total_cols = db.search_collections()
        assert total_cols == 2

        # Both experiments are queryable in the global DB
        items_a, _ = db.search_items({"experiment": "exp-alpha"})
        assert len(items_a) == 2
        items_b, _ = db.search_items({"experiment": "exp-beta"})
        assert len(items_b) == 1


def test_merge_is_idempotent(tmp_path):
    """Merging the same shard twice must not create duplicate rows."""
    shard = _make_shard(tmp_path / "shard.duckdb", "exp-alpha-echam", "exp-alpha",
                        ["item-1", "item-2"])
    global_db = tmp_path / "global.duckdb"

    merge_shards([shard], global_db)
    merge_shards([shard], global_db)  # second merge

    with CatalogDB(global_db) as db:
        _, total = db.search_items()
        assert total == 2  # still 2, not 4


def test_merge_into_existing_global(tmp_path):
    """Merging a shard into a non-empty global DB preserves existing data."""
    shard_a = _make_shard(tmp_path / "a.duckdb", "exp-alpha-echam", "exp-alpha",
                          ["item-a1"])
    shard_b = _make_shard(tmp_path / "b.duckdb", "exp-beta-echam", "exp-beta",
                          ["item-b1"])
    global_db = tmp_path / "global.duckdb"

    # First merge
    merge_shards([shard_a], global_db)
    # Second merge adds to existing global
    merge_shards([shard_b], global_db)

    with CatalogDB(global_db) as db:
        _, total = db.search_items()
        assert total == 2
        _, col_total = db.search_collections()
        assert col_total == 2


def test_merge_skips_missing_shard(tmp_path):
    """A missing shard path is skipped with a warning; other shards still merge."""
    shard_a = _make_shard(tmp_path / "a.duckdb", "exp-alpha-echam", "exp-alpha",
                          ["item-a1"])
    missing = tmp_path / "does_not_exist.duckdb"
    global_db = tmp_path / "global.duckdb"

    n_cols, n_items = merge_shards([shard_a, missing], global_db)

    assert n_cols == 1
    assert n_items == 1


def test_merge_collection_item_props_indexed(tmp_path):
    """collection_item_props are transferred so the global DB supports property search."""
    shard = tmp_path / "shard.duckdb"
    col = _col("exp-alpha-echam", "exp-alpha")
    item = _item("item-1", "exp-alpha-echam", "exp-alpha", variable="ssh")

    with CatalogDB(shard) as db:
        db.insert_collection(col)
        db.insert_item(item)
        db.upsert_collection_item_props("exp-alpha-echam", item)

    global_db = tmp_path / "global.duckdb"
    merge_shards([shard], global_db)

    with CatalogDB(global_db) as db:
        props = db.get_collection_item_props("exp-alpha-echam")
        assert "ssh" in props.get("variable", set())


# ---------------------------------------------------------------------------
# CLI: 'esm-catalog merge'
# ---------------------------------------------------------------------------

def test_cli_merge(tmp_path):
    shard_a = _make_shard(tmp_path / "a.duckdb", "exp-alpha-echam", "exp-alpha",
                          ["item-a1", "item-a2"])
    shard_b = _make_shard(tmp_path / "b.duckdb", "exp-beta-echam", "exp-beta",
                          ["item-b1"])
    global_db = tmp_path / "global.duckdb"

    result = CliRunner().invoke(
        main,
        ["merge", str(shard_a), str(shard_b), "--output", str(global_db)],
    )

    assert result.exit_code == 0, result.output
    assert global_db.exists()
    assert "2 shard(s)" in result.output

    with CatalogDB(global_db) as db:
        _, total = db.search_items()
        assert total == 3


def test_cli_merge_idempotent(tmp_path):
    shard = _make_shard(tmp_path / "shard.duckdb", "exp-alpha-echam", "exp-alpha",
                        ["item-1"])
    global_db = tmp_path / "global.duckdb"
    runner = CliRunner()

    runner.invoke(main, ["merge", str(shard), "--output", str(global_db)])
    runner.invoke(main, ["merge", str(shard), "--output", str(global_db)])

    with CatalogDB(global_db) as db:
        _, total = db.search_items()
        assert total == 1
