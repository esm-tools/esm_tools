"""Tests for storage/duckdb.py and storage/export.py."""

import json
from pathlib import Path

import pytest

from esm_catalog.stac.collection import make_collection
from esm_catalog.stac.item import make_item
from esm_catalog.storage.duckdb import CatalogDB
from esm_catalog.storage.export import export_json, export_parquet, import_parquet


# ---------------------------------------------------------------------------
# storage/duckdb.py — schema and collections
# ---------------------------------------------------------------------------

class TestCatalogDBSchema:
    def test_creates_db_file(self, tmp_path):
        db_path = tmp_path / "test.duckdb"
        assert not db_path.exists()
        with CatalogDB(db_path):
            pass
        assert db_path.exists()

    def test_creates_nested_directories(self, tmp_path):
        db_path = tmp_path / "a" / "b" / "c" / "catalog.duckdb"
        with CatalogDB(db_path):
            pass
        assert db_path.exists()

    def test_context_manager_closes_connection(self, tmp_path):
        db_path = tmp_path / "test.duckdb"
        with CatalogDB(db_path) as db:
            pass
        # Reconnect to verify data is persisted
        with CatalogDB(db_path) as db2:
            result = db2.db.execute("SELECT COUNT(*) FROM items").fetchone()
            assert result[0] == 0


class TestCollectionCRUD:
    def test_collection_not_exists_initially(self, db):
        assert not db.collection_exists("basic-001-fesom")

    def test_insert_and_exists(self, db, ctx):
        col = make_collection(ctx)
        db.insert_collection(col)
        assert db.collection_exists("basic-001-fesom")

    def test_get_collection_returns_dict(self, db, ctx):
        col = make_collection(ctx)
        db.insert_collection(col)
        retrieved = db.get_collection("basic-001-fesom")
        assert isinstance(retrieved, dict)
        assert retrieved["id"] == "basic-001-fesom"

    def test_get_nonexistent_collection_returns_none(self, db):
        assert db.get_collection("does-not-exist") is None

    def test_insert_is_idempotent(self, db, ctx):
        col = make_collection(ctx)
        db.insert_collection(col)
        db.insert_collection(col)  # INSERT OR REPLACE — must not raise
        _, n = db.search_collections()
        assert n == 1

    def test_iter_collections(self, db, ctx):
        col = make_collection(ctx)
        db.insert_collection(col)
        cols = list(db.iter_collections())
        assert len(cols) == 1
        assert cols[0]["id"] == "basic-001-fesom"

    def test_update_collection_extent(self, db, ctx, sample_item):
        col = make_collection(ctx)
        db.insert_collection(col)
        db.update_collection_extent("basic-001-fesom", sample_item)
        updated = db.get_collection("basic-001-fesom")
        interval = updated["extent"]["temporal"]["interval"][0]
        assert interval[0] is not None


# ---------------------------------------------------------------------------
# storage/duckdb.py — items
# ---------------------------------------------------------------------------

class TestItemCRUD:
    def test_insert_item(self, db, ctx, sample_item):
        col = make_collection(ctx)
        db.insert_collection(col)
        db.insert_item(sample_item)
        items, total = db.search_items()
        assert total == 1

    def test_item_has_non_null_collection(self, db, ctx, sample_item):
        col = make_collection(ctx)
        db.insert_collection(col)
        db.insert_item(sample_item)
        row = db.db.execute(
            "SELECT collection FROM items WHERE id = ?", [sample_item["id"]]
        ).fetchone()
        assert row is not None
        assert row[0] is not None
        assert row[0] == "basic-001-fesom"

    def test_insert_is_idempotent(self, db, ctx, sample_item):
        col = make_collection(ctx)
        db.insert_collection(col)
        db.insert_item(sample_item)
        db.insert_item(sample_item)  # INSERT OR REPLACE
        _, total = db.search_items()
        assert total == 1

    def test_search_items_no_filter(self, db, ctx, sample_item):
        col = make_collection(ctx)
        db.insert_collection(col)
        db.insert_item(sample_item)
        items, total = db.search_items()
        assert total == 1
        assert items[0]["id"] == sample_item["id"]

    def test_search_items_by_experiment(self, db, ctx, sample_item):
        col = make_collection(ctx)
        db.insert_collection(col)
        db.insert_item(sample_item)
        items, total = db.search_items({"experiment": "basic-001"})
        assert total == 1
        items_none, total_none = db.search_items({"experiment": "nonexistent"})
        assert total_none == 0

    def test_search_items_by_collection(self, db, ctx, sample_item):
        col = make_collection(ctx)
        db.insert_collection(col)
        db.insert_item(sample_item)
        items, total = db.search_items({"collection": "basic-001-fesom"})
        assert total == 1

    def test_search_items_limit_and_offset(self, db, ctx, sample_metadata, fesom_nc):
        col = make_collection(ctx)
        db.insert_collection(col)
        # Insert 3 items with different IDs by tweaking datetime_str
        for i in range(3):
            meta = dict(sample_metadata, datetime_str=f"18500{i+1}")
            item = make_item(fesom_nc, meta, ctx)
            item["id"] = f"test-item-{i}"
            db.insert_item(item)
        _, total = db.search_items()
        assert total == 3
        items_page, _ = db.search_items(limit=2, offset=0)
        assert len(items_page) == 2
        items_page2, _ = db.search_items(limit=2, offset=2)
        assert len(items_page2) == 1


# ---------------------------------------------------------------------------
# storage/duckdb.py — collection_item_props index
# ---------------------------------------------------------------------------

class TestCollectionItemProps:
    def test_upsert_indexes_variable(self, db, ctx, sample_item):
        col = make_collection(ctx)
        db.insert_collection(col)
        db.insert_item(sample_item)
        db.upsert_collection_item_props("basic-001-fesom", sample_item)
        props = db.get_collection_item_props("basic-001-fesom")
        assert "variable" in props
        assert "ssh" in props["variable"]

    def test_upsert_indexes_experiment(self, db, ctx, sample_item):
        col = make_collection(ctx)
        db.insert_collection(col)
        db.upsert_collection_item_props("basic-001-fesom", sample_item)
        props = db.get_collection_item_props("basic-001-fesom")
        assert "basic-001" in props.get("experiment", set())

    def test_upsert_is_idempotent(self, db, ctx, sample_item):
        col = make_collection(ctx)
        db.insert_collection(col)
        db.upsert_collection_item_props("basic-001-fesom", sample_item)
        db.upsert_collection_item_props("basic-001-fesom", sample_item)
        props = db.get_collection_item_props("basic-001-fesom")
        assert len(props["variable"]) == 1  # no duplicates

    def test_search_collections_filters_by_item_props(self, db, ctx, sample_item):
        col = make_collection(ctx)
        db.insert_collection(col)
        db.insert_item(sample_item)
        db.upsert_collection_item_props("basic-001-fesom", sample_item)
        cols, n = db.search_collections({"variable": "ssh"})
        assert n == 1
        cols_none, n_none = db.search_collections({"variable": "temp"})
        assert n_none == 0


# ---------------------------------------------------------------------------
# storage/export.py
# ---------------------------------------------------------------------------

class TestExportParquet:
    def test_creates_parquet_file(self, tmp_path, sample_item):
        out = tmp_path / "items.parquet"
        export_parquet([sample_item], out)
        assert out.exists()

    def test_parquet_contains_correct_id(self, tmp_path, sample_item):
        import pyarrow.parquet as pq
        out = tmp_path / "items.parquet"
        export_parquet([sample_item], out)
        table = pq.read_table(str(out))
        ids = [v.as_py() for v in table.column("id")]
        assert sample_item["id"] in ids

    def test_parquet_data_is_valid_json(self, tmp_path, sample_item):
        import pyarrow.parquet as pq
        out = tmp_path / "items.parquet"
        export_parquet([sample_item], out)
        table = pq.read_table(str(out))
        for row in table.column("data"):
            parsed = json.loads(row.as_py())
            assert "id" in parsed

    def test_creates_parent_directories(self, tmp_path, sample_item):
        out = tmp_path / "deep" / "nested" / "items.parquet"
        export_parquet([sample_item], out)
        assert out.exists()


class TestExportJson:
    def test_creates_json_file(self, tmp_path, sample_item):
        out = tmp_path / "items.json"
        export_json([sample_item], out)
        assert out.exists()

    def test_json_is_feature_collection(self, tmp_path, sample_item):
        out = tmp_path / "items.json"
        export_json([sample_item], out)
        data = json.loads(out.read_text())
        assert data["type"] == "FeatureCollection"
        assert len(data["features"]) == 1

    def test_json_item_id_preserved(self, tmp_path, sample_item):
        out = tmp_path / "items.json"
        export_json([sample_item], out)
        data = json.loads(out.read_text())
        assert data["features"][0]["id"] == sample_item["id"]


class TestImportParquet:
    def test_imports_items_into_db(self, tmp_path, db, ctx, sample_item):
        col = make_collection(ctx)
        db.insert_collection(col)
        pq_path = tmp_path / "staging.parquet"
        export_parquet([sample_item], pq_path)
        import_parquet(db, [pq_path])
        _, total = db.search_items()
        assert total == 1

    def test_import_round_trip_preserves_collection(self, tmp_path, db, ctx, sample_item):
        col = make_collection(ctx)
        db.insert_collection(col)
        pq_path = tmp_path / "staging.parquet"
        export_parquet([sample_item], pq_path)
        import_parquet(db, [pq_path])
        items, _ = db.search_items()
        assert items[0]["collection"] == "basic-001-fesom"

    def test_import_multiple_parquet_files(self, tmp_path, db, ctx, sample_item, sample_metadata, fesom_nc):
        col = make_collection(ctx)
        db.insert_collection(col)
        meta2 = dict(sample_metadata, datetime_str="185002")
        item2 = make_item(fesom_nc, meta2, ctx)
        item2["id"] = "ssh.fesom.185002.test"
        pq1 = tmp_path / "batch1.parquet"
        pq2 = tmp_path / "batch2.parquet"
        export_parquet([sample_item], pq1)
        export_parquet([item2], pq2)
        import_parquet(db, [pq1, pq2])
        _, total = db.search_items()
        assert total == 2
