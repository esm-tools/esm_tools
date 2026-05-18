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


# ---------------------------------------------------------------------------
# storage/duckdb.py — _compare_values behavioral tests
# ---------------------------------------------------------------------------

class TestCompareValues:
    """Behavioral tests for CatalogDB._compare_values method.

    Tests cover:
    - Scalar numeric comparison
    - Scalar string comparison
    - List value handling (ANY semantics)
    - Edge cases (None, NaN, empty lists)
    - Type coercion behavior
    """

    def test_scalar_numeric_equality(self, db):
        """Numeric values are compared as floats."""
        assert db._compare_values(42, "=", 42) is True
        assert db._compare_values(42.0, "=", 42) is True
        assert db._compare_values(42, "=", 43) is False

    def test_scalar_numeric_inequality(self, db):
        """Numeric inequality operators work correctly."""
        assert db._compare_values(10, "<", 20) is True
        assert db._compare_values(20, "<", 10) is False
        assert db._compare_values(20, ">", 10) is True
        assert db._compare_values(10, ">", 20) is False

    def test_scalar_numeric_bounds(self, db):
        """Numeric <= and >= include boundary values."""
        assert db._compare_values(10, "<=", 10) is True
        assert db._compare_values(10, ">=", 10) is True
        assert db._compare_values(9, "<=", 10) is True
        assert db._compare_values(11, ">=", 10) is True

    def test_scalar_not_equal(self, db):
        """<> operator tests inequality."""
        assert db._compare_values(10, "<>", 20) is True
        assert db._compare_values(10, "<>", 10) is False

    def test_string_comparison(self, db):
        """String comparison for non-numeric values."""
        assert db._compare_values("abc", "=", "abc") is True
        assert db._compare_values("abc", "=", "ABC") is False
        assert db._compare_values("abc", "<>", "xyz") is True

    def test_string_ordering(self, db):
        """String comparison respects lexicographic order."""
        assert db._compare_values("apple", "<", "banana") is True
        assert db._compare_values("zebra", ">", "apple") is True

    def test_numeric_string_coercion(self, db):
        """Numeric strings are compared as numbers."""
        assert db._compare_values("42", "=", 42) is True
        assert db._compare_values("10.5", "<", 11) is True

    def test_list_any_semantics_equality(self, db):
        """List comparison uses ANY semantics: True if ANY element matches."""
        assert db._compare_values([1, 2, 3], "=", 2) is True
        assert db._compare_values([1, 2, 3], "=", 5) is False

    def test_list_any_semantics_inequality(self, db):
        """List comparison: True if ANY element satisfies the operator."""
        # [1, 100] > 50: True because 100 > 50
        assert db._compare_values([1, 100], ">", 50) is True
        # [1, 100] < 50: True because 1 < 50
        assert db._compare_values([1, 100], "<", 50) is True
        # [60, 70, 80] < 50: False because no element < 50
        assert db._compare_values([60, 70, 80], "<", 50) is False

    def test_empty_list_returns_false(self, db):
        """Empty list cannot match any condition."""
        assert db._compare_values([], "=", 1) is False
        assert db._compare_values([], ">", 0) is False
        assert db._compare_values([], "<", 100) is False

    def test_none_value_returns_false(self, db):
        """None actual value never matches."""
        assert db._compare_values(None, "=", None) is False
        assert db._compare_values(None, "=", "None") is False

    def test_nan_falls_back_to_string(self, db):
        """NaN values fall back to string comparison."""
        import math
        # NaN comparisons use string fallback
        # float('nan') as string is "nan"
        result = db._compare_values(float('nan'), "=", float('nan'))
        # Both become string "nan", so they are equal as strings
        assert result is True

    def test_mixed_type_list(self, db):
        """Lists with mixed types work correctly."""
        # [1, "two", 3] - "two" can't be converted to float, uses string
        assert db._compare_values([1, "two", 3], "=", 1) is True
        assert db._compare_values([1, "two", 3], "=", "two") is True

    def test_float_precision(self, db):
        """Float comparison handles precision correctly."""
        # This tests that we're comparing as floats, not exact decimals
        assert db._compare_values(0.1 + 0.2, "=", 0.3) is False  # Floating point imprecision
        # But reasonable tolerances work
        assert db._compare_values(0.284e-3, ">", 0.283e-3) is True


# ---------------------------------------------------------------------------
# storage/duckdb.py — collection summaries filtering tests
# ---------------------------------------------------------------------------

class TestCollectionSummariesFiltering:
    """Tests for filtering collections by summaries values."""

    def test_filter_collection_by_summary_scalar(self, db, ctx):
        """Filter collections by single-value summary field."""
        col = make_collection(ctx)
        col["summaries"] = {"paleo:years_bp": [21000]}  # Single-element list
        db.insert_collection(col)

        # Should match
        cols, n = db.search_collections({"paleo:years_bp": ("=", 21000)})
        assert n == 1

        # Should not match
        cols, n = db.search_collections({"paleo:years_bp": ("=", 6000)})
        assert n == 0

    def test_filter_collection_by_summary_range(self, db, ctx):
        """Filter collections by summary field with range operators."""
        col = make_collection(ctx)
        col["summaries"] = {"paleo:years_bp": [21000]}
        db.insert_collection(col)

        # Should match: 21000 > 10000
        cols, n = db.search_collections({"paleo:years_bp": (">", 10000)})
        assert n == 1

        # Should not match: 21000 < 10000
        cols, n = db.search_collections({"paleo:years_bp": ("<", 10000)})
        assert n == 0

    def test_filter_collection_by_summary_multi_value(self, db, ctx):
        """Filter collections by multi-value summary (ANY semantics)."""
        col = make_collection(ctx)
        # Multiple paleo periods represented in this collection
        col["summaries"] = {"paleo:years_bp": [6000, 21000, 125000]}
        db.insert_collection(col)

        # Should match: 21000 is in the list
        cols, n = db.search_collections({"paleo:years_bp": ("=", 21000)})
        assert n == 1

        # Should match: 6000 < 10000 (at least one element matches)
        cols, n = db.search_collections({"paleo:years_bp": ("<", 10000)})
        assert n == 1

        # Should not match: no value equals 50000
        cols, n = db.search_collections({"paleo:years_bp": ("=", 50000)})
        assert n == 0

    def test_filter_collection_by_namelist_param(self, db, ctx):
        """Filter collections by namelist parameters."""
        col = make_collection(ctx)
        col["nml:parameters"] = {"radctl:co2vmr": 0.000284}
        db.insert_collection(col)

        # Should match
        cols, n = db.search_collections({"nml:radctl:co2vmr": ("=", 0.000284)})
        assert n == 1

        # Should match with comparison
        cols, n = db.search_collections({"nml:radctl:co2vmr": (">", 0.00028)})
        assert n == 1

    def test_filter_collection_multiple_constraints(self, db, ctx):
        """Filter collections with multiple constraints."""
        col = make_collection(ctx)
        col["summaries"] = {"paleo:years_bp": [21000]}
        col["nml:parameters"] = {"radctl:co2vmr": 0.000190}  # LGM CO2 ~190ppm
        db.insert_collection(col)

        # Both constraints must match
        cols, n = db.search_collections({
            "paleo:years_bp": ("=", 21000),
            "nml:radctl:co2vmr": ("<", 0.0002),
        })
        assert n == 1

        # One constraint fails
        cols, n = db.search_collections({
            "paleo:years_bp": ("=", 21000),
            "nml:radctl:co2vmr": (">", 0.0003),  # LGM CO2 was lower
        })
        assert n == 0


# ---------------------------------------------------------------------------
# storage/duckdb.py — _get_collection_field_value tests
# ---------------------------------------------------------------------------

class TestGetCollectionFieldValue:
    """Tests for _get_collection_field_value method."""

    def test_get_native_field(self, db, ctx):
        """Get a native collection field."""
        col = make_collection(ctx)
        col["description"] = "Test collection"
        db.insert_collection(col)

        # Retrieve and test
        retrieved = db.get_collection("basic-001-fesom")
        idx = db.get_collection_item_props("basic-001-fesom")
        val = db._get_collection_field_value(retrieved, "description", idx)
        assert val == "Test collection"

    def test_get_namelist_param(self, db, ctx):
        """Get a namelist parameter via nml: prefix."""
        col = make_collection(ctx)
        col["nml:parameters"] = {"radctl:co2vmr": 0.000284}
        db.insert_collection(col)

        retrieved = db.get_collection("basic-001-fesom")
        idx = db.get_collection_item_props("basic-001-fesom")
        val = db._get_collection_field_value(retrieved, "nml:radctl:co2vmr", idx)
        assert val == 0.000284

    def test_get_summary_single_value(self, db, ctx):
        """Get a single-element summary (returns scalar)."""
        col = make_collection(ctx)
        col["summaries"] = {"paleo:years_bp": [21000]}
        db.insert_collection(col)

        retrieved = db.get_collection("basic-001-fesom")
        idx = db.get_collection_item_props("basic-001-fesom")
        val = db._get_collection_field_value(retrieved, "paleo:years_bp", idx)
        # Single-element list returns the scalar value
        assert val == 21000

    def test_get_summary_multi_value(self, db, ctx):
        """Get a multi-element summary (returns list)."""
        col = make_collection(ctx)
        col["summaries"] = {"paleo:years_bp": [6000, 21000]}
        db.insert_collection(col)

        retrieved = db.get_collection("basic-001-fesom")
        idx = db.get_collection_item_props("basic-001-fesom")
        val = db._get_collection_field_value(retrieved, "paleo:years_bp", idx)
        # Multi-element list returns the list
        assert val == [6000, 21000]

    def test_get_indexed_property(self, db, ctx, sample_item):
        """Get an item-derived indexed property (returns set)."""
        col = make_collection(ctx)
        db.insert_collection(col)
        db.insert_item(sample_item)
        db.upsert_collection_item_props("basic-001-fesom", sample_item)

        retrieved = db.get_collection("basic-001-fesom")
        idx = db.get_collection_item_props("basic-001-fesom")
        val = db._get_collection_field_value(retrieved, "variable", idx)
        assert isinstance(val, set)
        assert "ssh" in val

    def test_get_nonexistent_field(self, db, ctx):
        """Get a field that doesn't exist returns None."""
        col = make_collection(ctx)
        db.insert_collection(col)

        retrieved = db.get_collection("basic-001-fesom")
        idx = db.get_collection_item_props("basic-001-fesom")
        val = db._get_collection_field_value(retrieved, "nonexistent:field", idx)
        assert val is None
