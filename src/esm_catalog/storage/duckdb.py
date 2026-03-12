"""DuckDB-backed catalog storage: insert, query, and update STAC objects."""

import json
from pathlib import Path

import duckdb
from loguru import logger


class CatalogDB:
    """Per-experiment DuckDB catalog.

    One instance = one catalog.duckdb file.  The database is created with
    the full schema on first open; subsequent opens reuse the existing tables.
    """

    def __init__(self, path: Path | str):
        self.path = Path(path)
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self.db = duckdb.connect(str(self.path))
        self.db.execute("SET TimeZone='UTC'")  # all TIMESTAMPTZ reads return UTC
        self._init_schema()

    # ------------------------------------------------------------------
    # Schema
    # ------------------------------------------------------------------

    def _init_schema(self):
        self.db.execute("""
            CREATE TABLE IF NOT EXISTS items (
                id         TEXT PRIMARY KEY,
                collection TEXT,
                experiment TEXT,
                datetime   TIMESTAMPTZ,
                bbox       DOUBLE[],
                data       JSON
            )
        """)
        self.db.execute("""
            CREATE TABLE IF NOT EXISTS collections (
                id   TEXT PRIMARY KEY,
                data JSON
            )
        """)
        self.db.execute("""
            CREATE TABLE IF NOT EXISTS catalogs (
                id   TEXT PRIMARY KEY,
                data JSON
            )
        """)
        self.db.execute("""
            CREATE TABLE IF NOT EXISTS collection_item_props (
                collection_id TEXT,
                property      TEXT,
                value         TEXT,
                PRIMARY KEY (collection_id, property, value)
            )
        """)
        # Indexes (CREATE IF NOT EXISTS not supported; use try/except)
        for stmt in [
            "CREATE INDEX idx_collection ON items(collection)",
            "CREATE INDEX idx_experiment ON items(experiment)",
            "CREATE INDEX idx_datetime   ON items(datetime)",
        ]:
            try:
                self.db.execute(stmt)
            except duckdb.CatalogException:
                pass  # Already exists

    # ------------------------------------------------------------------
    # Collections
    # ------------------------------------------------------------------

    def collection_exists(self, collection_id: str) -> bool:
        row = self.db.execute(
            "SELECT 1 FROM collections WHERE id = ?", [collection_id]
        ).fetchone()
        return row is not None

    def insert_collection(self, collection: dict):
        self.db.execute(
            "INSERT OR REPLACE INTO collections (id, data) VALUES (?, ?)",
            [collection["id"], json.dumps(collection)],
        )
        logger.debug("Inserted collection: {}", collection["id"])

    def get_collection(self, collection_id: str) -> dict | None:
        row = self.db.execute(
            "SELECT data FROM collections WHERE id = ?", [collection_id]
        ).fetchone()
        if row is None:
            return None
        return json.loads(row[0])

    def update_collection_extent(self, collection_id: str, item: dict):
        """Re-read the collection, update extent, and write it back."""
        from esm_catalog.stac.collection import update_collection_extent

        collection = self.get_collection(collection_id)
        if collection is None:
            return
        updated = update_collection_extent(collection, item)
        self.db.execute(
            "UPDATE collections SET data = ? WHERE id = ?",
            [json.dumps(updated), collection_id],
        )

    def iter_collections(self):
        """Yield all collection dicts."""
        rows = self.db.execute("SELECT data FROM collections").fetchall()
        for (data,) in rows:
            yield json.loads(data)

    # ------------------------------------------------------------------
    # Items
    # ------------------------------------------------------------------

    def insert_item(self, item: dict):
        """Insert or replace a STAC Item."""
        props = item.get("properties", {})
        dt_str = props.get("datetime") or props.get("start_datetime")
        bbox = item.get("bbox")

        self.db.execute(
            """
            INSERT OR REPLACE INTO items
                (id, collection, experiment, datetime, bbox, data)
            VALUES (?, ?, ?, ?, ?, ?)
            """,
            [
                item["id"],
                item.get("collection"),
                props.get("experiment"),
                dt_str,
                bbox,
                json.dumps(item),
            ],
        )
        logger.debug("Inserted item: {}", item["id"])

    def upsert_collection_item_props(self, collection_id: str, item: dict):
        """Index item properties for fast collection search.

        Inserts (collection_id, property, value) rows for key item properties.
        INSERT OR IGNORE avoids duplicates.
        """
        props = item.get("properties", {})
        index_keys = ("variable", "experiment", "component", "format",
                      "hpc:facility", "hpc:system", "hpc:storage_tier")
        for key in index_keys:
            val = props.get(key)
            if val is not None:
                self.db.execute(
                    """
                    INSERT OR IGNORE INTO collection_item_props
                        (collection_id, property, value)
                    VALUES (?, ?, ?)
                    """,
                    [collection_id, key, str(val)],
                )

    def get_collection_item_props(self, collection_id: str) -> dict:
        """Return {property: set_of_values} index for *collection_id*."""
        rows = self.db.execute(
            "SELECT property, value FROM collection_item_props WHERE collection_id = ?",
            [collection_id],
        ).fetchall()
        result: dict[str, set] = {}
        for prop, val in rows:
            result.setdefault(prop, set()).add(val)
        return result

    # ------------------------------------------------------------------
    # Search
    # ------------------------------------------------------------------

    def search_items(
        self, filter_props: dict | None = None, limit: int = 100, offset: int = 0
    ) -> tuple[list[dict], int]:
        """Return (items, total_count) matching *filter_props*.

        filter_props: {field: (operator, value)} or {field: value}

        Supported operators:
        - Standard comparison: =, !=, <, <=, >, >=, LIKE
        - IN: for multi-value matching, value should be a list
        """
        conditions = ["1=1"]
        params: list = []

        if filter_props:
            for field, spec in filter_props.items():
                if isinstance(spec, tuple):
                    op, val = spec
                else:
                    op, val = "=", spec

                # Handle IN operator for multi-value matching
                if op == "IN":
                    if not isinstance(val, (list, tuple)) or not val:
                        continue  # Skip invalid IN clauses
                    placeholders = ", ".join("?" for _ in val)
                    if field in ("id", "collection", "experiment"):
                        conditions.append(f"{field} IN ({placeholders})")
                        params.extend(val)
                    else:
                        # JSON path query for item properties
                        conditions.append(
                            f"json_extract_string(data, '$.properties.{field}') IN ({placeholders})"
                        )
                        params.extend(str(v) for v in val)
                elif field in ("id", "collection", "experiment"):
                    conditions.append(f"{field} {op} ?")
                    params.append(val)
                elif field in ("datetime", "datetime_end"):
                    # datetime and datetime_end map to the native TIMESTAMPTZ column
                    col = "datetime"
                    conditions.append(f"{col} {op} ?::TIMESTAMPTZ")
                    params.append(val)
                else:
                    # JSON path query for item properties
                    conditions.append(
                        f"json_extract(data, '$.properties.{field}') {op} ?"
                    )
                    params.append(json.dumps(val))

        where = " AND ".join(conditions)
        total = self.db.execute(
            f"SELECT COUNT(*) FROM items WHERE {where}", params
        ).fetchone()[0]
        rows = self.db.execute(
            f"SELECT data FROM items WHERE {where} LIMIT ? OFFSET ?",
            params + [limit, offset],
        ).fetchall()
        items = [json.loads(r[0]) for r in rows]
        return items, total

    def search_collections(
        self, filter_props: dict | None = None, limit: int = 100, offset: int = 0
    ) -> tuple[list[dict], int]:
        """Return (collections, total_count) matching *filter_props*.

        Filters on both native collection fields and item-derived properties
        from the collection_item_props index.
        """
        matched = []
        for col in self.iter_collections():
            if filter_props and not self._collection_matches(col, filter_props):
                continue
            matched.append(col)
        total = len(matched)
        return matched[offset:offset + limit], total

    def _collection_matches(self, collection: dict, filter_props: dict) -> bool:
        """Return True if *collection* satisfies all constraints in *filter_props*.

        Supports CQL2 operators (=, <, >, <=, >=, <>) on:
        - Native collection fields
        - Namelist parameters (nml:group:key -> looks in nml:parameters)
        - Item-derived property index (for equality only)
        """
        idx = self.get_collection_item_props(collection["id"])

        for field, spec in filter_props.items():
            if isinstance(spec, tuple):
                op, val = spec
            else:
                op, val = "=", spec

            # Get the actual value to compare
            actual_val = self._get_collection_field_value(collection, field, idx)

            if actual_val is None:
                # Field not found - constraint fails
                return False

            # For indexed values (sets), only equality is supported
            if isinstance(actual_val, set):
                if op == "=" and str(val) in actual_val:
                    continue
                return False

            # Compare with CQL2 operator
            if not self._compare_values(actual_val, op, val):
                return False

        return True

    def _get_collection_field_value(
        self, collection: dict, field: str, idx: dict
    ):
        """Get value for a field from collection, namelist params, or index.

        Args:
            collection: Collection dict
            field: Field name (e.g., "experiment", "nml:radctl:co2vmr")
            idx: Item-derived property index

        Returns:
            Value if found, set of values from index, or None if not found.
        """
        # 1. Check native collection field
        if field in collection:
            return collection[field]

        # 2. Check namelist parameters (nml:group:key format)
        if field.startswith("nml:"):
            nml_params = collection.get("nml:parameters", {})
            # Field is "nml:radctl:co2vmr", key in nml:parameters is "radctl:co2vmr"
            param_key = field[4:]  # Remove "nml:" prefix
            if param_key in nml_params:
                return nml_params[param_key]

        # 3. Check item-derived property index (returns set)
        if field in idx:
            return idx[field]

        return None

    def _compare_values(self, actual, op: str, expected) -> bool:
        """Compare actual value against expected using CQL2 operator.

        Handles type coercion for numeric comparisons.
        """
        # Try numeric comparison first
        try:
            actual_num = float(actual)
            expected_num = float(expected)

            if op == "=":
                return actual_num == expected_num
            if op == "<":
                return actual_num < expected_num
            if op == ">":
                return actual_num > expected_num
            if op == "<=":
                return actual_num <= expected_num
            if op == ">=":
                return actual_num >= expected_num
            if op == "<>":
                return actual_num != expected_num
        except (ValueError, TypeError):
            pass

        # Fall back to string comparison
        actual_str = str(actual)
        expected_str = str(expected)

        if op == "=":
            return actual_str == expected_str
        if op == "<>":
            return actual_str != expected_str
        if op == "<":
            return actual_str < expected_str
        if op == ">":
            return actual_str > expected_str
        if op == "<=":
            return actual_str <= expected_str
        if op == ">=":
            return actual_str >= expected_str

        return False

    def close(self):
        self.db.close()

    def __enter__(self):
        return self

    def __exit__(self, *_):
        self.close()
