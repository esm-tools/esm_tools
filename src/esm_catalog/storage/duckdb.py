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

        # Index each variable name in multi-variable files (GRIB).
        # Stored individually so collection-level searches also work.
        for var_name in props.get("variables", []):
            if var_name:
                self.db.execute(
                    """
                    INSERT OR IGNORE INTO collection_item_props
                        (collection_id, property, value)
                    VALUES (?, ?, ?)
                    """,
                    [collection_id, "variables", str(var_name)],
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
        """
        conditions = ["1=1"]
        params: list = []

        if filter_props:
            for field, spec in filter_props.items():
                # spec can be a single (op, val) tuple, a plain value,
                # or a list of (op, val) tuples for multiple AND conditions
                # on the same field (e.g. variable = 'ssh' AND variable = 'sst').
                specs = spec if isinstance(spec, list) else [spec]
                for s in specs:
                    if isinstance(s, tuple):
                        op, val = s
                    else:
                        op, val = "=", s

                    if field in ("id", "collection", "experiment"):
                        conditions.append(f"{field} {op} ?")
                        params.append(val)
                    elif field in ("datetime", "datetime_end"):
                        col = "datetime"
                        conditions.append(f"{col} {op} ?::TIMESTAMPTZ")
                        params.append(val)
                    elif field == "variables":
                        conditions.append(
                            "list_contains("
                            "    json_extract(data, '$.properties.variables')::VARCHAR[],"
                            "    ?"
                            ")"
                        )
                        params.append(val)
                    else:
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
        """Return True if *collection* satisfies all constraints in *filter_props*."""
        idx = self.get_collection_item_props(collection["id"])
        for field, spec in filter_props.items():
            specs = spec if isinstance(spec, list) else [spec]
            for s in specs:
                if isinstance(s, tuple):
                    op, val = s
                else:
                    op, val = "=", s

                # Check native collection field first
                native_val = collection.get(field)
                if native_val is not None and str(native_val) == str(val):
                    continue

                # Check item-derived property index
                indexed_vals = idx.get(field, set())
                if op == "=" and str(val) in indexed_vals:
                    continue

                # Constraint not satisfied
                return False
        return True

    def close(self):
        self.db.close()

    def __enter__(self):
        return self

    def __exit__(self, *_):
        self.close()
