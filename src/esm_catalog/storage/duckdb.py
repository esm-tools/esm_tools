"""DuckDB-backed catalog storage: insert, query, and update STAC objects."""

import json
import os
from pathlib import Path

import duckdb
from loguru import logger


class CatalogDB:
    """Per-experiment DuckDB catalog.

    One instance = one catalog.duckdb file.  The database is created with
    the full schema on first open; subsequent opens reuse the existing tables.

    Thread Safety:
        DuckDB connections are not thread-safe for concurrent queries. While
        a single connection can be shared across threads, queries are serialized.
        This class uses cursor() to create thread-local query handles, ensuring
        safe concurrent access when used with a connection pool.

        See: https://duckdb.org/docs/stable/guides/python/multiple_threads
    """

    def __init__(self, path: Path | str):
        self.path = Path(path)
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self.db = duckdb.connect(str(self.path))
        # Limit internal worker threads. DuckDB defaults to one thread per logical
        # CPU; on a 256-core HPC node that spawns 256 threads per connection, which
        # quickly exhausts the per-user process/thread limit (ulimit -u).
        # 4 threads is ample for the read-heavy STAC API workload.
        _duckdb_threads = int(os.environ.get("ESM_CATALOG_DUCKDB_THREADS", "4"))
        self.db.execute(f"SET threads = {_duckdb_threads}")
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
        cursor = self.db.cursor()
        try:
            row = cursor.execute(
                "SELECT 1 FROM collections WHERE id = ?", [collection_id]
            ).fetchone()
            return row is not None
        finally:
            cursor.close()

    def insert_collection(self, collection: dict):
        self.db.execute(
            "INSERT OR REPLACE INTO collections (id, data) VALUES (?, ?)",
            [collection["id"], json.dumps(collection)],
        )
        logger.debug("Inserted collection: {}", collection["id"])

    def get_collection(self, collection_id: str) -> dict | None:
        cursor = self.db.cursor()
        try:
            row = cursor.execute(
                "SELECT data FROM collections WHERE id = ?", [collection_id]
            ).fetchone()
            if row is None:
                return None
            return json.loads(row[0])
        finally:
            cursor.close()

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
        """Yield all collection dicts.

        Thread Safety:
            Fetches all rows before yielding to avoid holding a cursor open
            during iteration, which could cause issues with concurrent queries.
        """
        cursor = self.db.cursor()
        try:
            rows = cursor.execute("SELECT data FROM collections").fetchall()
        finally:
            cursor.close()
        for (data,) in rows:
            yield json.loads(data)

    def add_component_to_collection(self, collection_id: str, component: str) -> None:
        """Append *component* to the collection's components list if not already present."""
        collection = self.get_collection(collection_id)
        if collection is None:
            return
        components = collection.get("components", [])
        if component not in components:
            components = sorted(set(components) | {component})
            collection["components"] = components
            self.db.execute(
                "UPDATE collections SET data = ? WHERE id = ?",
                [json.dumps(collection), collection_id],
            )
            logger.debug(
                "Added component '{}' to collection '{}'", component, collection_id
            )

    def iter_experiments(self) -> list[str]:
        """Return sorted list of distinct experiment IDs in this catalog."""
        cursor = self.db.cursor()
        try:
            rows = cursor.execute("""
                SELECT DISTINCT json_extract_string(data, '$.experiment')
                FROM collections
                WHERE json_extract_string(data, '$.experiment') IS NOT NULL
                  AND json_extract_string(data, '$.experiment') != ''
                ORDER BY 1
            """).fetchall()
        finally:
            cursor.close()
        return [row[0] for row in rows]

    def get_collections_for_experiment(self, experiment_id: str) -> list[dict]:
        """Return all collection dicts belonging to *experiment_id*."""
        cursor = self.db.cursor()
        try:
            rows = cursor.execute("""
                SELECT data FROM collections
                WHERE json_extract_string(data, '$.experiment') = ?
                ORDER BY id
            """, [experiment_id]).fetchall()
        finally:
            cursor.close()
        return [json.loads(row[0]) for row in rows]

    # ------------------------------------------------------------------
    # Items
    # ------------------------------------------------------------------

    def insert_item(self, item: dict):
        """Insert or update a STAC Item (upsert)."""
        props = item.get("properties", {})
        dt_str = props.get("datetime") or props.get("start_datetime")
        bbox = item.get("bbox")

        self.db.execute(
            """
            INSERT INTO items (id, collection, experiment, datetime, bbox, data)
            VALUES (?, ?, ?, ?, ?, ?)
            ON CONFLICT (id) DO UPDATE SET
                collection = EXCLUDED.collection,
                experiment = EXCLUDED.experiment,
                datetime = EXCLUDED.datetime,
                bbox = EXCLUDED.bbox,
                data = EXCLUDED.data
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
        logger.debug("Upserted item: {}", item["id"])

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

        # Index variable names under the "variables" property so the queryables
        # dropdown and filter cover both multi-variable files (properties.variables
        # array) and single-variable files (properties.variable string).
        var_names_to_index: list[str] = []
        for var in props.get("variables", []):
            if isinstance(var, dict):
                name = var.get("name")
            elif isinstance(var, str):
                name = var
            else:
                continue
            if name:
                var_names_to_index.append(name)
        # Single-variable files only have the singular "variable" field.
        if not var_names_to_index and props.get("variable"):
            var_names_to_index.append(str(props["variable"]))
        for var_name in var_names_to_index:
            self.db.execute(
                """
                INSERT OR IGNORE INTO collection_item_props
                    (collection_id, property, value)
                VALUES (?, ?, ?)
                """,
                [collection_id, "variables", var_name],
            )

    def reindex_variables_prop(self) -> int:
        """Rebuild the ``variables`` rows in collection_item_props from item data.

        Previous code stored ``str({"name": "ssh", ...})`` (dict repr) instead of
        the variable name string.  This method clears and rebuilds those rows so
        the queryables dropdown and variables filter work correctly.

        Returns:
            Number of items processed.
        """
        # Remove stale entries (dict-as-string values are unusable)
        self.db.execute(
            "DELETE FROM collection_item_props WHERE property = 'variables'"
        )
        total = 0
        batch_size = 500
        offset = 0
        while True:
            rows = self.db.execute(
                "SELECT collection, data FROM items LIMIT ? OFFSET ?",
                [batch_size, offset],
            ).fetchall()
            if not rows:
                break
            for collection_id, data_str in rows:
                item = json.loads(data_str) if isinstance(data_str, str) else data_str
                props = item.get("properties", {})
                var_names: list[str] = []
                for var in props.get("variables", []):
                    if isinstance(var, dict):
                        name = var.get("name")
                    elif isinstance(var, str):
                        name = var
                    else:
                        continue
                    if name:
                        var_names.append(name)
                # Single-variable files only set "variable" (singular).
                if not var_names and props.get("variable"):
                    var_names.append(str(props["variable"]))
                for var_name in var_names:
                    if collection_id:
                        self.db.execute(
                            "INSERT OR IGNORE INTO collection_item_props"
                            " (collection_id, property, value) VALUES (?, ?, ?)",
                            [collection_id, "variables", var_name],
                        )
                total += 1
            offset += batch_size
        logger.info("reindex_variables_prop: processed {} items", total)
        return total

    def get_collection_item_props(self, collection_id: str) -> dict:
        """Return {property: set_of_values} index for *collection_id*."""
        cursor = self.db.cursor()
        try:
            rows = cursor.execute(
                "SELECT property, value FROM collection_item_props WHERE collection_id = ?",
                [collection_id],
            ).fetchall()
        finally:
            cursor.close()
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

        Thread Safety:
            Uses a cursor to ensure thread-safe query execution. Each call
            gets its own cursor handle, preventing "closed pending query result"
            errors when multiple threads query the same connection concurrently.
        """
        conditions = ["1=1"]
        params: list = []

        if filter_props:
            for field, spec in filter_props.items():
                # OR list: a list of plain (non-tuple) values → IN clause
                if isinstance(spec, list) and spec and not isinstance(spec[0], tuple):
                    vals = spec
                    if field in ("id", "collection", "experiment"):
                        ph = ", ".join(["?"] * len(vals))
                        conditions.append(f"{field} IN ({ph})")
                        params.extend(vals)
                    elif field in ("datetime", "datetime_end"):
                        or_conds = ["datetime = ?::TIMESTAMPTZ" for _ in vals]
                        conditions.append(f"({' OR '.join(or_conds)})")
                        params.extend(vals)
                    elif field == "variables":
                        # Check both multi-var array (properties.variables) and
                        # single-var field (properties.variable) so all items are
                        # covered regardless of how many variables they contain.
                        or_conds = [
                            "list_contains("
                            "    COALESCE("
                            "        TRY_CAST(json_extract(data, '$.properties.variables') AS VARCHAR[]),"
                            "        CASE WHEN json_extract_string(data, '$.properties.variable') IS NOT NULL"
                            "             THEN list_value(json_extract_string(data, '$.properties.variable'))"
                            "             ELSE []::VARCHAR[] END"
                            "    ), ?)"
                            for _ in vals
                        ]
                        conditions.append(f"({' OR '.join(or_conds)})")
                        params.extend(vals)
                    else:
                        or_conds = [
                            f"json_extract(data, '$.properties.{field}') = ?"
                            for _ in vals
                        ]
                        conditions.append(f"({' OR '.join(or_conds)})")
                        params.extend([json.dumps(v) for v in vals])
                    continue

                # Check for ("IN", list) tuple: multi-value IN clause
                if isinstance(spec, tuple) and len(spec) == 2 and spec[0] == "IN":
                    _, val = spec
                    if isinstance(val, (list, tuple)) and val:
                        placeholders = ", ".join("?" for _ in val)
                        if field in ("id", "collection", "experiment"):
                            conditions.append(f"{field} IN ({placeholders})")
                            params.extend(val)
                        elif field in ("datetime", "datetime_end"):
                            or_conds = ["datetime = ?::TIMESTAMPTZ" for _ in val]
                            conditions.append(f"({' OR '.join(or_conds)})")
                            params.extend(val)
                        elif field == "variables":
                            or_conds = [
                                "list_contains("
                                "    COALESCE("
                                "        TRY_CAST(json_extract(data, '$.properties.variables') AS VARCHAR[]),"
                                "        CASE WHEN json_extract_string(data, '$.properties.variable') IS NOT NULL"
                                "             THEN list_value(json_extract_string(data, '$.properties.variable'))"
                                "             ELSE []::VARCHAR[] END"
                                "    ), ?)"
                                for _ in val
                            ]
                            conditions.append(f"({' OR '.join(or_conds)})")
                            params.extend(val)
                        else:
                            conditions.append(
                                f"json_extract_string(data, '$.properties.{field}') IN ({placeholders})"
                            )
                            params.extend(str(v) for v in val)
                    continue

                # AND: single (op, val) tuple, plain value,
                # or list of (op, val) tuples for multiple conditions on same field.
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
                            "    COALESCE("
                            "        TRY_CAST(json_extract(data, '$.properties.variables') AS VARCHAR[]),"
                            "        CASE WHEN json_extract_string(data, '$.properties.variable') IS NOT NULL"
                            "             THEN list_value(json_extract_string(data, '$.properties.variable'))"
                            "             ELSE []::VARCHAR[] END"
                            "    ), ?"
                            ")"
                        )
                        params.append(val)
                    else:
                        if isinstance(val, (int, float)):
                            # Numeric comparison: cast JSON to DOUBLE for correct
                            # ordering.  Without this, DuckDB compares JSON vs
                            # VARCHAR lexicographically: '100' >= '90' → False.
                            conditions.append(
                                f"CAST(json_extract(data, '$.properties.{field}') AS DOUBLE) {op} ?"
                            )
                            params.append(float(val))
                        else:
                            conditions.append(
                                f"json_extract(data, '$.properties.{field}') {op} ?"
                            )
                            params.append(json.dumps(val))

        where = " AND ".join(conditions)

        # Use a cursor for thread-safe query execution.
        # This prevents "InvalidInputException: Attempting to execute an
        # unsuccessful or closed pending query result" errors during concurrent
        # pagination requests. Each cursor provides an isolated query context.
        cursor = self.db.cursor()
        try:
            count_result = cursor.execute(
                f"SELECT COUNT(*) FROM items WHERE {where}", params
            ).fetchone()
            total = count_result[0] if count_result is not None else 0
            rows = cursor.execute(
                f"SELECT data FROM items WHERE {where} LIMIT ? OFFSET ?",
                params + [limit, offset],
            ).fetchall()
            items = [json.loads(r[0]) for r in rows]

            # Debug logging for single-item lookups (when both id and collection are specified)
            if filter_props and "id" in filter_props and "collection" in filter_props:
                if total == 0:
                    logger.debug(
                        "search_items: No items found for collection={!r}, id={!r}",
                        filter_props.get("collection"),
                        filter_props.get("id"),
                    )
                else:
                    logger.debug(
                        "search_items: Found {} item(s) for collection={!r}, id={!r}",
                        total,
                        filter_props.get("collection"),
                        filter_props.get("id"),
                    )

            return items, total
        finally:
            cursor.close()

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
            # OR list: plain values — collection matches if any value is present
            if isinstance(spec, list) and spec and not isinstance(spec[0], tuple):
                native_val = collection.get(field)
                if native_val is not None and str(native_val) in [str(v) for v in spec]:
                    continue
                indexed_vals = idx.get(field, set())
                if any(str(v) in indexed_vals for v in spec):
                    continue
                return False

            # Extract op and val from spec
            if isinstance(spec, tuple) and len(spec) == 2:
                op, val = spec
            else:
                op, val = "=", spec

            # Handle IN operator: ("IN", [v1, v2, ...])
            if op == "IN":
                if not isinstance(val, (list, tuple)) or not val:
                    return False
                native_val = collection.get(field)
                if native_val is not None and str(native_val) in [str(v) for v in val]:
                    continue
                indexed_vals = idx.get(field, set())
                if indexed_vals and all(str(v) in indexed_vals for v in val):
                    continue
                return False

            # Get the actual value to compare using helper
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

        # 2. Check namelist parameters.
        # Queryables exposes these as "nml:group.param" (dot sub-separator)
        # to avoid double colons that break STAC Browser's filter builder.
        # The stored key in nml:parameters is "group:param" (colon), so we
        # try both the incoming key as-is AND with dots converted to colons.
        if field.startswith("nml:"):
            nml_params = collection.get("nml:parameters", {})
            param_key = field[4:]  # Remove "nml:" prefix → e.g. "radctl.co2vmr"
            if param_key in nml_params:
                return nml_params[param_key]
            # Try colon form for backwards compatibility and dot→colon conversion
            colon_key = param_key.replace(".", ":")  # "radctl.co2vmr" → "radctl:co2vmr"
            if colon_key in nml_params:
                return nml_params[colon_key]

        # 3. Check item-derived property index (returns set)
        if field in idx:
            return idx[field]

        # 4. Check collection summaries (e.g., "paleo:years_bp")
        summaries = collection.get("summaries", {})
        if field in summaries:
            values = summaries[field]
            # Return first value if single-element list, otherwise return list
            if isinstance(values, list) and len(values) == 1:
                return values[0]
            return values

        return None

    def _compare_values(self, actual, op: str, expected) -> bool:
        """Compare actual value against expected using CQL2 operator.

        Args:
            actual: The value from the collection/item (may be scalar or list).
            op: CQL2 operator string: "=", "<>", "<", ">", "<=", ">=".
            expected: The value to compare against (from the filter).

        Returns:
            True if the comparison matches, False otherwise.

        List Semantics (ANY/SOME):
            When `actual` is a list (e.g., from collection summaries with
            multiple values), uses ANY semantics: returns True if ANY element
            in the list satisfies the comparison.

            Example: actual=[1, 100], op=">", expected=50
                     Returns True because 100 > 50, even though 1 < 50.

            This matches SQL's "field IN (...)" or "EXISTS" behavior and is
            appropriate for collection-level filtering where we want to find
            collections containing at least one matching value.

        Type Coercion:
            Attempts numeric comparison first. If both values can be converted
            to float, uses numeric comparison. Otherwise falls back to string
            comparison.

        Edge Cases:
            - Empty list: Returns False (no element can match).
            - NaN values: Numeric comparison may fail, falls back to string.
            - None values: Converted to string "None" for comparison.
        """
        # Handle empty list - no matches possible
        if isinstance(actual, list) and len(actual) == 0:
            return False

        # Handle list values (e.g., from summaries with multiple values)
        # Uses ANY/SOME semantics: True if ANY value matches
        if isinstance(actual, list):
            return any(self._compare_values(v, op, expected) for v in actual)

        # Handle None values explicitly
        if actual is None:
            return False

        # Handle boolean values (e.g. nml:run_config.use_ice = true).
        # Must be checked BEFORE numeric conversion because float(True)=1.0
        # but the filter value is the string "true"/"false", not "1"/"0".
        if isinstance(actual, bool):
            expected_lower = str(expected).lower()
            if op in ("=", "!=", "<>"):
                expected_bool = expected_lower in ("true", "1", "yes")
                matches = actual == expected_bool
                return matches if op == "=" else not matches
            # Booleans don't support ordered comparisons
            return False

        # Try numeric comparison first
        try:
            actual_num = float(actual)
            expected_num = float(expected)

            # Guard against NaN comparisons (NaN != NaN in IEEE 754)
            import math
            if math.isnan(actual_num) or math.isnan(expected_num):
                # Fall through to string comparison for NaN
                raise ValueError("NaN comparison")

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
