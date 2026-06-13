from __future__ import annotations
"""Merge multiple per-experiment DuckDB shards into a single global catalog."""

from pathlib import Path

from loguru import logger

from esm_catalog.storage.duckdb import CatalogDB


def merge_shards(
    shard_paths: list[Path | str],
    output_path: Path | str,
) -> tuple[int, int]:
    """Merge per-experiment shard .duckdb files into a single global catalog.

    Uses DuckDB ATTACH to read each shard and INSERT OR REPLACE all rows
    into the output database.  The output file is created if it does not
    exist; existing rows are updated (upserted) on ID conflict so the
    command is safe to re-run as shards are refreshed.

    Args:
        shard_paths: Ordered list of paths to source .duckdb shard files.
        output_path: Path to the global catalog .duckdb file (created if absent).

    Returns:
        (n_collections, n_items) — total rows merged across all shards.
    """
    output_path = Path(output_path).resolve()
    n_collections = 0
    n_items = 0

    with CatalogDB(output_path) as global_db:
        for shard_path in shard_paths:
            shard_path = Path(shard_path).resolve()
            if not shard_path.exists():
                logger.warning("Shard not found, skipping: {}", shard_path)
                continue

            logger.info("Merging shard: {}", shard_path)
            # Use a temporary alias; no two ATTACHed DBs can share a name.
            alias = f"_shard_{abs(hash(str(shard_path)))}"
            try:
                global_db.db.execute(
                    f"ATTACH '{shard_path}' AS {alias} (READ_ONLY)"
                )

                # collections: INSERT OR REPLACE on primary key (id)
                before_cols = global_db.db.execute(
                    "SELECT COUNT(*) FROM collections"
                ).fetchone()[0]
                global_db.db.execute(f"""
                    INSERT OR REPLACE INTO collections (id, data)
                    SELECT id, data FROM {alias}.collections
                """)
                after_cols = global_db.db.execute(
                    "SELECT COUNT(*) FROM collections"
                ).fetchone()[0]
                merged_cols = after_cols - before_cols

                # items: INSERT OR REPLACE on primary key (id)
                before_items = global_db.db.execute(
                    "SELECT COUNT(*) FROM items"
                ).fetchone()[0]
                global_db.db.execute(f"""
                    INSERT OR REPLACE INTO items
                        (id, collection, experiment, datetime, bbox, data)
                    SELECT id, collection, experiment, datetime, bbox, data
                    FROM {alias}.items
                """)
                after_items = global_db.db.execute(
                    "SELECT COUNT(*) FROM items"
                ).fetchone()[0]
                merged_items = after_items - before_items

                # collection_item_props: INSERT OR IGNORE (three-col PK)
                global_db.db.execute(f"""
                    INSERT OR IGNORE INTO collection_item_props
                        (collection_id, property, value)
                    SELECT collection_id, property, value
                    FROM {alias}.collection_item_props
                """)

                logger.info(
                    "  {} new collections, {} new items from {}",
                    merged_cols, merged_items, shard_path.name,
                )
                n_collections += merged_cols
                n_items += merged_items
            finally:
                try:
                    global_db.db.execute(f"DETACH {alias}")
                except Exception:
                    pass

    return n_collections, n_items
