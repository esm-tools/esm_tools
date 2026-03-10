"""Bridge from ESM-Tools tidy phase to the catalog.

Public API:
    add_files(db, files, experiment_config)

Called by the ESM-Tools tidy phase after output files are finalised.
ESM-Tools already knows what it wrote, so no filesystem scan is needed —
we receive the file list directly, which is faster and avoids the HPC
rate-limiting problem for tape systems.
"""

from __future__ import annotations

from pathlib import Path

from loguru import logger

from esm_catalog.scan.context import resolve_context
from esm_catalog.scan.detect import scan_file
from esm_catalog.stac.extensions.hpc import add_hpc_extension
from esm_catalog.stac.item import make_item
from esm_catalog.storage.duckdb import CatalogDB


def add_files(
    db: str | Path,
    files: list[Path | str],
    experiment_config: dict,
) -> int:
    """Catalog *files* into the DuckDB at *db*.

    Args:
        db:                Path to the catalog.duckdb file (created if absent).
        files:             Iterable of paths to output files produced by ESM-Tools.
        experiment_config: The finished_config dict (or equivalent) with at
                           minimum config["general"]["expid"] set.

    Returns:
        Number of items successfully inserted.
    """
    ok = 0
    seen_real: set[Path] = set()
    with CatalogDB(db) as catalog_db:
        for raw_path in files:
            fp = Path(raw_path).resolve()  # follow symlinks; deduplicate
            if fp in seen_real:
                logger.debug("Skipping duplicate (symlink target already seen): {}", fp)
                continue
            if fp.stat().st_size == 0:
                logger.debug("Skipping zero-byte file: {}", fp)
                continue
            seen_real.add(fp)
            try:
                ctx = resolve_context(fp, config=experiment_config, db=catalog_db)
                metadata = scan_file(fp)
                item = make_item(fp, metadata, ctx, config=experiment_config)
                item = add_hpc_extension(item, fp)
                catalog_db.insert_item(item)
                catalog_db.update_collection_extent(ctx.collection_id, item)
                catalog_db.upsert_collection_item_props(ctx.collection_id, item)
                ok += 1
                logger.debug("Cataloged: {}", fp)
            except Exception as e:
                logger.error("Failed to catalog {}: {}", fp, e)

    logger.info("add_files: {}/{} files cataloged into {}", ok, len(list(files)), db)
    return ok
