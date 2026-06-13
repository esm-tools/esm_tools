"""Bridge from ESM-Tools tidy phase to the catalog.

Public API:
    add_files(db, files, experiment_config, checksums=None)
    add_run(db, experiment_dir, component, run_datestamp, experiment_config)

``add_run()`` is the preferred entry point: it implements the source priority
chain (file_operations_tidy → finished_config → nothing) and automatically
passes checksums from the tidy log to ``add_files()``.

``add_files()`` is the lower-level primitive: it catalogs an explicit list of
paths, optionally with pre-computed checksums.
"""

from __future__ import annotations

from pathlib import Path

from loguru import logger

from esm_catalog.scan.context import resolve_context
from esm_catalog.scan.detect import scan_file
from esm_catalog.stac.extensions.hpc import add_hpc_extension
from esm_catalog.stac.extensions.registry import EXTENSION_URLS
from esm_catalog.stac.item import make_item
from esm_catalog.storage.duckdb import CatalogDB


def add_files(
    db: str | Path,
    files: list[Path | str],
    experiment_config: dict,
    checksums: dict[str, str] | None = None,
) -> int:
    """Catalog *files* into the DuckDB at *db*.

    Args:
        db:                Path to the catalog.duckdb file (created if absent).
        files:             Iterable of paths to output files produced by ESM-Tools.
        experiment_config: The finished_config dict (or equivalent) with at
                           minimum ``config["general"]["expid"]`` set.
        checksums:         Optional mapping of ``str(resolved_path)`` → MD5 hex
                           string.  When provided, the checksum is stored in the
                           STAC item asset as ``file:checksum`` and the STAC
                           ``file`` extension is added.  Typically sourced from
                           :func:`~esm_catalog.integration.config.get_outdata_from_file_operations`.

    Returns:
        Number of items successfully inserted.
    """
    files = list(files)
    ok = 0
    seen_real: set[Path] = set()
    with CatalogDB(db) as catalog_db:
        for raw_path in files:
            fp = Path(raw_path).resolve()  # follow symlinks; deduplicate
            if not fp.exists():
                logger.debug("Skipping missing file: {}", fp)
                continue
            if fp in seen_real:
                logger.debug("Skipping duplicate (symlink target already seen): {}", fp)
                continue
            if fp.stat().st_size == 0:
                logger.debug("Skipping zero-byte file: {}", fp)
                continue
            seen_real.add(fp)
            try:
                ctx = resolve_context(fp, config=experiment_config)
                metadata = scan_file(fp)
                item = make_item(fp, metadata, ctx, config=experiment_config)
                item = add_hpc_extension(item, fp)

                if checksums:
                    checksum = checksums.get(str(fp))
                    if checksum:
                        item["assets"]["data"]["file:checksum"] = checksum
                        url = EXTENSION_URLS["file"]
                        if url not in item["stac_extensions"]:
                            item["stac_extensions"].append(url)

                catalog_db.insert_item(item)
                catalog_db.update_collection_extent(ctx.collection_id, item)
                catalog_db.upsert_collection_item_props(ctx.collection_id, item)
                ok += 1
                logger.debug("Cataloged: {}", fp)
            except Exception as e:
                logger.error("Failed to catalog {}: {}", fp, e)

    logger.info("add_files: {}/{} files cataloged into {}", ok, len(files), db)
    return ok


def add_run(
    db: str | Path,
    experiment_dir: Path | str,
    component: str,
    run_datestamp: str,
    experiment_config: dict,
) -> int:
    """Catalog all output files for one component run.

    Implements the source priority chain:

    1. **file_operations_tidy YAML** — preferred when available; exact file
       list with MD5 checksums already computed by ESM-Tools during tidy.
    2. **finished_config.yaml outdata_targets** — fallback; paths only,
       no checksums.

    Example usage in an ESM-Tools run script::

        from esm_catalog.integration.esm_tools import add_run

        add_run(
            db=exp_dir / "catalog.duckdb",
            experiment_dir=exp_dir,
            component="fesom",
            run_datestamp="19580101-19580131",
            experiment_config=config,
        )

    Args:
        db:                Path to the catalog.duckdb file.
        experiment_dir:    Root experiment directory.
        component:         Component name, e.g. ``"fesom"``.
        run_datestamp:     Date range string, e.g. ``"19580101-19580131"``.
        experiment_config: Loaded finished_config dict; required for
                           collection context (``general.expid``).

    Returns:
        Number of items successfully inserted.
    """
    from esm_catalog.integration.config import (
        find_file_operations_log,
        get_outdata_files,
        get_outdata_from_file_operations,
    )

    experiment_dir = Path(experiment_dir)
    files: list[Path] = []
    checksums: dict[str, str] = {}

    log_path = find_file_operations_log(experiment_dir, component, run_datestamp)
    if log_path:
        records = get_outdata_from_file_operations(log_path)
        files = [r["destination"] for r in records]
        checksums = {
            str(r["destination"].resolve()): r["checksum"]
            for r in records
            if r["checksum"]
        }
        logger.info(
            "add_run: using file_operations_tidy log ({} files, {} checksums): {}",
            len(files), len(checksums), log_path.name,
        )
    else:
        files = get_outdata_files(experiment_config, component)
        logger.info(
            "add_run: file_operations_tidy not found; "
            "using finished_config outdata_targets ({} files)",
            len(files),
        )

    if not files:
        logger.debug(
            "add_run: no outdata files found for {}/{}", experiment_dir.name, component
        )
        return 0

    return add_files(db, files, experiment_config, checksums=checksums or None)
