"""ESM Catalog CLI entry point.

Commands:
    scan          Scan a single file or directory into a catalog
    scan-batch    Scan a list of files and write to Parquet (for SLURM arrays)
    merge-parquet Merge Parquet staging files into a catalog.duckdb
    serve         Launch the STAC API server (Phase 3)
"""

from __future__ import annotations

import sys
from pathlib import Path

import rich_click as click
from loguru import logger

click.rich_click.USE_RICH_MARKUP = True
click.rich_click.SHOW_ARGUMENTS = True


def _configure_logging(verbose: bool):
    logger.remove()
    level = "DEBUG" if verbose else "INFO"
    logger.add(sys.stderr, level=level, format="{time:HH:mm:ss} | {level:<8} | {message}")


# ------------------------------------------------------------------
# Main group
# ------------------------------------------------------------------

@click.group()
@click.option("--verbose", "-v", is_flag=True, default=False, help="Enable debug logging")
@click.pass_context
def main(ctx, verbose):
    """[bold]ESM Catalog[/bold] — STAC-based catalog for ESM-Tools experiment output."""
    ctx.ensure_object(dict)
    ctx.obj["verbose"] = verbose
    _configure_logging(verbose)


# ------------------------------------------------------------------
# scan
# ------------------------------------------------------------------

@main.command()
@click.argument("path", type=click.Path(exists=True, path_type=Path))
@click.option(
    "--db", "db_path",
    required=True,
    type=click.Path(path_type=Path),
    help="Path to the catalog.duckdb file (created if absent)",
)
@click.option(
    "--config", "config_path",
    default=None,
    type=click.Path(exists=True, path_type=Path),
    help="ESM-Tools finished_config.yaml (enables config-based context resolution)",
)
@click.pass_context
def scan(ctx, path, db_path, config_path):
    """Scan PATH (file or directory) into the catalog at DB.

    PATH can be a single file (.nc, .grb, .grib2, ...) or a directory.
    All supported files in a directory are scanned recursively.
    """
    from esm_catalog.integration.config import load_config
    from esm_catalog.scan.context import resolve_context
    from esm_catalog.scan.detect import UnsupportedFormatError, scan_file
    from esm_catalog.stac.extensions.hpc import add_hpc_extension
    from esm_catalog.stac.item import make_item
    from esm_catalog.storage.duckdb import CatalogDB

    config = load_config(config_path) if config_path else None

    files: list[Path] = []
    if path.is_file():
        files = [path.resolve()]
    else:
        known_exts = {".nc", ".nc4", ".nc3", ".cdf", ".h5", ".hdf5", ".hdf",
                      ".grb", ".grb2", ".grib", ".grib2"}
        skip_exts  = {".codes", ".txt", ".log", ".sh", ".py", ".yaml", ".yml", ".json"}

        # Deduplicate by resolved real path to handle symlinks and hardlinks
        seen_real: set[Path] = set()

        def _add(f: Path) -> None:
            real = f.resolve()
            if real in seen_real or real.stat().st_size == 0:
                return
            seen_real.add(real)
            files.append(real)

        # Known extensions (fast path)
        for ext in ("*.nc", "*.nc4", "*.grb", "*.grb2", "*.grib", "*.grib2"):
            for f in path.rglob(ext):
                _add(f)

        # Extension-less candidates — detected via magic bytes in scan_file()
        for candidate in path.rglob("*"):
            if candidate.is_file() and candidate.suffix.lower() not in known_exts | skip_exts:
                _add(candidate)

    if not files:
        logger.warning("No supported files found at: {}", path)
        return

    with CatalogDB(db_path) as db:
        ok = 0
        for fp in files:
            try:
                ctx_col = resolve_context(fp, config=config, db=db)
                metadata = scan_file(fp)
                item = make_item(fp, metadata, ctx_col, config=config)
                item = add_hpc_extension(item, fp)
                db.insert_item(item)
                db.update_collection_extent(ctx_col.collection_id, item)
                db.upsert_collection_item_props(ctx_col.collection_id, item)
                ok += 1
            except UnsupportedFormatError:
                logger.debug("Skipping unsupported file: {}", fp)
            except ValueError as e:
                logger.error("Skipping {}: {}", fp, e)
            except Exception as e:
                logger.error("Error scanning {}: {}", fp, e)
                if ctx.obj.get("verbose"):
                    raise

    logger.info("Scanned {}/{} files into {}", ok, len(files), db_path)


# ------------------------------------------------------------------
# scan-batch
# ------------------------------------------------------------------

@main.command("scan-batch")
@click.argument("files", nargs=-1, required=True, type=click.Path(path_type=Path))
@click.option(
    "--config", "config_path",
    required=True,
    type=click.Path(exists=True, path_type=Path),
    help="ESM-Tools finished_config.yaml",
)
@click.option(
    "--output", "output_path",
    required=True,
    type=click.Path(path_type=Path),
    help="Output Parquet file (staging area for merge step)",
)
@click.option("--jobs", "-j", default=4, show_default=True, help="Parallel worker count")
@click.pass_context
def scan_batch(ctx, files, config_path, output_path, jobs):
    """Scan FILES in parallel and write STAC Items to a Parquet staging file.

    Designed for SLURM array jobs: each array task scans a batch of files and
    writes a Parquet file. A subsequent serial `merge-parquet` job assembles
    all Parquet files into catalog.duckdb.
    """
    from joblib import Parallel, delayed

    from esm_catalog.integration.config import load_config
    from esm_catalog.scan.context import resolve_context
    from esm_catalog.scan.detect import UnsupportedFormatError, scan_file
    from esm_catalog.stac.extensions.hpc import add_hpc_extension
    from esm_catalog.stac.item import make_item
    from esm_catalog.storage.export import export_parquet

    config = load_config(config_path)

    def process_one(fp: Path) -> dict | None:
        try:
            ctx_col = resolve_context(fp, config=config, db=None)
            metadata = scan_file(fp)
            item = make_item(fp, metadata, ctx_col, config=config)
            item = add_hpc_extension(item, fp)
            return item
        except UnsupportedFormatError:
            return None
        except Exception as e:
            logger.error("Error scanning {}: {}", fp, e)
            return None

    results = Parallel(n_jobs=jobs)(delayed(process_one)(Path(f)) for f in files)
    items = [r for r in results if r is not None]

    export_parquet(items, output_path)
    logger.info("scan-batch: {}/{} files → {}", len(items), len(files), output_path)


# ------------------------------------------------------------------
# merge-parquet
# ------------------------------------------------------------------

@main.command("merge-parquet")
@click.argument(
    "parquet_files",
    nargs=-1,
    required=True,
    type=click.Path(exists=True, path_type=Path),
)
@click.option(
    "--output", "db_path",
    required=True,
    type=click.Path(path_type=Path),
    help="Destination catalog.duckdb",
)
@click.option(
    "--config", "config_path",
    default=None,
    type=click.Path(exists=True, path_type=Path),
    help="ESM-Tools finished_config.yaml (needed to recreate collections)",
)
def merge_parquet(parquet_files, db_path, config_path):
    """Merge Parquet staging files from scan-batch into CATALOG.DUCKDB.

    This is the serial final step after parallel SLURM array scans.
    Collections are created from item metadata before items are inserted.
    """
    from esm_catalog.integration.config import load_config
    from esm_catalog.scan.context import resolve_context
    from esm_catalog.storage.duckdb import CatalogDB
    from esm_catalog.storage.export import import_parquet

    config = load_config(config_path) if config_path else None

    with CatalogDB(db_path) as db:
        # Pre-create collections from item data in Parquet files
        import json
        import pyarrow.parquet as pq

        for pq_path in parquet_files:
            table = pq.read_table(str(pq_path), columns=["data"])
            for row in table.column("data"):
                item = json.loads(row.as_py())
                collection_id = item.get("collection")
                if collection_id and not db.collection_exists(collection_id):
                    # Reconstruct context from item properties to create collection
                    props = item.get("properties", {})
                    experiment_id = props.get("experiment", "unknown")
                    component = props.get("component", "unknown")
                    from esm_catalog.scan.context import _make_ctx, _ensure_collection
                    ctx_col = _make_ctx(experiment_id, component)
                    _ensure_collection(ctx_col, db)

        import_parquet(db, list(parquet_files))

    logger.info("Merged {} Parquet files into {}", len(parquet_files), db_path)


# ------------------------------------------------------------------
# serve  (Phase 3 stub)
# ------------------------------------------------------------------

@main.command()
@click.option("--catalog", "catalog_paths", multiple=True, required=True,
              type=click.Path(exists=True, path_type=Path),
              help="Path(s) to catalog.duckdb file(s) to federate")
@click.option("--host", default="0.0.0.0", show_default=True)
@click.option("--port", default=8000, show_default=True)
def serve(catalog_paths, host, port):
    """Launch the STAC API server backed by DuckDB.

    [dim](Phase 3 — requires stac-fastapi to be installed)[/dim]
    """
    try:
        import uvicorn
        from esm_catalog.api.server import build_app

        app = build_app(list(catalog_paths))
        logger.info("Starting STAC API on {}:{}", host, port)
        uvicorn.run(app, host=host, port=port)
    except ImportError as e:
        logger.error(
            "Missing dependency for serve command: {}\n"
            "Install with: pip install 'esm-tools[api]'",
            e,
        )
        sys.exit(1)
