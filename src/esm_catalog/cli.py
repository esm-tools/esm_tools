"""ESM Catalog CLI entry point.

Commands:
    scan          Scan a single file or directory into a catalog
    scan-batch    Scan a list of files and write to Parquet (for SLURM arrays)
    merge-parquet Merge Parquet staging files into a catalog.duckdb
    serve         Launch the STAC API server (Phase 3)

Supports remote filesystems via fsspec/UPath::

    esm-catalog scan ssh://albedo0/work/user/experiment --db catalog.duckdb
    esm-catalog scan scoutfs://albedo1/hpss/archive/exp --db catalog.duckdb
    esm-catalog scan s3://bucket/prefix --db catalog.duckdb
"""

from __future__ import annotations

import queue
import sys
import threading
from pathlib import Path

import rich_click as click
from loguru import logger
from rich.console import Console, Group
from rich.live import Live
from rich.panel import Panel
from rich.progress import Progress, SpinnerColumn, BarColumn, TextColumn, TimeElapsedColumn
from rich.text import Text

click.rich_click.USE_RICH_MARKUP = True
click.rich_click.SHOW_ARGUMENTS = True


def _configure_logging(verbose: bool):
    logger.remove()
    level = "DEBUG" if verbose else "INFO"
    logger.add(sys.stderr, level=level, format="{time:HH:mm:ss} | {level:<8} | {message}")


def _scan_file_worker(fp_str: str) -> tuple[str, str, dict | None]:
    """Scan a single file in a separate process (avoids HDF5 thread-safety issues).

    Must be module-level for ProcessPoolExecutor pickling.
    """
    import time as _t
    t0 = _t.monotonic()

    import os
    import warnings
    from pathlib import Path
    from esm_catalog.scan.detect import UnsupportedFormatError, scan_file

    t_import = _t.monotonic() - t0

    warnings.filterwarnings("ignore", message=".*Ambiguous reference date.*")
    warnings.filterwarnings("ignore", message=".*Unable to decode time axis.*")

    fp = Path(fp_str)
    try:
        # Capture stat in the worker so the main thread doesn't re-stat
        try:
            st = os.stat(fp_str)
            stat_info = {
                "file_size": st.st_size,
                "file_mtime": st.st_mtime,
                "file_atime": st.st_atime,
            }
        except OSError:
            stat_info = {}

        t1 = _t.monotonic()
        metadata = scan_file(fp)
        t_scan = _t.monotonic() - t1

        metadata.update(stat_info)
        metadata["_timing"] = {
            "import_s": round(t_import, 3),
            "scan_s": round(t_scan, 3),
        }
        return (fp_str, "ok", metadata)
    except UnsupportedFormatError:
        return (fp_str, "unsupported", None)
    except Exception as e:
        return (fp_str, "error", str(e))


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
@click.argument("path", type=str)
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
@click.option(
    "--jobs", "-j",
    default=32,
    show_default=True,
    help="Number of parallel workers for file scanning",
)
@click.option(
    "--ssh-connections", "-c",
    default=2,
    show_default=True,
    help="Max concurrent SSH connections (prevents connection exhaustion)",
)
@click.option(
    "--include-extensionless/--no-extensionless",
    default=None,
    help="Include extension-less files (GRIB). Default: yes for local, no for remote.",
)
@click.option(
    "--distributed", "use_dask",
    is_flag=True,
    default=False,
    help="Use dask.distributed LocalCluster. Default: ProcessPoolExecutor.",
)
@click.option(
    "--scheduler", "scheduler_address",
    default=None,
    help="Connect to existing Dask scheduler (e.g. tcp://host:8786). "
         "Implies --distributed. See examples/dask_slurm_scan/ for SLURM setup.",
)
@click.option(
    "--exclude", "exclude_patterns",
    multiple=True,
    default=("/run_", "/input/"),
    show_default=True,
    help="Directory patterns to exclude (can be specified multiple times). "
         "Default excludes run_*/ and input/ directories.",
)
@click.option(
    "--no-exclude",
    is_flag=True,
    default=False,
    help="Disable default exclusions and scan all directories.",
)
@click.option(
    "--no-dask",
    is_flag=True,
    default=False,
    help="Force ProcessPoolExecutor even if dask.distributed is available.",
)
@click.pass_context
def scan(ctx, path, db_path, config_path, jobs, ssh_connections, include_extensionless, use_dask, scheduler_address, exclude_patterns, no_exclude, no_dask):
    """Scan PATH (file or directory) into the catalog at DB.

    PATH can be a local path or a remote URI:
      - /local/path/to/experiment
      - ssh://albedo0/work/user/experiment
      - scoutfs://albedo1/hpss/archive/experiment
      - s3://bucket/prefix

    All supported files (.nc, .grb, etc.) are scanned recursively.
    Files are discovered and scanned in parallel using -j workers.
    """
    from esm_catalog.integration.config import load_config
    from esm_catalog.scan.context import resolve_context, RestartFileError
    from esm_catalog.scan.detect import UnsupportedFormatError, scan_file
    from esm_catalog.scan.upath import (
        parse_uri, walk_files, is_file, is_dir, get_protocol,
    )
    from esm_catalog.stac.extensions.hpc import add_hpc_extension
    from esm_catalog.stac.item import make_item
    from esm_catalog.storage.duckdb import CatalogDB

    import time as _time

    t_total_start = _time.monotonic()

    # Detect non-interactive (SLURM, pipe, redirect) -- skip Rich Live display
    interactive = sys.stderr.isatty()
    if not interactive:
        logger.info("Non-interactive mode (SLURM/pipe): using log-based progress")

    config = load_config(config_path) if config_path else None

    # Parse the path (local or remote URI)
    root = parse_uri(path)
    protocol = get_protocol(path)
    is_remote = protocol != "file"

    # Auto-detect Dask as default backend when available
    if not use_dask and scheduler_address is None and not no_dask:
        try:
            import dask.distributed  # noqa: F401
            use_dask = True
            logger.debug("dask.distributed available, using as default backend")
        except ImportError:
            pass

    backend = "dask" if (use_dask or scheduler_address) else "ProcessPoolExecutor"
    if is_remote:
        logger.info("Scanning remote filesystem: {} (protocol: {})", path, protocol)
        logger.warning("Remote scanning is slow. Consider: rsync first, then scan locally.")
    else:
        logger.info("Scanning local filesystem: {} ({} workers, backend: {})", path, jobs, backend)

    known_exts = {".nc", ".nc4", ".nc3", ".cdf", ".h5", ".hdf5", ".hdf",
                  ".grb", ".grb2", ".grib", ".grib2"}
    skip_exts = {".codes", ".txt", ".log", ".sh", ".py", ".yaml", ".yml", ".json"}

    # Note: scan_one_file is defined at module level (_scan_file_worker) for pickling

    # Determine active exclusion patterns
    active_excludes = [] if no_exclude else list(exclude_patterns)
    if active_excludes:
        logger.debug("Excluding paths matching: {}", active_excludes)

    # Extract directory name fragments for os.walk pruning.
    # e.g. "/restart/" -> "restart", "/run_" -> "run_", "/input/" -> "input"
    exclude_dir_names = [
        pat.strip("/") for pat in active_excludes
    ] if active_excludes else []

    def should_exclude(fp_str: str) -> bool:
        """Check if file path matches any exclusion pattern."""
        for pattern in active_excludes:
            if pattern in fp_str:
                return True
        return False

    def file_candidates(root, include_extensionless=True):
        """Yield file candidates via a single directory walk."""
        count = 0
        for fp, _has_known_ext in walk_files(
            root,
            known_extensions=known_exts,
            skip_extensions=skip_exts,
            exclude_dirs=exclude_dir_names,
            include_extensionless=include_extensionless,
        ):
            count += 1
            if count % 1000 == 0:
                logger.info("  ... discovered {:,} files so far", count)
            yield fp

    t_db_start = _time.monotonic()
    with CatalogDB(db_path) as catalog_db:
        logger.debug("DB opened in {:.2f}s: {}", _time.monotonic() - t_db_start, db_path)
        ok = 0
        errors = 0
        include_ext = True  # default; overridden below for directories

        # Resolve scan roots
        _EXP_MARKER = ".top_of_exp_tree"

        if is_file(root):
            file_source = "single"
            scan_roots = [root]
        elif is_dir(root):
            file_source = "dir"
            if include_extensionless is None:
                include_ext = not is_remote
            else:
                include_ext = include_extensionless
            if not include_ext and is_remote:
                logger.info("Skipping extension-less files for remote scan")

            root_path = Path(str(root))
            if (root_path / _EXP_MARKER).exists():
                scan_roots = [root]
                logger.info("Experiment tree detected: {}", root_path.name)
            else:
                exp_dirs = sorted(
                    d for d in root_path.iterdir()
                    if d.is_dir() and (d / _EXP_MARKER).exists()
                )
                if exp_dirs:
                    scan_roots = [parse_uri(str(d)) for d in exp_dirs]
                    logger.info(
                        "Found {} experiment trees: {}",
                        len(exp_dirs),
                        ", ".join(d.name for d in exp_dirs),
                    )
                else:
                    scan_roots = [root]
                    logger.info("No .top_of_exp_tree markers found, scanning entire directory")
        else:
            logger.error("Path does not exist or is not accessible: {}", path)
            return

        # Batch accumulation for DB writes
        BATCH_SIZE = 500
        _item_batch: list[dict] = []
        _prop_batch: list[tuple[str, dict]] = []
        _seen_collections: set[str] = set()
        _timing_import: list[float] = []
        _timing_scan: list[float] = []
        restart_skipped = 0
        unsupported_skipped = 0
        context_errors = 0

        def _flush_batch():
            """Write accumulated items to DB in a single transaction."""
            if not _item_batch:
                return
            catalog_db.insert_items_batch(_item_batch)
            catalog_db.upsert_collection_item_props_batch(_prop_batch)
            _item_batch.clear()
            _prop_batch.clear()

        def handle_result(fp_str, status, data):
            """Resolve context, build STAC item, accumulate for batch insert.

            Returns (message, rich_style) for the activity panel, or None.
            """
            nonlocal ok, errors, restart_skipped, unsupported_skipped, context_errors
            fp = Path(fp_str)
            fname = fp.name

            if status == "ok":
                # Collect timing diagnostics
                timing = (data or {}).pop("_timing", None)
                if timing:
                    _timing_import.append(timing["import_s"])
                    _timing_scan.append(timing["scan_s"])

                try:
                    ctx_col = resolve_context(fp, config=config, db=catalog_db, allow_restart=True)
                    item = make_item(fp, data, ctx_col, config=config)
                    item = add_hpc_extension(item, fp, metadata=data)
                    _item_batch.append(item)
                    _prop_batch.append((ctx_col.collection_id, item))
                    _seen_collections.add(ctx_col.collection_id)
                    ok += 1
                    if len(_item_batch) >= BATCH_SIZE:
                        _flush_batch()
                    return ("+ " + fname, "green")
                except RestartFileError:
                    restart_skipped += 1
                    return ("- " + fname + " (restart, skipped)", "dim")
                except Exception as e:
                    context_errors += 1
                    errors += 1
                    return ("x " + fname + ": " + str(e), "red")
            elif status == "error":
                errors += 1
                return ("x " + fname + ": " + str(data), "yellow")
            elif status == "unsupported":
                unsupported_skipped += 1
                return ("- " + fname + " (skipped)", "dim")
            return None

        # ==============================================================
        # Path A: Dask distributed (--distributed or --scheduler)
        #
        # Two-phase design:
        #   1. Discover all files up front (spinner with count)
        #   2. Submit to Dask, harvest via as_completed (progress bar
        #      with percentage, ETA, and activity panel)
        # ==============================================================
        if use_dask or scheduler_address:
            from esm_catalog.scan.distributed import get_client, run_scan

            console = Console() if interactive else None

            # Phase 1: discover all files
            t_discovery = _time.monotonic()
            all_files: list[str] = []
            if file_source == "single":
                all_files = [str(scan_roots[0])]
            elif interactive:
                with console.status("[cyan]Discovering files...") as spinner:
                    for scan_root in scan_roots:
                        for fp in file_candidates(
                            scan_root, include_extensionless=include_ext
                        ):
                            all_files.append(str(fp))
                            if len(all_files) % 500 == 0:
                                spinner.update(
                                    f"[cyan]Discovering files... "
                                    f"({len(all_files):,} found)"
                                )
            else:
                for scan_root in scan_roots:
                    for fp in file_candidates(
                        scan_root, include_extensionless=include_ext
                    ):
                        all_files.append(str(fp))
                        if len(all_files) % 5000 == 0:
                            logger.info("  ... discovered {:,} files", len(all_files))
            t_discovery = _time.monotonic() - t_discovery
            logger.info("Discovery: {:,} files in {:.1f}s", len(all_files), t_discovery)

            if not all_files:
                logger.info("No files found to scan")
                return

            # Phase 2: connect to Dask
            t_dask_start = _time.monotonic()
            client, cluster = get_client(
                scheduler=scheduler_address,
                n_workers=jobs,
            )
            logger.info(
                "Dask ready in {:.1f}s ({} workers)",
                _time.monotonic() - t_dask_start,
                len(client.scheduler_info().get("workers", {})),
            )

            # Phase 3: scan files
            t_scan_start = _time.monotonic()
            if interactive:
                logger.disable("esm_catalog")
            try:
                stats = run_scan(
                    client,
                    all_files,
                    _scan_file_worker,
                    handle_result,
                    console=console,
                )
            finally:
                if interactive:
                    logger.enable("esm_catalog")
                # Shut down gracefully to avoid heartbeat errors in logs
                import logging
                logging.getLogger("distributed").setLevel(logging.CRITICAL)
                client.close()
                if cluster is not None:
                    cluster.close()
                logging.getLogger("distributed").setLevel(logging.WARNING)

            t_scan = _time.monotonic() - t_scan_start
            scanned = stats["total"]
            logger.info(
                "Scan phase: {:.1f}s ({:,.0f} files/s)",
                t_scan, scanned / t_scan if t_scan > 0 else 0,
            )

        # ==============================================================
        # Path B: ProcessPoolExecutor (default, no Dask dependency)
        #
        # Streaming design: discovery thread feeds a queue, main thread
        # submits to executor and harvests concurrently.
        # ==============================================================
        else:
            from concurrent.futures import ProcessPoolExecutor
            from collections import deque

            scanned = 0
            discovered = 0
            discovery_done = False

            file_q: queue.Queue = queue.Queue(maxsize=2000)
            _SENTINEL = None

            def _discovery_worker():
                nonlocal discovered
                if file_source == "single":
                    file_q.put(scan_roots[0])
                    discovered = 1
                else:
                    for scan_root in scan_roots:
                        for fp in file_candidates(
                            scan_root, include_extensionless=include_ext
                        ):
                            file_q.put(fp)
                            discovered += 1
                file_q.put(_SENTINEL)

            discovery_thread = threading.Thread(
                target=_discovery_worker, daemon=True
            )

            progress = Progress(
                SpinnerColumn(),
                TextColumn("[progress.description]{task.description}"),
                BarColumn(),
                TextColumn("({task.completed}/{task.total})"),
                TimeElapsedColumn(),
            )

            messages: deque = deque(maxlen=6)

            def make_display():
                lines = [
                    Text(msg, style=s) if s else Text(msg)
                    for msg, s in messages
                ]
                if lines:
                    panel = Panel(
                        Text("\n").join(lines),
                        title="Activity",
                        border_style="dim",
                        height=8,
                    )
                    return Group(progress, panel)
                return progress

            def _drain_queue(max_items=100):
                items = []
                for _ in range(max_items):
                    try:
                        item = file_q.get_nowait()
                    except queue.Empty:
                        break
                    if item is _SENTINEL:
                        return items, True
                    items.append(item)
                return items, False

            t_scan_start = _time.monotonic()
            logger.info("Starting streaming scan ({} workers, ProcessPoolExecutor)...", jobs)
            if interactive:
                logger.disable("esm_catalog")
            discovery_thread.start()

            try:
                if interactive:
                    # Interactive: Rich Live display
                    with Live(make_display(), refresh_per_second=4) as live:
                        task_id = progress.add_task(
                            "[cyan]Discovering & scanning...[/cyan]", total=None
                        )

                        with ProcessPoolExecutor(max_workers=jobs) as executor:
                            pending: dict = {}

                            while not discovery_done or pending:
                                batch, sentinel = _drain_queue(max_items=200)
                                if sentinel:
                                    discovery_done = True
                                for fp in batch:
                                    fut = executor.submit(
                                        _scan_file_worker, str(fp)
                                    )
                                    pending[fut] = None

                                done_futures = [f for f in pending if f.done()]
                                for f in done_futures:
                                    del pending[f]
                                    try:
                                        fp_str, status, data = f.result()
                                    except Exception as e:
                                        errors += 1
                                        scanned += 1
                                        messages.append(
                                            ("x Error: " + str(e), "red")
                                        )
                                    else:
                                        scanned += 1
                                        msg = handle_result(fp_str, status, data)
                                        if msg:
                                            messages.append(msg)

                                desc = (
                                    "[cyan]Scanning...[/cyan]"
                                    if discovery_done
                                    else "[cyan]Discovering & scanning...[/cyan]"
                                )
                                total = discovered if discovery_done else None
                                progress.update(
                                    task_id,
                                    description=desc,
                                    total=total,
                                    completed=scanned,
                                )
                                live.update(make_display())

                                if (
                                    not discovery_done
                                    and not batch
                                    and not done_futures
                                ):
                                    _time.sleep(0.05)

                        progress.update(
                            task_id,
                            description="[green]Done[/green]",
                            total=discovered,
                            completed=scanned,
                        )
                        live.update(make_display())
                else:
                    # Non-interactive (SLURM/pipe): log-based progress
                    LOG_INTERVAL = 30
                    t_last_log = _time.monotonic()

                    with ProcessPoolExecutor(max_workers=jobs) as executor:
                        pending: dict = {}

                        while not discovery_done or pending:
                            batch, sentinel = _drain_queue(max_items=200)
                            if sentinel:
                                discovery_done = True
                            for fp in batch:
                                fut = executor.submit(
                                    _scan_file_worker, str(fp)
                                )
                                pending[fut] = None

                            done_futures = [f for f in pending if f.done()]
                            for f in done_futures:
                                del pending[f]
                                try:
                                    fp_str, status, data = f.result()
                                except Exception as e:
                                    errors += 1
                                    scanned += 1
                                else:
                                    scanned += 1
                                    handle_result(fp_str, status, data)

                            now = _time.monotonic()
                            if now - t_last_log >= LOG_INTERVAL:
                                total_str = f"/{discovered:,}" if discovery_done else "+?"
                                logger.info(
                                    "Progress: {:,}{} scanned | ok={:,} err={:,} | pending={:,}",
                                    scanned, total_str, ok, errors, len(pending),
                                )
                                t_last_log = now

                            if (
                                not discovery_done
                                and not batch
                                and not done_futures
                            ):
                                _time.sleep(0.05)

            finally:
                if interactive:
                    logger.enable("esm_catalog")
                discovery_thread.join(timeout=5)

            t_scan = _time.monotonic() - t_scan_start
            logger.info(
                "Scan phase: {:.1f}s ({:,.0f} files/s)",
                t_scan, scanned / t_scan if t_scan > 0 else 0,
            )

        # Flush remaining items and compute collection extents
        t_flush = _time.monotonic()
        _flush_batch()
        logger.info("Final batch flush: {:.2f}s", _time.monotonic() - t_flush)

        if _seen_collections:
            t_extent = _time.monotonic()
            logger.info("Updating extents for {} collections...", len(_seen_collections))
            catalog_db.update_collection_extents_bulk(_seen_collections)
            logger.info("Extent update: {:.2f}s", _time.monotonic() - t_extent)

    t_total = _time.monotonic() - t_total_start
    logger.info(
        "Done in {:.1f}s: {} scanned, {} cataloged, {} restart-skipped, "
        "{} unsupported, {} errors",
        t_total, scanned, ok, restart_skipped, unsupported_skipped, errors,
    )

    # Worker timing summary
    if _timing_import:
        import_first = max(_timing_import)
        import_avg = sum(_timing_import) / len(_timing_import)
        scan_avg = sum(_timing_scan) / len(_timing_scan)
        scan_min = min(_timing_scan)
        scan_max = max(_timing_scan)
        logger.info(
            "Worker stats ({} samples): import max={:.1f}s avg={:.3f}s | "
            "scan avg={:.3f}s min={:.3f}s max={:.3f}s",
            len(_timing_import), import_first, import_avg,
            scan_avg, scan_min, scan_max,
        )


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
    from esm_catalog.scan.context import resolve_context, RestartFileError
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
        except (UnsupportedFormatError, RestartFileError):
            # Silently skip unsupported formats and restart files
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
@click.option("--catalog", "catalog_paths", multiple=True,
              type=click.Path(exists=False, path_type=Path),
              help="Path(s) to catalog.duckdb file(s) to federate. "
                   "Can also be added dynamically via the /catalogs API.")
@click.option("--registry", "registry_path",
              type=click.Path(path_type=Path),
              default=None,
              help="JSON file to persist dynamic catalog registrations. "
                   "If not provided, catalog changes are in-memory only.")
@click.option("--host", default="0.0.0.0", show_default=True)
@click.option("--port", default=8000, show_default=True)
@click.option("--jupyterhub", is_flag=True, default=False,
              help="Enable JupyterHub authentication. Requires JUPYTERHUB_API_URL "
                   "and JUPYTERHUB_API_TOKEN environment variables.")
@click.option("--write-groups", multiple=True, default=["catalog-managers"],
              help="Groups that can register/modify catalogs (with --jupyterhub)")
def serve(catalog_paths, registry_path, host, port, jupyterhub, write_groups):
    """Launch the STAC API server backed by DuckDB.

    Catalogs can be specified via --catalog flags at startup, or added
    dynamically via the /catalogs REST API. Use --registry to persist
    dynamic catalog registrations across server restarts.

    [dim](Phase 3 - requires stac-fastapi to be installed)[/dim]
    """
    try:
        import uvicorn
        from esm_catalog.api.app import create_app

        # Configure authenticator
        authenticator = None
        if jupyterhub:
            from esm_catalog.api.auth import JupyterHubAuthenticator
            authenticator = JupyterHubAuthenticator(write_groups=list(write_groups))
            logger.info("JupyterHub authentication enabled")
            logger.info("Write access groups: {}", list(write_groups))

        api = create_app(
            catalogs=list(catalog_paths) if catalog_paths else None,
            registry_persist_path=registry_path,
            authenticator=authenticator,
        )
        logger.info("Starting STAC API on {}:{}", host, port)
        if registry_path:
            logger.info("Registry persistence: {}", registry_path)
        if catalog_paths:
            logger.info("Initial catalogs: {}", len(catalog_paths))
        else:
            logger.info("No initial catalogs - add via /catalogs API")
        uvicorn.run(api.app, host=host, port=port)
    except ImportError as e:
        logger.error(
            "Missing dependency for serve command: {}\n"
            "Install with: pip install stac-fastapi-api uvicorn",
            e,
        )
        sys.exit(1)


# ------------------------------------------------------------------
# register (add catalog to running server)
# ------------------------------------------------------------------

@main.command()
@click.argument("db_path", type=click.Path(exists=True, path_type=Path))
@click.option("--server", "-s", default="http://localhost:8000", show_default=True,
              help="URL of the running STAC API server")
@click.option("--name", "-n", default=None,
              help="Human-readable name for the catalog (defaults to filename)")
@click.option("--description", "-d", default=None,
              help="Description of the catalog")
def register(db_path, server, name, description):
    """Register a catalog with a running STAC API server.

    If the catalog is already registered, it will be refreshed to pick up
    any changes made since the last registration.

    Example:
        esm-catalog register ~/exp/catalog.db --server http://localhost:8000
    """
    import httpx

    db_path = db_path.resolve()
    url = f"{server.rstrip('/')}/catalogs"

    payload = {"path": str(db_path)}
    if name:
        payload["name"] = name
    if description:
        payload["description"] = description

    try:
        # Try to register
        resp = httpx.post(url, json=payload, timeout=30)

        if resp.status_code == 201:
            info = resp.json()
            logger.info("Registered catalog: {} (id: {})", info.get("name", db_path.name), info["id"])
        elif resp.status_code == 409:
            # Already exists - find it and refresh
            logger.info("Catalog already registered, refreshing...")
            list_resp = httpx.get(url, timeout=30)
            if list_resp.status_code == 200:
                catalogs = list_resp.json().get("catalogs", [])
                for cat in catalogs:
                    if cat["path"] == str(db_path):
                        refresh_url = f"{url}/{cat['id']}/refresh"
                        refresh_resp = httpx.post(refresh_url, timeout=30)
                        if refresh_resp.status_code == 200:
                            logger.info("Refreshed catalog: {} (id: {})", cat.get("name"), cat["id"])
                        else:
                            logger.error("Failed to refresh: {}", refresh_resp.text)
                        return
            logger.error("Could not find catalog to refresh")
        else:
            logger.error("Failed to register: {} {}", resp.status_code, resp.text)
            sys.exit(1)

    except httpx.ConnectError:
        logger.error("Could not connect to server: {}", server)
        sys.exit(1)
    except Exception as e:
        logger.error("Error: {}", e)
        sys.exit(1)


# ------------------------------------------------------------------
# refresh (refresh catalog on running server)
# ------------------------------------------------------------------

@main.command()
@click.argument("catalog_id", required=False)
@click.option("--server", "-s", default="http://localhost:8000", show_default=True,
              help="URL of the running STAC API server")
@click.option("--db", "db_path", type=click.Path(path_type=Path), default=None,
              help="Find catalog by path instead of ID")
@click.option("--all", "refresh_all", is_flag=True, default=False,
              help="Refresh all registered catalogs")
def refresh(catalog_id, server, db_path, refresh_all):
    """Refresh a catalog on a running STAC API server.

    Use this after scanning new files into a catalog to make them
    visible via the API.

    Examples:
        esm-catalog refresh abc123 --server http://localhost:8000
        esm-catalog refresh --db ~/exp/catalog.db
        esm-catalog refresh --all
    """
    import httpx

    base_url = f"{server.rstrip('/')}/catalogs"

    try:
        if refresh_all:
            # Get all catalogs and refresh each
            resp = httpx.get(base_url, timeout=30)
            if resp.status_code != 200:
                logger.error("Failed to list catalogs: {}", resp.text)
                sys.exit(1)

            catalogs = resp.json().get("catalogs", [])
            for cat in catalogs:
                refresh_resp = httpx.post(f"{base_url}/{cat['id']}/refresh", timeout=30)
                if refresh_resp.status_code == 200:
                    logger.info("Refreshed: {} ({})", cat.get("name"), cat["id"])
                else:
                    logger.warning("Failed to refresh {}: {}", cat["id"], refresh_resp.text)
            return

        # Find catalog by path if needed
        if db_path and not catalog_id:
            db_path = db_path.resolve()
            resp = httpx.get(base_url, timeout=30)
            if resp.status_code == 200:
                catalogs = resp.json().get("catalogs", [])
                for cat in catalogs:
                    if cat["path"] == str(db_path):
                        catalog_id = cat["id"]
                        break
            if not catalog_id:
                logger.error("No catalog found with path: {}", db_path)
                sys.exit(1)

        if not catalog_id:
            logger.error("Specify a catalog ID, --db path, or --all")
            sys.exit(1)

        # Refresh the catalog
        resp = httpx.post(f"{base_url}/{catalog_id}/refresh", timeout=30)
        if resp.status_code == 200:
            info = resp.json()
            logger.info("Refreshed: {} - {}", info["id"], info["message"])
        elif resp.status_code == 404:
            logger.error("Catalog not found: {}", catalog_id)
            sys.exit(1)
        else:
            logger.error("Failed: {}", resp.text)
            sys.exit(1)

    except httpx.ConnectError:
        logger.error("Could not connect to server: {}", server)
        sys.exit(1)


# ------------------------------------------------------------------
# catalogs (list catalogs on running server)
# ------------------------------------------------------------------

@main.command()
@click.option("--server", "-s", default="http://localhost:8000", show_default=True,
              help="URL of the running STAC API server")
@click.option("--json", "as_json", is_flag=True, default=False,
              help="Output as JSON")
def catalogs(server, as_json):
    """List catalogs registered on a running STAC API server.

    Example:
        esm-catalog catalogs --server http://localhost:8000
    """
    import httpx
    import json

    url = f"{server.rstrip('/')}/catalogs"

    try:
        resp = httpx.get(url, timeout=30)
        if resp.status_code != 200:
            logger.error("Failed: {} {}", resp.status_code, resp.text)
            sys.exit(1)

        data = resp.json()

        if as_json:
            print(json.dumps(data, indent=2))
        else:
            catalogs_list = data.get("catalogs", [])
            if not catalogs_list:
                logger.info("No catalogs registered")
            else:
                logger.info("Registered catalogs ({}):", len(catalogs_list))
                for cat in catalogs_list:
                    status = "[green]online[/green]" if cat.get("status") == "online" else "[red]offline[/red]"
                    Console().print(f"  {cat['id']}  {cat.get('name', '(unnamed)')}  {status}")
                    Console().print(f"    [dim]{cat['path']}[/dim]")

    except httpx.ConnectError:
        logger.error("Could not connect to server: {}", server)
        sys.exit(1)


if __name__ == "__main__":
    main()
