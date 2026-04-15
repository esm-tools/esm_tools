"""ESM Catalog CLI entry point.

Commands:
    scan          Scan a single file or directory into a catalog
    scan-batch    Scan a list of files and write to Parquet (for SLURM arrays)
    merge-parquet Merge Parquet staging files into a catalog.duckdb
    serve         Launch the STAC API server (Phase 3)
    mcp           Start the MCP server for LLM tool access to the catalog

Supports remote filesystems via fsspec/UPath::

    esm-catalog scan ssh://albedo0/work/user/experiment --db catalog.duckdb
    esm-catalog scan scoutfs://albedo1/hpss/archive/exp --db catalog.duckdb
    esm-catalog scan s3://bucket/prefix --db catalog.duckdb
"""

from __future__ import annotations

import sys
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
    import warnings
    from pathlib import Path
    from esm_catalog.scan.detect import UnsupportedFormatError, scan_file

    # Suppress xarray warnings about ambiguous date strings (e.g., "1-1-1" vs "0001-1-1")
    # These are common in climate model output and harmless
    warnings.filterwarnings("ignore", message=".*Ambiguous reference date.*")
    warnings.filterwarnings("ignore", message=".*Unable to decode time axis.*")

    fp = Path(fp_str)
    try:
        metadata = scan_file(fp)
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
    default=4,
    show_default=True,
    help="Number of parallel workers for processing results",
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
    help="Use dask.distributed (heavier, but scalable to SLURM). Default: ProcessPoolExecutor.",
)
@click.option(
    "--exclude", "exclude_patterns",
    multiple=True,
    default=("/run_", "/input/", "/unknown/", "/restart/"),
    show_default=True,
    help="Directory patterns to exclude (can be specified multiple times). "
         "Default excludes run_*/, input/, unknown/, and restart/ directories.",
)
@click.option(
    "--no-exclude",
    is_flag=True,
    default=False,
    help="Disable default exclusions and scan all directories.",
)
@click.pass_context
def scan(ctx, path, db_path, config_path, jobs, ssh_connections, include_extensionless, use_dask, exclude_patterns, no_exclude):
    """Scan PATH (file or directory) into the catalog at DB.

    PATH can be a local path or a remote URI:
      - /local/path/to/experiment
      - ssh://albedo0/work/user/experiment
      - scoutfs://albedo1/hpss/archive/experiment
      - s3://bucket/prefix

    All supported files (.nc, .grb, etc.) are scanned recursively.
    Files are discovered and scanned in parallel using -j workers.
    """
    from concurrent.futures import ProcessPoolExecutor, as_completed as futures_as_completed

    from esm_catalog.integration.config import load_config
    from esm_catalog.scan.context import resolve_context, RestartFileError
    from esm_catalog.scan.detect import UnsupportedFormatError, scan_file
    from esm_catalog.scan.upath import (
        parse_uri, list_files, list_all_files, is_file, is_dir, get_protocol,
    )
    from esm_catalog.stac.extensions.hpc import add_hpc_extension
    from esm_catalog.stac.item import make_item
    from esm_catalog.storage.duckdb import CatalogDB

    config = load_config(config_path) if config_path else None

    # Parse the path (local or remote URI)
    root = parse_uri(path)
    protocol = get_protocol(path)
    is_remote = protocol != "file"

    if is_remote:
        logger.info("Scanning remote filesystem: {} (protocol: {})", path, protocol)
        logger.warning("Remote scanning is slow. Consider: rsync first, then scan locally.")
    else:
        logger.info("Scanning local filesystem: {} ({} workers)", path, jobs)

    known_exts = {".nc", ".nc4", ".nc3", ".cdf", ".h5", ".hdf5", ".hdf",
                  ".grb", ".grb2", ".grib", ".grib2"}
    skip_exts = {".codes", ".txt", ".log", ".sh", ".py", ".yaml", ".yml", ".json"}

    # Note: scan_one_file is defined at module level (_scan_file_worker) for pickling

    # Determine active exclusion patterns
    active_excludes = [] if no_exclude else list(exclude_patterns)
    if active_excludes:
        logger.debug("Excluding paths matching: {}", active_excludes)

    def should_exclude(fp_str: str) -> bool:
        """Check if file path matches any exclusion pattern."""
        for pattern in active_excludes:
            if pattern in fp_str:
                return True
        return False

    def file_candidates(root, include_extensionless=True):
        """Yield file candidates: known extensions first, then extension-less files."""
        seen = set()

        # First: files with known extensions (fast path)
        for fp in list_files(root):
            key = str(fp)
            if key not in seen:
                seen.add(key)
                if not should_exclude(key):
                    yield fp

        # Second: extension-less files (magic byte detection happens in worker)
        # Skip for remote scans - GRIB (most extension-less files) can't be scanned remotely anyway
        if include_extensionless:
            for fp in list_all_files(root, skip_extensions=skip_exts):
                key = str(fp)
                if key not in seen and fp.suffix.lower() not in known_exts:
                    seen.add(key)
                    if not should_exclude(key):
                        yield fp

    with CatalogDB(db_path) as catalog_db:
        ok = 0
        errors = 0
        scanned = 0

        # Phase 1: Discover all files
        logger.info("Discovering files...")
        files = []
        if is_file(root):
            files = [root]
        elif is_dir(root):
            # Determine whether to include extension-less files
            if include_extensionless is None:
                include_ext = not is_remote
            else:
                include_ext = include_extensionless

            if not include_ext and is_remote:
                logger.info("Skipping extension-less files for remote scan")

            files = list(file_candidates(root, include_extensionless=include_ext))
        else:
            logger.error("Path does not exist or is not accessible: {}", path)
            return

        total_files = len(files)
        logger.info("Found {} files to scan", total_files)

        if total_files == 0:
            logger.info("No files to scan")
            return

        # Phase 2: Scan files in parallel
        # Default: ProcessPoolExecutor (lightweight, works on login nodes)
        # --distributed: dask.distributed (heavier, but scalable to SLURM)
        progress = Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            BarColumn(),
            TextColumn("[progress.percentage]{task.percentage:>3.0f}%"),
            TextColumn("({task.completed}/{task.total})"),
            TimeElapsedColumn(),
        )

        from collections import deque
        messages = deque(maxlen=6)

        def make_display():
            msg_lines = [Text(msg, style=style) if style else Text(msg) for msg, style in messages]
            if msg_lines:
                msg_text = Text("\n").join(msg_lines)
                msg_panel = Panel(msg_text, title="Activity", border_style="dim", height=8)
                return Group(progress, msg_panel)
            return progress

        def process_scan_result(fp_str, status, data):
            """Process a single scan result and insert into database."""
            nonlocal ok, errors, scanned
            fp = Path(fp_str)
            fname = fp.name
            scanned += 1

            if status == "ok":
                try:
                    ctx_col = resolve_context(fp, config=config, db=catalog_db)
                    item = make_item(fp, data, ctx_col, config=config)
                    item = add_hpc_extension(item, fp)
                    catalog_db.insert_item(item)
                    catalog_db.update_collection_extent(ctx_col.collection_id, item)
                    catalog_db.upsert_collection_item_props(ctx_col.collection_id, item)
                    ok += 1
                    messages.append((f"✓ {fname}", "green"))
                except RestartFileError:
                    # Restart files are silently skipped (not counted as errors)
                    messages.append((f"- {fname} (restart, skipped)", "dim"))
                except Exception as e:
                    errors += 1
                    messages.append((f"✗ {fname}: {e}", "red"))
            elif status == "error":
                errors += 1
                messages.append((f"✗ {fname}: {data}", "yellow"))
            elif status == "unsupported":
                messages.append((f"- {fname} (skipped)", "dim"))

        logger.disable("esm_catalog")

        try:
            with Live(make_display(), refresh_per_second=4) as live:
                task_id = progress.add_task("[cyan]Scanning...[/cyan]", total=total_files)

                if use_dask:
                    # dask.distributed - heavier but scalable to SLURM
                    from dask.distributed import Client, LocalCluster, as_completed as dask_as_completed

                    with LocalCluster(n_workers=jobs, processes=True, threads_per_worker=1) as cluster:
                        with Client(cluster) as client:
                            futures = client.map(_scan_file_worker, [str(fp) for fp in files])

                            for future in dask_as_completed(futures):
                                try:
                                    fp_str, status, data = future.result()
                                except Exception as e:
                                    errors += 1
                                    scanned += 1
                                    messages.append((f"✗ Error: {e}", "red"))
                                else:
                                    process_scan_result(fp_str, status, data)

                                progress.update(task_id, completed=scanned)
                                live.update(make_display())
                elif jobs == 1:
                    # Serial mode: run in the main process, no fork needed.
                    # Use this when the node is low on memory (os.fork fails).
                    for fp in files:
                        try:
                            fp_str, status, data = _scan_file_worker(str(fp))
                        except Exception as e:
                            errors += 1
                            scanned += 1
                            messages.append((f"✗ Error: {e}", "red"))
                        else:
                            process_scan_result(fp_str, status, data)

                        progress.update(task_id, completed=scanned)
                        live.update(make_display())
                else:
                    # ProcessPoolExecutor - lightweight, works on login nodes
                    with ProcessPoolExecutor(max_workers=jobs) as executor:
                        future_to_path = {
                            executor.submit(_scan_file_worker, str(fp)): fp
                            for fp in files
                        }

                        for future in futures_as_completed(future_to_path):
                            try:
                                fp_str, status, data = future.result()
                            except Exception as e:
                                errors += 1
                                scanned += 1
                                messages.append((f"✗ Error: {e}", "red"))
                            else:
                                process_scan_result(fp_str, status, data)

                            progress.update(task_id, completed=scanned)
                            live.update(make_display())

                progress.update(task_id, description="[green]Done[/green]")
                live.update(make_display())
        finally:
            logger.enable("esm_catalog")

    logger.info("Scanned {} files: {} cataloged, {} errors", scanned, ok, errors)


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
# mcp  (Phase 7 — LLM assistant)
# ------------------------------------------------------------------

@main.command()
@click.option(
    "--catalog-url",
    default="http://localhost:23100",
    show_default=True,
    help="STAC API URL to connect to",
)
@click.option(
    "--transport",
    type=click.Choice(["stdio", "sse", "streamable-http", "openapi"]),
    default="stdio",
    show_default=True,
    help=(
        "MCP transport: stdio (process-based), "
        "streamable-http (Open WebUI 'MCP Streamable HTTP' — endpoint at /mcp), "
        "sse (HTTP MCP clients), or openapi (Open WebUI Tool Servers)"
    ),
)
@click.option(
    "--port",
    default=8001,
    show_default=True,
    help="Port for sse/streamable-http/openapi transport (ignored when transport=stdio)",
)
@click.option(
    "--viz-url",
    default=None,
    show_default=True,
    help="ESM Visualization Service URL (enables preview_item tool, e.g. http://localhost:23001)",
)
def mcp(catalog_url, transport, port, viz_url):
    """Start the MCP server for LLM tool access to the catalog.

    Exposes four tools to connected LLMs:
      list_collections, get_collection_info, search_items, run_python

    [dim]Requires: pip install 'esm-catalog[mcp]'[/dim]
    """
    try:
        from esm_catalog.mcp.server import run as mcp_run

        logger.info("Starting MCP server (transport={}, catalog={}, viz={})", transport, catalog_url, viz_url or "disabled")
        mcp_run(catalog_url=catalog_url, transport=transport, port=port, viz_url=viz_url)
    except ImportError as e:
        logger.error(
            "Missing dependency for mcp command: {}\n"
            "Install with: pip install 'esm-catalog[mcp]'",
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
