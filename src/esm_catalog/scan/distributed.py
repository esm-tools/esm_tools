"""Distributed file scanning with Dask.

Provides a two-phase scan pipeline with proper progress tracking:

1. **Discovery**: Walk filesystem, collect file paths (Rich spinner with count)
2. **Scan**: Submit tasks via ``client.map()``, harvest via ``as_completed()``
   with a Rich progress bar showing completion count, percentage, ETA.

Scheduler modes::

    # LocalCluster (default with --distributed)
    esm-catalog scan /data --db cat.db --distributed -j 8

    # Connect to existing scheduler (started separately, e.g. on SLURM)
    esm-catalog scan /data --db cat.db --scheduler tcp://login-node:8786

For SLURM, see ``examples/dask_slurm_scan/`` for how to start a scheduler
and workers via ``sbatch``, then connect with ``--scheduler``.
"""

from __future__ import annotations

from collections import deque
from typing import Callable

from loguru import logger
from rich.console import Console, Group
from rich.live import Live
from rich.panel import Panel
from rich.progress import (
    BarColumn,
    MofNCompleteColumn,
    Progress,
    SpinnerColumn,
    TextColumn,
    TimeElapsedColumn,
    TimeRemainingColumn,
)
from rich.text import Text


def get_client(
    scheduler: str | None = None,
    n_workers: int = 4,
    threads_per_worker: int = 1,
):
    """Create or connect to a Dask distributed client.

    Args:
        scheduler: Address of existing scheduler (e.g. ``tcp://host:8786``).
                   If None, creates a LocalCluster.
        n_workers: Number of workers for LocalCluster (ignored when
                   connecting to an existing scheduler).
        threads_per_worker: Threads per worker for LocalCluster.

    Returns:
        ``(client, cluster)`` tuple. *cluster* is None when connecting
        to an existing scheduler (caller does not own it).
    """
    from dask.distributed import Client, LocalCluster

    if scheduler:
        logger.info("Connecting to Dask scheduler: {}", scheduler)
        client = Client(scheduler)
        info = client.scheduler_info()
        n = len(info.get("workers", {}))
        logger.info(
            "Connected to {} ({} workers, dashboard: {})",
            scheduler,
            n,
            client.dashboard_link,
        )
        return client, None

    logger.info(
        "Creating LocalCluster ({} workers, {} threads/worker)",
        n_workers,
        threads_per_worker,
    )
    cluster = LocalCluster(
        n_workers=n_workers,
        processes=True,
        threads_per_worker=threads_per_worker,
    )
    client = Client(cluster)
    logger.info("LocalCluster dashboard: {}", client.dashboard_link)
    return client, cluster


def run_scan(
    client,
    file_paths: list[str],
    scan_fn: Callable,
    result_handler: Callable,
    console: Console | None = None,
) -> dict:
    """Submit scan tasks to Dask and process results with a Rich progress bar.

    Two-phase design: all file paths are submitted at once via
    ``client.map()``, then results are harvested in completion order
    via ``as_completed()`` with a live Rich progress display.

    Args:
        client: ``dask.distributed.Client`` instance.
        file_paths: Absolute file path strings to scan.
        scan_fn: Module-level worker function. Signature:
                 ``(file_path_str) -> (path_str, status, data)``.
                 Must be importable (not a closure) for Dask serialization.
        result_handler: Called for each completed result. Signature:
                        ``(path_str, status, data) -> (message, style) | None``.
                        Return a ``(text, rich_style)`` tuple to show in the
                        activity panel, or None for no message.
        console: Optional Rich Console (default: new Console).

    Returns:
        Dict with ``{"ok": int, "errors": int, "skipped": int, "total": int}``.
    """
    import time as _t
    from dask.distributed import as_completed

    interactive = console is not None
    stats = {"ok": 0, "errors": 0, "skipped": 0, "total": len(file_paths)}

    if not file_paths:
        return stats

    # Submit all tasks up front -- Dask handles backpressure internally
    futures = client.map(scan_fn, file_paths, pure=False)
    logger.info(
        "Submitted {} scan tasks to {} workers",
        len(futures),
        len(client.scheduler_info().get("workers", {})),
    )

    def _process_future(future):
        """Process one completed future, update stats."""
        try:
            result = future.result()
            fp_str, status, data = result
            msg = result_handler(fp_str, status, data)
            if status == "ok":
                stats["ok"] += 1
            elif status == "error":
                stats["errors"] += 1
            else:
                stats["skipped"] += 1
            return msg
        except Exception as exc:
            stats["errors"] += 1
            return (f"x Worker crash: {exc}", "red")

    completed = 0
    total = len(file_paths)
    ac = as_completed(futures, raise_errors=False)

    if interactive:
        # Interactive: Rich progress bar with activity panel
        if console is None:
            console = Console()

        progress = Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            BarColumn(bar_width=40),
            MofNCompleteColumn(),
            TextColumn("({task.percentage:>3.0f}%)"),
            TimeElapsedColumn(),
            TextColumn("eta"),
            TimeRemainingColumn(),
            console=console,
        )

        messages: deque[tuple[str, str]] = deque(maxlen=8)

        def _make_display():
            parts = [progress]
            if messages:
                msg_lines = [
                    Text(msg, style=style) for msg, style in messages
                ]
                panel = Panel(
                    Text("\n").join(msg_lines),
                    title="Activity",
                    border_style="dim",
                    height=10,
                )
                parts.append(panel)
            return Group(*parts)

        task_id = progress.add_task("Scanning files", total=total)

        with Live(_make_display(), console=console, refresh_per_second=8) as live:
            for batch in ac.batches():
                for future in batch:
                    msg = _process_future(future)
                    if msg:
                        messages.append(msg)
                    progress.advance(task_id)
                    completed += 1
                live.update(_make_display())

            progress.update(task_id, description="[green]Scan complete[/green]")
            live.update(_make_display())
    else:
        # Non-interactive (SLURM/pipe): log-based progress
        t_last_log = _t.monotonic()
        LOG_INTERVAL = 30  # seconds between progress log lines

        for batch in ac.batches():
            for future in batch:
                _process_future(future)
                completed += 1

            now = _t.monotonic()
            if now - t_last_log >= LOG_INTERVAL:
                pct = 100.0 * completed / total if total else 0
                logger.info(
                    "Progress: {:,}/{:,} ({:.0f}%) | ok={:,} err={:,} skip={:,}",
                    completed, total, pct,
                    stats["ok"], stats["errors"], stats["skipped"],
                )
                t_last_log = now

        logger.info(
            "Scan complete: {:,}/{:,} | ok={:,} err={:,} skip={:,}",
            completed, total, stats["ok"], stats["errors"], stats["skipped"],
        )

    return stats
