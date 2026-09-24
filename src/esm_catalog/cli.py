"""esm-catalog command-line interface.

Workflow for one experiment::

    esm-catalog auth login https://stac.awi.de   # once; token cached locally
    esm-catalog init  <exp_root>                 # set up <exp_root>/catalog/
    esm-catalog scan                             # write stac-geoparquet shards
    esm-catalog push                             # ship new shards -> pgstac

On disk, ``<exp_root>/catalog/`` holds the catalog PFS-friendly: one
``collection.json`` plus sharded stac-geoparquet (a handful of files, never one
JSON per item), and an ``esm-catalog.json`` workspace-state file (experiment id,
server, and which files have been scanned). ``push`` bulk-loads new shards into
the server's pgstac, which stac-fastapi-pgstac then serves to the web viewer.
"""

from __future__ import annotations

from contextlib import contextmanager
from pathlib import Path
from typing import Generator, Optional

import rich_click as click

from esm_catalog import __version__


def _not_implemented(command: str) -> None:
    """Fail cleanly: *command* is scaffolded but its logic is not written yet."""
    raise click.ClickException(f"'{command}' is not implemented yet.")


def _quiet_worker_logging(level: int) -> None:
    """Set noisy third-party loggers to *level* in a worker process.

    Runs as the scan's ``ProcessPoolExecutor`` initializer: a spawned worker
    starts with fresh logging, so the parent's quieting does not carry over and
    each worker would otherwise re-emit paramiko's connection banners.
    """
    import logging

    for noisy in ("paramiko", "stac_geoparquet"):
        logging.getLogger(noisy).setLevel(level)


@contextmanager
def _scan_progress(enabled: bool) -> Generator[Optional[object], None, None]:
    """A transient rich spinner+bar over a scan; yields an ``on_progress`` callback.

    Yields ``None`` (a no-op) when *enabled* is false — not a TTY, or the caller
    asked for verbose logs instead. The whole display is transient: it vanishes
    on exit, leaving only the final summary line (which goes to stdout, while the
    progress renders on stderr so a piped stdout stays clean).
    """
    if not enabled:
        yield None
        return

    from rich.console import Console
    from rich.progress import (
        BarColumn,
        MofNCompleteColumn,
        Progress,
        SpinnerColumn,
        TaskID,
        TextColumn,
        TimeElapsedColumn,
    )

    progress = Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        BarColumn(),
        MofNCompleteColumn(),
        TimeElapsedColumn(),
        console=Console(stderr=True),
        transient=True,
    )
    tasks: dict[str, TaskID] = {}

    def enter_phase(name: str, after: Optional[str], **task_fields) -> TaskID:
        """Return phase *name*'s task, creating it once and hiding *after*'s first.

        The phases run in sequence (sourcing → reading → writing); each hands off
        by hiding the previous phase's bar as its own appears.
        """
        if name not in tasks:
            if after and after in tasks:
                progress.update(tasks[after], visible=False)
            tasks[name] = progress.add_task(**task_fields)
        return tasks[name]

    def on_progress(event) -> None:
        if event.phase == "sourcing":
            task = enter_phase(
                "sourcing", None, description="sourcing experiment…", total=None
            )
            if event.detail:
                progress.update(task, description=f"walking outdata — {event.detail}")
        elif event.phase == "reading":
            task = enter_phase(
                "reading", "sourcing", description="reading", total=event.total
            )
            progress.update(
                task,
                completed=event.current,
                description=f"reading {event.detail}" if event.detail else "reading",
            )
        elif event.phase == "writing":
            enter_phase("writing", "reading", description="writing catalog…", total=None)

    with progress:
        yield on_progress


@click.group()
@click.version_option(version=__version__, prog_name="esm-catalog")
def main() -> None:
    """ESM-Tools simulation catalog."""


@main.group()
def auth() -> None:
    """Authenticate against a STAC server (token cached locally)."""


@auth.command("login")
@click.argument("server_url")
def auth_login(server_url: str) -> None:
    """Log in to SERVER_URL and cache a token for later push."""
    _not_implemented("auth login")


@auth.command("logout")
def auth_logout() -> None:
    """Discard the cached token."""
    _not_implemented("auth logout")


@main.command()
@click.argument("exp_root", type=click.Path(file_okay=False, path_type=Path))
@click.option("--server", help="Target STAC server, recorded for later push.")
def init(exp_root: Path, server: Optional[str]) -> None:
    """Set up <EXP_ROOT>/catalog/ (collection.json + workspace state)."""
    _not_implemented("init")


@main.command()
@click.option(
    "--exp-root",
    default=".",
    help="Experiment root; may be remote (e.g. sftp://host/path). Defaults to '.'.",
)
@click.option(
    "--catalog-dir",
    default=None,
    help="Where to write the catalog; defaults to <exp-root>/catalog. Point it "
    "local (or s3://…) to scan a remote experiment without writing back.",
)
@click.option(
    "--distributed",
    is_flag=True,
    help="Scan on a Dask cluster instead of a local process pool.",
)
@click.option("--scheduler", default=None, help="Attach to a Dask scheduler (tcp://…).")
@click.option(
    "-j", "--jobs", type=int, default=None, help="Number of parallel workers."
)
@click.option("--strict", is_flag=True, help="Exit non-zero if any file fails to scan.")
@click.option(
    "-v",
    "--verbose",
    is_flag=True,
    help="Show connection and library logs instead of the progress display.",
)
def scan(
    exp_root: str,
    catalog_dir: Optional[str],
    distributed: bool,
    scheduler: Optional[str],
    jobs: Optional[int],
    strict: bool,
    verbose: bool,
) -> None:
    """Scan the experiment's output into the catalog (stac-geoparquet shards)."""
    import logging
    import sys

    from upath import UPath

    from esm_catalog.scan.ingest import ScanError, scan_experiment
    from esm_catalog.scan.sourcing import SourcingError

    # Quiet noisy third-party INFO/CPU/RSS chatter (paramiko connection banners,
    # stac_geoparquet telemetry) unless --verbose. CLI-only: library importers
    # keep their own logging config.
    level = logging.INFO if verbose else logging.WARNING
    for noisy in ("paramiko", "stac_geoparquet"):
        logging.getLogger(noisy).setLevel(level)

    show_progress = sys.stderr.isatty() and not verbose
    try:
        with _scan_progress(show_progress) as on_progress:
            report = scan_experiment(
                UPath(exp_root),
                catalog=UPath(catalog_dir) if catalog_dir else None,
                distributed=distributed,
                scheduler=scheduler,
                jobs=jobs,
                strict=strict,
                on_progress=on_progress,
                worker_initializer=_quiet_worker_logging,
                worker_initargs=(level,),
            )
    except (SourcingError, ScanError) as exc:
        raise click.ClickException(str(exc)) from exc
    click.echo(
        f"scanned {report.scanned}, catalogued {report.items}, "
        f"skipped {report.skipped}, unsupported {report.unsupported}, "
        f"failed {len(report.failures)}"
    )


@main.command()
def push() -> None:
    """Ship not-yet-pushed shards to the server's pgstac."""
    _not_implemented("push")


@main.command()
@click.argument("file", type=click.Path(path_type=Path))
def add(file: Path) -> None:
    """Add one file's Item to the current shard."""
    _not_implemented("add")


@main.command()
@click.argument("file", type=click.Path(path_type=Path))
def rm(file: Path) -> None:
    """Remove one file's Item from the catalog."""
    _not_implemented("rm")


@main.command()
@click.argument("target")
def edit(target: str) -> None:
    """Edit a Collection's or Item's metadata (TARGET)."""
    _not_implemented("edit")


if __name__ == "__main__":
    main()
