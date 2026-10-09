"""esm-catalog command-line interface.

Workflow for one experiment::

    esm-catalog auth login https://stac.awi.de   # once; token cached locally
    esm-catalog scan                             # write stac-geoparquet shards
    esm-catalog validate-cmip6                   # check any declared cmip6:* facets, if present
    esm-catalog server put                       # ship new shards -> pgstac
    esm-catalog status                           # what's local, what's configured

A large scan is distributed across SLURM instead::

    esm-catalog distributed render-scripts VARS_FILE   # sched/worker/driver/cleanup .sbatch
    esm-catalog scan --distributed --scheduler tcp://...  # what the driver script runs

One-off manual edits outside a full scan -- e.g. registering a file scan
never saw, or dropping a bad asset -- each write exactly one local shard
(same 'server put' afterwards to ship it), never touch the network themselves::

    esm-catalog asset add <component>-<stream> FILE...        # cache-shortcut when possible
    esm-catalog validate <component>-<stream> FILE            # always a real, authoritative read
    esm-catalog asset rm <component>-<stream> ASSET_KEY       # tombstone; push then drops it
    esm-catalog asset add-alternate <component>-<stream> ASSET_KEY NAME LOCATION  # e.g. a tape copy
    esm-catalog asset set-main <component>-<stream> ASSET_KEY --to NAME    # promote an alternate

On disk, ``<exp_root>/catalog/`` holds the catalog PFS-friendly: one
``collection.json`` plus sharded stac-geoparquet (a handful of files, never one
JSON per item), and an ``esm-catalog.json`` workspace-state file (experiment id
and which files have been scanned, for incremental re-scans). ``push`` bulk-loads
new shards into the server's pgstac, which stac-fastapi-pgstac then serves to the
web viewer.

Configuration (identity-provider settings, a default ``server_url``) comes
from environment variables prefixed ``ESM_CATALOG_`` or from a config file —
run ``esm-catalog status`` to see what is currently resolved and where the
config file would live.
"""

from __future__ import annotations

import json
import sys
from contextlib import contextmanager
from pathlib import Path
from typing import Generator, NoReturn, Optional

import questionary
import rich_click as click

from esm_catalog import __version__

_CONFIG_EPILOG = (
    "Configuration (identity-provider settings, a default server_url) comes "
    "from environment variables prefixed 'ESM_CATALOG_' (e.g. "
    "ESM_CATALOG_SERVER_URL) or a config file — run 'esm-catalog status' to "
    "see what is currently resolved and where the config file would live."
)

click.rich_click.COMMAND_GROUPS["esm-catalog"] = [
    {
        "name": "Local",
        "commands": ["scan", "status", "asset"],
        "panel_styles": {"border_style": "cyan"},
    },
    {
        "name": "Auth",
        "commands": ["auth"],
        "panel_styles": {"border_style": "magenta"},
    },
    {
        "name": "Server",
        "commands": ["server"],
        "panel_styles": {"border_style": "green"},
    },
]  # fmt: skip

click.rich_click.OPTION_GROUPS["esm-catalog distributed render-scripts"] = [
    {
        "name": "Experiment identity",
        "options": [
            "--job-prefix", "--scratch-dir", "--image-tag", "--exp-root", "--catalog-dir",
        ],
    },
    {
        "name": "Worker mode",
        "options": [
            "--worker-mode", "--n-workers", "--throttle", "--n-nodes", "--cores-per-node",
        ],
    },
    {
        "name": "SLURM",
        "options": ["--partition", "--qos", "--account", "--walltime"],
    },
    {
        "name": "Container",
        "options": ["--bind-path", "--container-bin", "--container-module"],
    },
    {
        "name": "Output",
        "options": [
            "--push-after-scan", "--server-url", "--log-dir", "--out-dir",
            "--dump-vars-template",
        ],
    },
]  # fmt: skip


def _configure_logging(verbose: bool) -> int:
    """Set up logging for a CLI command; return the stdlib level used.

    Quiets noisy third-party stdlib loggers (paramiko, stac_geoparquet) and caps
    esm_catalog's own loguru sink at the same level, so a command's progress
    display is the only thing shown by default — internal detail (e.g. a token
    refresh) only prints with --verbose. Without this, loguru's default sink
    prints every DEBUG+ message unconditionally.
    """
    import logging

    from loguru import logger

    level = logging.DEBUG if verbose else logging.WARNING
    logger.remove()
    logger.add(sys.stderr, level=logging.getLevelName(level))
    for noisy in ("paramiko", "stac_geoparquet"):
        logging.getLogger(noisy).setLevel(level)
    return level


def _quiet_worker_logging(level: int) -> None:
    """Set noisy third-party loggers to *level* in a worker process.

    Runs as the scan's ``ProcessPoolExecutor`` initializer: a spawned worker
    starts with fresh logging, so the parent's quieting does not carry over and
    each worker would otherwise re-emit paramiko's connection banners.
    """
    import logging

    for noisy in ("paramiko", "stac_geoparquet"):
        logging.getLogger(noisy).setLevel(level)


# [FIXME] PG: This belongs somewhere else, it should not be directly in cli.py
@contextmanager
def _scan_progress(enabled: bool) -> Generator[Optional[object], None, None]:
    """A transient rich spinner+bar over a scan; yields an ``on_progress`` callback.

    Falls back to a throttled loguru INFO line every 30s when *enabled* is
    false — not a TTY, or the caller asked for verbose logs instead. Without
    this, ``-v``/a piped stdout went completely silent for the whole reading
    phase (confirmed live: many real minutes with zero feedback, reading a
    scan as hung when it was not) -- the rich bar's own progress had nowhere
    to go once disabled. The rich display, when enabled, is transient: it
    vanishes on exit, leaving only the final summary line (which goes to
    stdout, while the progress renders on stderr so a piped stdout stays
    clean).
    """
    if not enabled:
        import time

        from loguru import logger

        last = {"at": 0.0, "phase": None}

        def on_progress(event) -> None:
            now = time.monotonic()
            phase_changed = event.phase != last["phase"]
            # "sourcing" ticks once per file found (no known total -- could
            # be tens of thousands); "reading" ticks once per file too.
            # Confirmed live: throttling only applied to "reading", so
            # sourcing alone flooded the log with one line per file. Both
            # need it; only a phase change or the last reading tick (current
            # reaching total) always logs regardless of the 30s window.
            is_final_reading_tick = (
                event.phase == "reading"
                and event.total
                and event.current >= event.total
            )
            if (
                not phase_changed
                and not is_final_reading_tick
                and now - last["at"] < 30
            ):
                return
            last["at"], last["phase"] = now, event.phase
            if event.phase == "sourcing":
                logger.info("sourcing: {}", event.detail or "loading exp configs…")
            elif event.phase == "reading":
                detail = f" -- {event.detail}" if event.detail else ""
                logger.info("reading {}/{}{}", event.current, event.total, detail)
            elif event.phase == "writing":
                logger.info("writing catalog…")

        yield on_progress
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
                "sourcing", None, description="loading exp configs…", total=None
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
            enter_phase(
                "writing", "reading", description="writing catalog…", total=None
            )

    with progress:
        yield on_progress


@click.group(epilog=_CONFIG_EPILOG)
@click.version_option(version=__version__, prog_name="esm-catalog")
@click.option(
    "--json",
    "json_output",
    is_flag=True,
    is_eager=True,
    help="Emit machine-readable JSON instead of formatted text, where the "
    "command supports it (currently: server put, auth login). Unsupported commands "
    "ignore this flag.",
)
@click.pass_context
def main(ctx: click.Context, json_output: bool) -> None:
    """ESM-Tools simulation catalog."""
    ctx.obj = {"json": json_output}


# [NOTE] PG: Consider factoring these into separate files for "reusability" (we will never do that, but builder-pattern cli is good here for separateion)
@main.group()
def auth() -> None:
    """Authenticate against a STAC server (token cached locally)."""


@auth.command("login")
@click.argument("server_url")
@click.option(
    "--open", "open_browser", is_flag=True, help="Open the login URL in a browser."
)
@click.option(
    "-k",
    "--insecure",
    is_flag=True,
    help="Skip TLS verification (dev self-signed). Not persisted — pass it again "
    "on every 'server put' against this server.",
)
@click.pass_context
def auth_login(
    ctx: click.Context, server_url: str, open_browser: bool, insecure: bool
) -> None:
    """Log in and cache a token scoped to SERVER_URL.

    The token is cached per server (logging into a second server does not
    overwrite the first's token). SERVER_URL only labels which server this
    token is for — it is not itself contacted for login. The identity provider
    (who actually issues the token) is a separate system, configured via
    oidc_discovery_url/client_id (env or config file); see 'esm-catalog
    status' or the config file for what is currently resolved.

    Under --json: prints {"login_url": ..., "server_url": ...} as one line,
    then reads the code as a plain line from stdin instead of an interactive
    prompt — a script can drive a headless browser through login_url and
    write the code back on the same process's stdin. Prints
    {"server_url": ..., "token_cache_path": ..., "has_refresh_token": ...}
    on success, or {"error": ...} on failure (exit 1 either way on error).
    """
    json_output: bool = (ctx.obj or {}).get("json", False)

    from esm_catalog import auth as _auth
    from esm_catalog.config import Settings
    from esm_catalog.xdg import token_file

    def _die(message: str) -> NoReturn:
        if json_output:
            click.echo(json.dumps({"error": message}))
            sys.exit(1)
        raise click.ClickException(message)

    # -k only overrides; without it, ESM_CATALOG_VERIFY_TLS from env/config wins.
    settings = Settings(server_url=server_url)
    if insecure:
        settings.verify_tls = False
    try:
        meta = _auth.fetch_oidc_metadata(settings)
    except Exception as exc:  # noqa: BLE001 — surface any discovery failure cleanly
        _die(f"Could not reach the identity provider: {exc}")

    verifier, challenge = _auth.generate_pkce_pair()
    login_url = _auth.build_login_url(meta, settings, challenge)

    if json_output:
        click.echo(json.dumps({"login_url": login_url, "server_url": server_url}))
    else:
        click.secho("\nOpen this URL in a browser and log in:\n", fg="cyan")
        click.echo(login_url + "\n")
    if open_browser:
        import webbrowser

        webbrowser.open(login_url)

    if json_output:
        raw_code = sys.stdin.readline().strip()
        if not raw_code:
            _die(
                "No code read from stdin. The URL and PKCE challenge above are "
                "now stale; run this command again to restart."
            )
    else:
        try:
            raw_code = click.prompt("Paste the code from the landing page").strip()
        except click.Abort as exc:
            raise click.ClickException(
                "Login cancelled — no code was entered. The URL and PKCE challenge "
                "above are now stale; run this command again to restart."
            ) from exc
    code = _auth.AuthCode(raw_code)
    try:
        token = _auth.exchange_code_for_token(meta, settings, code, verifier)
    except _auth.AuthError as exc:
        _die(str(exc))
    _auth.save_token(token, server_url)

    if json_output:
        click.echo(
            json.dumps(
                {
                    "server_url": server_url,
                    "token_cache_path": str(token_file(server_url)),
                    "has_refresh_token": bool(token.refresh_token),
                    "expires_at": token.expires_at,
                }
            )
        )
        return

    click.secho(
        f"Logged in to {server_url} — token cached at {token_file(server_url)}",
        fg="green",
    )
    if token.refresh_token:
        click.secho(
            "Refresh token stored; future pushes will not need a login.", fg="green"
        )
    else:
        import datetime as _dt

        expiry = (
            _dt.datetime.fromtimestamp(token.expires_at).strftime("%Y-%m-%d %H:%M:%S")
            if token.expires_at is not None
            else "unknown"
        )
        click.secho(
            f"Note: no refresh token returned — you'll need to log in again once "
            f"this access token expires ({expiry}).",
            fg="yellow",
        )


@auth.command("logout")
@click.argument("server_url", required=False)
def auth_logout(server_url: Optional[str]) -> None:
    """Discard the cached token for SERVER_URL (default: the configured server)."""
    from esm_catalog.auth import clear_token
    from esm_catalog.config import Settings

    if server_url is None:
        server_url = Settings().server_url
    if not server_url:
        raise click.ClickException(
            "no server given and none configured; pass SERVER_URL, set "
            "ESM_CATALOG_SERVER_URL, or add server_url to the config file"
        )

    if clear_token(server_url):
        click.secho(f"Token cache for {server_url} removed.", fg="green")
    else:
        click.secho(f"Nothing to remove for {server_url}.", fg="yellow")


@main.group()
def workflow() -> None:
    """Higher-level operations than the bare server/asset nouns.

    Looser naming than 'server'/'asset' on purpose: these mirror esm_runscripts
    conventions (e.g. 'scan' and -e NAME) or do server-side JSON surgery that
    doesn't map cleanly to a single REST verb (e.g. 'reroot-experiment').
    """


@workflow.command("scan")
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
    "-j",
    "--jobs",
    type=click.IntRange(min=1),
    default=None,
    help="Number of parallel workers.",
)
@click.option("--strict", is_flag=True, help="Exit non-zero if any file fails to scan.")
@click.option(
    "--revalidate-every",
    type=click.IntRange(min=0),
    default=None,
    help="Once a stream's schema is frozen (from its first file), re-read every "
    "this-many-th later file for real instead of trusting a path-derived "
    "datetime -- catches schema drift. 0 disables it. Defaults to "
    "ESM_CATALOG_REVALIDATE_EVERY, or 20.",
)
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
    revalidate_every: Optional[int],
    verbose: bool,
) -> None:
    """Scan the experiment's output into the catalog (stac-geoparquet shards)."""
    import os

    from upath import UPath

    from esm_catalog.scan.ingest import ScanError, scan_experiment
    from esm_catalog.scan.sourcing import SourcingError

    level = _configure_logging(verbose)

    # ESM_CATALOG_PROFILE=/path/to/out.prof: cProfile the whole scan (dask
    # dispatch + the single-threaded item-building/writing phase), dumped for
    # `python -m pstats`/snakeviz. Off (zero overhead) unless set -- meant for
    # a one-off "what's actually slow" run, not routine use.
    profile_path = os.environ.get("ESM_CATALOG_PROFILE")

    def _run_scan():
        with _scan_progress(show_progress) as on_progress:
            kwargs = {}
            if revalidate_every is not None:
                kwargs["revalidate_every"] = revalidate_every
            return scan_experiment(
                UPath(exp_root),
                catalog=UPath(catalog_dir) if catalog_dir else None,
                distributed=distributed,
                scheduler=scheduler,
                jobs=jobs,
                strict=strict,
                on_progress=on_progress,
                worker_initializer=_quiet_worker_logging,
                worker_initargs=(level,),
                **kwargs,
            )

    show_progress = sys.stderr.isatty() and not verbose
    try:
        if profile_path:
            import cProfile

            profiler = cProfile.Profile()
            report = profiler.runcall(_run_scan)
            profiler.dump_stats(profile_path)
            click.echo(f"profile written to {profile_path}", err=True)
        else:
            report = _run_scan()
    except (SourcingError, ScanError) as exc:
        raise click.ClickException(str(exc)) from exc
    if report.scanned + report.skipped == 0:
        click.secho(
            f"0 output files found under {exp_root} — check --exp-root points "
            "at a completed ESM-Tools run.",
            fg="yellow",
        )
    click.echo(
        f"scanned {report.scanned}, catalogued {report.items}, "
        f"skipped {report.skipped}, unsupported {report.unsupported}, "
        f"failed {len(report.failures)}"
    )


def _parse_item_id(item_id: str) -> tuple[str, str]:
    """Split an Item id (``{component}-{stream}``, see item.py's ``_build_id``)
    back into its two parts. Splits on the *first* ``-`` only, so a stream
    name may itself contain one (component names never do, in practice)."""
    if "-" not in item_id:
        raise click.ClickException(
            f"{item_id!r} is not a valid item id (expected '<component>-<stream>')"
        )
    component, stream = item_id.split("-", 1)
    return component, stream


def _ad_hoc_shard_path(catalog, item_id: str):
    """A unique shard filename for a single hand-authored operation (``add``/
    ``validate``/``rm``) -- distinct from a real scan's own
    ``ts_shard_name``/``fx_shard_name`` convention, so the two are never
    confused (and never collide) on disk."""
    import uuid

    items_dir = catalog / "items"
    items_dir.mkdir(parents=True, exist_ok=True)
    safe_id = item_id.replace("/", "_")
    return items_dir / f"manual_{safe_id}_{uuid.uuid4().hex[:8]}.parquet"


def _resolve_catalog(exp_root: str, catalog_dir_opt: Optional[str]):
    from upath import UPath

    from esm_catalog.scan.workspace import catalog_dir as default_catalog_dir

    root = UPath(exp_root)
    catalog = UPath(catalog_dir_opt) if catalog_dir_opt else default_catalog_dir(root)
    return root, catalog


@main.group()
def asset() -> None:
    """Add, remove, or repoint an asset directly, without a full scan."""


@asset.command("add")
@click.argument("item_id")
@click.argument(
    "files", nargs=-1, required=True, type=click.Path(exists=True, path_type=Path)
)
@click.option(
    "--exp-root", default=".", help="Experiment root (for its finished_config)."
)
@click.option("--catalog-dir", default=None, help="Defaults to <exp-root>/catalog.")
@click.option(
    "--role", default="data", type=click.Choice(["data", "restart"]), show_default=True
)
@click.option(
    "--category",
    default=None,
    help="Restart category (e.g. 'oce_restart'); only meaningful with --role restart.",
)
def add_asset(
    item_id: str,
    files: tuple[Path, ...],
    exp_root: str,
    catalog_dir: Optional[str],
    role: str,
    category: Optional[str],
) -> None:
    """Add one or more FILES as assets of ITEM_ID (``<component>-<stream>``).

    Writes one local shard under the catalog's items/ directory -- run
    'esm-catalog server put' afterwards to ship it. Reuses ITEM_ID's cached schema
    (see 'esm-catalog scan') when available, falling back to a real read
    only for this stream's genuine first-ever asset. A single deliberate
    operation touching N files always writes exactly one shard, however
    many files are given -- to add an experiment's worth of files in bulk,
    use 'esm-catalog scan' instead.
    """
    from upath import UPath

    from esm_catalog.item import make_item
    from esm_catalog.scan.ingest import ingest_single_file
    from esm_catalog.scan.sourcing import source_experiment
    from esm_catalog.scan.types import OutputFile
    from esm_catalog.scan.workspace import WorkspaceState, load_state, save_state
    from esm_catalog.storage.geoparquet import write_shard

    component, stream = _parse_item_id(item_id)
    root, catalog = _resolve_catalog(exp_root, catalog_dir)
    exp_metadata = source_experiment(root)
    state = load_state(catalog) or WorkspaceState(
        experiment_id=exp_metadata.experiment_id
    )

    items = []
    for file_path in files:
        output_file = OutputFile(
            path=UPath(file_path),
            component=component,
            stream=stream,
            role=role,
            category=category,
        )
        result = ingest_single_file(output_file, state, force_read=False)
        if result.unsupported:
            click.secho(f"skipped {file_path} (unsupported format)", fg="yellow")
            continue
        if result.failure is not None:
            click.secho(f"failed {file_path}: {result.failure.error}", fg="red")
            continue
        items.append(make_item(output_file.path, result.file_metadata, exp_metadata))

    if not items:
        raise click.ClickException("no file could be added; see errors above")

    shard_path = _ad_hoc_shard_path(catalog, item_id)
    write_shard(items, shard_path)
    save_state(catalog, state)
    click.echo(f"added {len(items)} asset(s) to {item_id} -> {shard_path}")


def _resolve_href(location: str) -> str:
    """*location* as a STAC href: passed through unchanged if it already
    looks like a URI (has ``://`` -- covers a plain remote href and a
    chained fsspec string like ``tar://member::scoutfs://host/archive.tar``
    alike, neither of which this command should try to reparse), otherwise
    treated as a local path and built into a proper ``file://`` URI the
    same way a real scanned asset's href is (see item.py's ``_to_href``)."""
    if "://" in location:
        return location
    from upath import UPath

    from esm_catalog.item import _to_href

    return _to_href(UPath(location))


@asset.command("add-alternate")
@click.argument("item_id")
@click.argument("asset_key")
@click.argument("name")
@click.argument("location")
@click.option(
    "--exp-root", default=".", help="Experiment root (for its finished_config)."
)
@click.option("--catalog-dir", default=None, help="Defaults to <exp-root>/catalog.")
def add_alternate(
    item_id: str,
    asset_key: str,
    name: str,
    location: str,
    exp_root: str,
    catalog_dir: Optional[str],
) -> None:
    """Register LOCATION as the NAME alternate for ASSET_KEY on ITEM_ID.

    LOCATION is a URI (passed through as-is -- e.g. an HSM/tape address, or
    a chained fsspec string addressing a member inside an archived
    tarball) or a local path (turned into a proper file:// href). Per the
    'alternate-assets' STAC extension: this must be the exact same bytes as
    ASSET_KEY's primary location, just reachable a different way.

    Writes one local shard row -- run 'esm-catalog server put' afterwards to
    actually apply it (merge_item merges it into ASSET_KEY's own
    'alternate' dict without touching its other fields; this command alone
    changes nothing on the server, and needs no live fetch to work).
    """
    from datetime import datetime, timezone

    import pystac

    from esm_catalog.scan.sourcing import source_experiment
    from esm_catalog.storage.geoparquet import write_shard

    _parse_item_id(item_id)  # validated for a clear error message
    root, catalog = _resolve_catalog(exp_root, catalog_dir)
    exp_metadata = source_experiment(root)
    href = _resolve_href(location)

    row = pystac.Item(
        id=item_id,
        # No meaningful geometry/properties of its own -- same reasoning as
        # rm_asset's tombstone; existing is the real template at merge
        # time. The one asset entry is a harmless placeholder (pyarrow
        # cannot serialize an empty assets struct) -- its href must NOT be
        # the real alternate href: merge_item's assets.update(_real_assets)
        # runs unconditionally, before added_alternates is applied, and
        # would blindly overwrite ASSET_KEY's real primary href with
        # whatever this placeholder carries (confirmed live -- the exact
        # bug rm_asset's tombstone was already built to avoid). "about:blank"
        # keeps this row inert; the real href only ever appears inside
        # added_alternates, which merges into the asset's alternate dict
        # without touching href/roles/etc.
        geometry={"type": "Point", "coordinates": [0.0, 0.0]},
        bbox=[0.0, 0.0, 0.0, 0.0],
        datetime=datetime.now(timezone.utc),
        properties={},
        assets={
            asset_key: pystac.Asset(href="about:blank", roles=["alternate-placeholder"])
        },
        collection=exp_metadata.collection_id,
    )
    row.extra_fields["added_alternates"] = {asset_key: {name: {"href": href}}}

    shard_path = _ad_hoc_shard_path(catalog, item_id)
    write_shard([row], shard_path)
    click.echo(
        f"registered alternate {name!r} ({href}) for {asset_key!r} on {item_id} "
        f"-> {shard_path}"
    )


@asset.command("rm")
@click.argument("item_id")
@click.argument("asset_key")
@click.option(
    "--exp-root", default=".", help="Experiment root (for its finished_config)."
)
@click.option("--catalog-dir", default=None, help="Defaults to <exp-root>/catalog.")
def rm_asset(
    item_id: str, asset_key: str, exp_root: str, catalog_dir: Optional[str]
) -> None:
    """Mark ASSET_KEY on ITEM_ID for removal.

    Writes a tombstone as one local shard row -- run 'esm-catalog server put'
    afterwards to actually apply it (merge_item drops the key on push; this
    command alone changes nothing on the server). A whole Item/Collection is
    never deleted this way, only one asset key.
    """
    from datetime import datetime, timezone

    import pystac

    from esm_catalog.scan.sourcing import source_experiment
    from esm_catalog.storage.geoparquet import write_shard

    _parse_item_id(item_id)  # validated for a clear error message
    root, catalog = _resolve_catalog(exp_root, catalog_dir)
    exp_metadata = source_experiment(root)

    tombstone = pystac.Item(
        id=item_id,
        # A tombstone carries no meaningful geometry/properties of its own
        # (see push.merge_item: existing, not this row, is the template
        # once a real item already exists) -- this placeholder exists only
        # because pystac.Item requires *some* geometry/datetime. collection
        # still must be real: push groups items by it to know which
        # server collection to route the upsert into. The one asset entry
        # is a harmless self-canceling placeholder, not real data -- pyarrow
        # cannot serialize a struct with zero fields ("assets" with no
        # child), and this key is itself named in removed_assets below, so
        # it (or whatever real entry already exists under the same key) is
        # dropped by the merge regardless.
        geometry={"type": "Point", "coordinates": [0.0, 0.0]},
        bbox=[0.0, 0.0, 0.0, 0.0],
        datetime=datetime.now(timezone.utc),
        properties={},
        assets={asset_key: pystac.Asset(href="about:blank", roles=["tombstone"])},
        collection=exp_metadata.collection_id,
    )
    tombstone.extra_fields["removed_assets"] = [asset_key]

    shard_path = _ad_hoc_shard_path(catalog, item_id)
    write_shard([tombstone], shard_path)
    click.echo(f"marked {asset_key!r} on {item_id} for removal -> {shard_path}")


@asset.command("set-main")
@click.argument("item_id")
@click.argument("asset_key")
@click.option(
    "--to", "alternate_name", required=True, help="The alternate to promote to primary."
)
@click.option(
    "--demote-as",
    default=None,
    help="Alternate name to file the old primary href under. Omit to just drop it.",
)
@click.option(
    "--exp-root", default=".", help="Experiment root (for its finished_config)."
)
@click.option("--catalog-dir", default=None, help="Defaults to <exp-root>/catalog.")
def set_main_asset(
    item_id: str,
    asset_key: str,
    alternate_name: str,
    demote_as: Optional[str],
    exp_root: str,
    catalog_dir: Optional[str],
) -> None:
    """Promote ASSET_KEY's --to alternate to become its primary href.

    Needs no live fetch: writes a promote_alternate instruction as one local
    shard row -- merge_item resolves it at push time against whatever real
    asset state --exp-root/'esm-catalog server put' already has in hand (the
    same pattern 'rm asset'/'add alternate' already use). A no-op at merge
    time if ASSET_KEY or the named alternate does not actually exist -- run
    'esm-catalog server put' to find out, this command itself cannot check.
    """
    from datetime import datetime, timezone

    import pystac

    from esm_catalog.scan.sourcing import source_experiment
    from esm_catalog.storage.geoparquet import write_shard

    _parse_item_id(item_id)  # validated for a clear error message
    root, catalog = _resolve_catalog(exp_root, catalog_dir)
    exp_metadata = source_experiment(root)

    row = pystac.Item(
        id=item_id,
        geometry={"type": "Point", "coordinates": [0.0, 0.0]},
        bbox=[0.0, 0.0, 0.0, 0.0],
        datetime=datetime.now(timezone.utc),
        properties={},
        assets={
            asset_key: pystac.Asset(href="about:blank", roles=["set-main-placeholder"])
        },
        collection=exp_metadata.collection_id,
    )
    row.extra_fields["promote_alternate"] = {
        asset_key: {"from": alternate_name, "demote_as": demote_as}
    }

    shard_path = _ad_hoc_shard_path(catalog, item_id)
    write_shard([row], shard_path)
    click.echo(
        f"queued promoting {alternate_name!r} to primary for {asset_key!r} on "
        f"{item_id} -> {shard_path}"
    )


@main.command(hidden=True)
@click.argument("item_id")
@click.argument("file", type=click.Path(exists=True, path_type=Path))
@click.option(
    "--exp-root", default=".", help="Experiment root (for its finished_config)."
)
@click.option("--catalog-dir", default=None, help="Defaults to <exp-root>/catalog.")
@click.option(
    "--role", default="data", type=click.Choice(["data", "restart"]), show_default=True
)
@click.option(
    "--category",
    default=None,
    help="Restart category (e.g. 'oce_restart'); only meaningful with --role restart.",
)
def validate(
    item_id: str,
    file: Path,
    exp_root: str,
    catalog_dir: Optional[str],
    role: str,
    category: Optional[str],
) -> None:
    """Force a real read of FILE for ITEM_ID, refreshing its cached schema.

    Unlike 'esm-catalog add asset', never trusts a cached schema shortcut --
    always opens FILE. Writes one local shard row (same as 'add'); run
    'esm-catalog server put' afterwards to ship it. Does not participate in
    'esm-catalog scan --revalidate-every''s periodic checkpoint cadence --
    a one-off manual check has no "occurrence count" to advance.
    """
    from upath import UPath

    from esm_catalog.item import make_item
    from esm_catalog.scan.ingest import ingest_single_file
    from esm_catalog.scan.sourcing import source_experiment
    from esm_catalog.scan.types import OutputFile
    from esm_catalog.scan.workspace import WorkspaceState, load_state, save_state
    from esm_catalog.storage.geoparquet import write_shard

    component, stream = _parse_item_id(item_id)
    root, catalog = _resolve_catalog(exp_root, catalog_dir)
    exp_metadata = source_experiment(root)
    state = load_state(catalog) or WorkspaceState(
        experiment_id=exp_metadata.experiment_id
    )

    output_file = OutputFile(
        path=UPath(file),
        component=component,
        stream=stream,
        role=role,
        category=category,
    )
    result = ingest_single_file(output_file, state, force_read=True)
    if result.unsupported:
        raise click.ClickException(f"{file}: unsupported format")
    if result.failure is not None:
        raise click.ClickException(f"{file}: {result.failure.error}")

    item = make_item(output_file.path, result.file_metadata, exp_metadata)
    shard_path = _ad_hoc_shard_path(catalog, item_id)
    write_shard([item], shard_path)
    save_state(catalog, state)
    click.echo(f"validated {file} for {item_id} -> {shard_path}")


@main.group()
def server() -> None:
    """Read, write, or delete catalog objects on a STAC server."""


@server.command()
@click.argument(
    "paths",
    nargs=-1,
    required=True,
    type=click.Path(exists=True, path_type=Path, allow_dash=True),
)
@click.option("--server", default=None, help="Target STAC server (overrides config).")
@click.option(
    "-k",
    "--insecure",
    is_flag=True,
    help="Skip TLS verification (dev self-signed). Not persisted — pass it again "
    "on 'auth login' and on every 'server put' against this server.",
)
@click.option(
    "-v",
    "--verbose",
    is_flag=True,
    help="Show connection and library logs instead of the progress display.",
)
@click.option(
    "--resolve",
    "resolve_specs",
    multiple=True,
    metavar="HOST:PORT:IP",
    help="Pre-seed a DNS lookup with an IP, skipping the query (curl's own "
    "--resolve syntax). SNI and the Host header still use HOST, so "
    "certificate validation is unaffected — only useful when DNS itself is "
    "broken but the server is reachable by IP. Repeatable.",
)
@click.pass_context
def put(
    ctx: click.Context,
    paths: tuple[Path, ...],
    server: Optional[str],
    insecure: bool,
    verbose: bool,
    resolve_specs: tuple[str, ...],
) -> None:
    """Bulk-upsert STAC objects to the catalog.

    Each PATH is a Collection/Item JSON, a stac-geoparquet shard, a directory
    of them, or '-' to read one STAC JSON object from stdin. Writes are
    authenticated (run 'auth login' first) and idempotent (upsert) —
    re-running is safe, nothing is deleted.
    """
    json_output: bool = (ctx.obj or {}).get("json", False)

    import os

    from esm_catalog import auth
    from esm_catalog import push as pushmod
    from esm_catalog.client import StacClient
    from esm_catalog.config import Settings
    from esm_catalog.resolve import parse_resolve, ResolvingTransport

    _configure_logging(verbose)

    # Override only what the flags give, so config/env supplies the rest (init
    # kwargs have top precedence — passing them unconditionally clobbers env).
    settings = Settings()
    if server:
        settings.server_url = server
    if insecure:
        settings.verify_tls = False

    def _die(message: str) -> NoReturn:
        """Report a fatal error and exit, in whichever format was requested."""
        if json_output:
            click.echo(json.dumps({"error": message}))
            sys.exit(1)
        raise click.ClickException(message)

    try:
        api_url = settings.api_url
        token = auth.get_bearer_token(settings)
    except (ValueError, auth.AuthError) as exc:
        _die(str(exc))

    try:
        resolve_map = {
            (host, port): ip
            for host, port, ip in (parse_resolve(spec) for spec in resolve_specs)
        }
    except ValueError as exc:
        _die(str(exc))
    transport = (
        ResolvingTransport(resolve_map, verify_tls=settings.verify_tls)
        if resolve_map
        else None
    )

    # '-' means "read one STAC JSON object from stdin" -- a fast path, since
    # mixing it with the file-based classify/expand/push pipeline below would
    # need a throwaway temp file for no real benefit (one object, one upsert).
    if list(paths) == [Path("-")]:
        obj = json.loads(sys.stdin.read())
        with StacClient(
            api_url, token, verify_tls=settings.verify_tls, transport=transport
        ) as client:
            if str(obj.get("type", "")).lower() == "collection":
                client.upsert_collection(obj)
            else:
                client.upsert_item(obj)
        if json_output:
            click.echo(json.dumps({"id": obj.get("id"), "status": "ok"}))
        else:
            click.echo(f"put {obj.get('id')!r}")
        return
    if any(str(p) == "-" for p in paths):
        _die("'-' (stdin) must be the only PATH given, not mixed with others")

    files = pushmod.expand_paths(list(paths))
    total = sum(
        (
            1
            if pushmod.classify_file(f) in ("collection", "item")
            else pushmod.count_items(f)
        )
        for f in files
    )

    # ESM_CATALOG_PROFILE=/path/to/out.prof: cProfile the push (shard read-back
    # into Items + upload), dumped for `python -m pstats`/snakeviz. Off (zero
    # overhead) unless set -- same one-off "what's actually slow" hook as scan.
    profile_path = os.environ.get("ESM_CATALOG_PROFILE")

    def _run_push():
        show_progress = sys.stderr.isatty() and not json_output
        with StacClient(
            api_url, token, verify_tls=settings.verify_tls, transport=transport
        ) as client:
            with _push_progress(show_progress, total) as advance:
                return pushmod.push_paths(
                    paths,
                    client,
                    on_progress=advance,
                    include_traceback=verbose and json_output,
                )

    if profile_path:
        import cProfile

        profiler = cProfile.Profile()
        summary = profiler.runcall(_run_push)
        profiler.dump_stats(profile_path)
        click.echo(f"profile written to {profile_path}", err=True)
    else:
        summary = _run_push()

    # If a pushed catalog carries queryables the server has not registered, tell
    # the operator how to register them (filtering already works; this only
    # surfaces the fields in the STAC Browser filter UI).
    unregistered_queryables = []
    for directory in (p for p in paths if p.is_dir()):
        delta = pushmod.queryable_delta(directory, api_url, settings.verify_tls)
        if delta is None:
            continue
        properties = json.loads(delta.read_text()).get("properties", {})
        if json_output:
            unregistered_queryables.append(
                {
                    "directory": str(directory),
                    "count": len(properties),
                    "delta_path": str(delta),
                }
            )
        else:
            _report_new_queryables(properties, delta, settings.server_url)

    if json_output:
        result = summary.model_dump()
        result["unregistered_queryables"] = unregistered_queryables
        click.echo(json.dumps(result))
        if summary.errors:
            sys.exit(1)
        return

    click.echo(
        f"pushed {summary.collections} collection(s), {summary.items} item(s) "
        f"from {summary.shards} shard(s)"
    )
    if summary.errors:
        for err in summary.errors:
            click.secho(f"  ! {err}", fg="red", err=True)
        raise click.ClickException(f"{len(summary.errors)} path(s) failed.")


@main.command(hidden=True)
@click.argument(
    "paths",
    nargs=-1,
    required=True,
    type=click.Path(exists=True, path_type=Path, allow_dash=True),
)
@click.option("--server", default=None)
@click.option("-k", "--insecure", is_flag=True)
@click.option("-v", "--verbose", is_flag=True)
@click.option("--resolve", "resolve_specs", multiple=True, metavar="HOST:PORT:IP")
@click.pass_context
def push(
    ctx: click.Context,
    paths: tuple[Path, ...],
    server: Optional[str],
    insecure: bool,
    verbose: bool,
    resolve_specs: tuple[str, ...],
) -> None:
    """Deprecated alias for 'server put' — will be removed in a future release."""
    click.secho("warning: 'push' is deprecated, use 'server put' instead", fg="yellow", err=True)
    ctx.invoke(
        put,
        paths=paths,
        server=server,
        insecure=insecure,
        verbose=verbose,
        resolve_specs=resolve_specs,
    )


def _resolve_output_format(json_fmt: bool, pretty_fmt: bool) -> str:
    """Resolve 'json' vs 'pretty' from the command's own flags, or config."""
    if json_fmt and pretty_fmt:
        raise click.UsageError("--json and --pretty are mutually exclusive.")
    if json_fmt:
        return "json"
    if pretty_fmt:
        return "pretty"
    from esm_catalog.config import Settings

    return Settings().output_format


def _settings_for(server: Optional[str], insecure: bool):
    from esm_catalog.config import Settings

    settings = Settings()
    if server:
        settings.server_url = server
    if insecure:
        settings.verify_tls = False
    return settings


@contextmanager
def _stac_client(settings):
    """An authenticated StacClient for *settings*, or a clean ClickException."""
    from esm_catalog import auth
    from esm_catalog.client import StacClient

    try:
        api_url = settings.api_url
        token = auth.get_bearer_token(settings)
    except (ValueError, auth.AuthError) as exc:
        raise click.ClickException(str(exc)) from exc

    with StacClient(api_url, token, verify_tls=settings.verify_tls) as client:
        yield client


def _emit_single(obj: dict, fmt: str, render_pretty) -> None:
    if fmt == "json":
        click.echo(json.dumps(obj))
    else:
        render_pretty(obj)


def _emit_list(objs: list[dict], fmt: str, render_pretty) -> None:
    if fmt == "json":
        for obj in objs:
            click.echo(json.dumps(obj))
    else:
        render_pretty(objs)


def _render_collections_table(collections: list[dict]) -> None:
    from rich.console import Console
    from rich.table import Table

    table = Table()
    table.add_column("ID")
    table.add_column("Title")
    table.add_column("Description")
    for c in collections:
        table.add_row(
            c.get("id", ""), c.get("title", "") or "", (c.get("description") or "")[:60]
        )
    Console().print(table)


def _render_collection_pretty(c: dict) -> None:
    click.echo(f"id: {c.get('id')}")
    click.echo(f"title: {c.get('title', '')}")
    click.echo(f"description: {c.get('description', '')}")
    assets = c.get("assets") or {}
    if assets:
        click.echo("assets:")
        for key, asset in assets.items():
            click.echo(f"  {key}: {asset.get('href', '')}")


def _render_items_table(items: list[dict]) -> None:
    from rich.console import Console
    from rich.table import Table

    table = Table()
    table.add_column("ID")
    table.add_column("Collection")
    table.add_column("Datetime")
    for it in items:
        props = it.get("properties", {})
        table.add_row(it.get("id", ""), it.get("collection", ""), str(props.get("datetime", "")))
    Console().print(table)


def _render_item_pretty(it: dict) -> None:
    click.echo(f"id: {it.get('id')}")
    click.echo(f"collection: {it.get('collection')}")
    click.echo(json.dumps(it.get("properties", {}), indent=2))
    assets = it.get("assets") or {}
    if assets:
        click.echo("assets:")
        for key, asset in assets.items():
            click.echo(f"  {key}: {asset.get('href', '')}")
            alternates = (asset.get("alternate") or {})
            for alt_name, alt in alternates.items():
                click.echo(f"    alternate:{alt_name}: {alt.get('href', '')}")


_SERVER_OPTION = click.option(
    "--server", default=None, help="Target STAC server (overrides config)."
)
_INSECURE_OPTION = click.option(
    "-k", "--insecure", is_flag=True, help="Skip TLS verification (dev self-signed)."
)


@workflow.command("reroot-experiment")
@click.argument("old_root")
@click.argument("new_root")
@click.option(
    "--exp-root",
    default=".",
    help="Experiment root; may be remote (e.g. sftp://host/path). Defaults to '.'.",
)
@click.option(
    "--catalog-dir",
    default=None,
    help="Local catalog to rewrite; defaults to <exp-root>/catalog. Must already "
    "exist (a prior 'workflow scan') -- this command edits local shards, it "
    "does not paginate everything off the server.",
)
@_SERVER_OPTION
@_INSECURE_OPTION
@click.option(
    "--dry-run", is_flag=True,
    help="Report what would change without writing or pushing anything.",
)  # fmt: skip
def reroot_experiment(
    old_root: str,
    new_root: str,
    exp_root: str,
    catalog_dir: Optional[str],
    server: Optional[str],
    insecure: bool,
    dry_run: bool,
) -> None:
    """Rewrite OLD_ROOT to NEW_ROOT in every asset href, after an on-disk move.

    Scoped to asset hrefs only (never a blind substring replace across the
    whole document) -- the Collection's own assets, plus every Item's assets
    in every local shard under ITEMS. Then pushes exactly what changed,
    unless --dry-run.
    """
    from esm_catalog.workflow_reroot import push_rerooted
    from esm_catalog.workflow_reroot import reroot_experiment as do_reroot

    _, catalog = _resolve_catalog(exp_root, catalog_dir)
    if not catalog.exists():
        raise click.ClickException(
            f"no local catalog at {catalog} -- run 'esm-catalog workflow scan' first"
        )

    report = do_reroot(catalog, old_root, new_root, dry_run=dry_run)

    if report.total_changed == 0:
        click.echo(f"no asset hrefs under {catalog} contain {old_root!r}")
        return

    click.echo(
        f"{'would rewrite' if dry_run else 'rewrote'} "
        f"{report.collection_assets_changed} collection asset(s), "
        f"{report.item_assets_changed} item asset(s) across "
        f"{len(report.shards_touched)} shard(s)"
    )

    if dry_run:
        return

    settings = _settings_for(server, insecure)
    with _stac_client(settings) as client:
        summary = push_rerooted(catalog, report, client)
    if summary is not None:
        click.echo(f"pushed {summary.collections} collection(s), {summary.items} item(s)")
        if summary.errors:
            for error in summary.errors:
                click.secho(f"  ! {error.path}: {error.message}", fg="red")
            raise click.ClickException(f"{len(summary.errors)} path(s) failed to push")


_FORMAT_OPTIONS = [
    click.option(
        "--json",
        "json_fmt",
        is_flag=True,
        help="Emit JSON (newline-delimited for a list, one object for a single "
        "resource).",
    ),
    click.option(
        "--pretty",
        "pretty_fmt",
        is_flag=True,
        help="Emit a human-readable table/summary (default unless configured "
        "otherwise; see output_format in the config file).",
    ),
]


def _format_options(fn):
    for option in _FORMAT_OPTIONS:
        fn = option(fn)
    return fn


@server.group()
def queryables() -> None:
    """Inspect queryable fields registered on a STAC server."""


@queryables.command("get")
@_SERVER_OPTION
@_INSECURE_OPTION
@_format_options
def queryables_get(
    server: Optional[str], insecure: bool, json_fmt: bool, pretty_fmt: bool
) -> None:
    """Show the queryable fields currently registered on the server.

    Compose with a local catalog's queryables.json and jq/diff to find
    what's missing -- e.g.:

        jq '.properties|keys' catalog/queryables.json > local.txt
        esm-catalog server queryables get --json | jq '.|keys' > remote.txt
        diff local.txt remote.txt
    """
    from esm_catalog.push import get_queryables

    fmt = _resolve_output_format(json_fmt, pretty_fmt)
    settings = _settings_for(server, insecure)
    try:
        api_url = settings.api_url
    except ValueError as exc:
        raise click.ClickException(str(exc)) from exc
    properties = get_queryables(api_url, settings.verify_tls)
    if fmt == "json":
        click.echo(json.dumps(properties))
        return
    from rich.console import Console
    from rich.table import Table

    table = Table()
    table.add_column("Property")
    table.add_column("Type")
    for name, schema in sorted(properties.items()):
        table.add_row(name, str(schema.get("type", "")))
    Console().print(table)


@queryables.command("print-recipe")
@click.argument(
    "delta_file",
    required=False,
    type=click.Path(exists=True, dir_okay=False, path_type=Path, allow_dash=True),
)
@_SERVER_OPTION
def queryables_print_recipe(delta_file: Optional[Path], server: Optional[str]) -> None:
    """Print the ssh/sudo recipe to register a queryables delta on the pgstac host.

    DELTA_FILE is a {"properties": {...}} JSON document -- e.g. produced by
    diffing 'server queryables get --json' against a local queryables.json
    (see that command's help for a jq recipe) -- or '-'/omitted to read it
    from stdin.
    """
    if delta_file is None or str(delta_file) == "-":
        import tempfile

        raw = sys.stdin.read()
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".json", delete=False
        ) as handle:
            handle.write(raw)
            delta_path = Path(handle.name)
    else:
        raw = delta_file.read_text()
        delta_path = delta_file

    properties = json.loads(raw).get("properties", {})
    if not properties:
        click.echo("nothing to register")
        return
    settings = _settings_for(server, insecure=False)
    _report_new_queryables(properties, delta_path, settings.server_url)


@server.group()
def get() -> None:
    """Fetch a collection or item from the server."""


@get.command("collections")
@click.argument("collection_id", required=False)
@_SERVER_OPTION
@_INSECURE_OPTION
@click.option(
    "--limit", type=int, default=None,
    help="Max results (list form only); passed through to the API's own 'limit'.",
)  # fmt: skip
@_format_options
def get_collections(
    collection_id: Optional[str],
    server: Optional[str],
    insecure: bool,
    limit: Optional[int],
    json_fmt: bool,
    pretty_fmt: bool,
) -> None:
    """List all collections, or fetch COLLECTION_ID."""
    fmt = _resolve_output_format(json_fmt, pretty_fmt)
    settings = _settings_for(server, insecure)
    with _stac_client(settings) as client:
        if collection_id:
            obj = client.get_collection(collection_id)
            if obj is None:
                raise click.ClickException(f"collection {collection_id!r} not found")
            _emit_single(obj, fmt, _render_collection_pretty)
        else:
            resp = client.list_collections(limit)
            _emit_list(resp.get("collections", []), fmt, _render_collections_table)


@get.command("items")
@click.argument("collection_id")
@click.argument("item_id", required=False)
@_SERVER_OPTION
@_INSECURE_OPTION
@click.option(
    "--limit", type=int, default=None,
    help="Max results (list form only); passed through to the API's own 'limit'.",
)  # fmt: skip
@click.option(
    "--filter", "cql2_filter", default=None,
    help=(
        "List form only: a cql2-text filter, passed through to the API verbatim "
        "(OGC API Features Filter extension), e.g. --filter \"variable='temp2'\". "
        "A plain top-level property needs no queryables registration to be "
        "filterable this way."
    ),
)  # fmt: skip
@_format_options
def get_items(
    collection_id: str,
    item_id: Optional[str],
    server: Optional[str],
    insecure: bool,
    limit: Optional[int],
    cql2_filter: Optional[str],
    json_fmt: bool,
    pretty_fmt: bool,
) -> None:
    """List all items in COLLECTION_ID, or fetch ITEM_ID."""
    fmt = _resolve_output_format(json_fmt, pretty_fmt)
    settings = _settings_for(server, insecure)
    with _stac_client(settings) as client:
        if item_id:
            if cql2_filter is not None:
                raise click.ClickException("--filter only applies when listing items")
            obj = client.get_item(collection_id, item_id)
            if obj is None:
                raise click.ClickException(
                    f"item {item_id!r} not found in {collection_id!r}"
                )
            _emit_single(obj, fmt, _render_item_pretty)
        else:
            resp = client.list_items(collection_id, limit, cql2_filter)
            _emit_list(resp.get("features", []), fmt, _render_items_table)


@server.group()
def delete() -> None:
    """Remove a collection or item from the server."""


@delete.command("collections")
@click.argument("collection_id")
@_SERVER_OPTION
@_INSECURE_OPTION
@click.option("-y", "--yes", is_flag=True, help="Skip the confirmation prompt.")
def delete_collections(
    collection_id: str, server: Optional[str], insecure: bool, yes: bool
) -> None:
    """Delete COLLECTION_ID (cascades its items server-side)."""
    from esm_catalog.client import StacClientError

    settings = _settings_for(server, insecure)
    with _stac_client(settings) as client:
        if not yes:
            try:
                listing = client.list_items(collection_id, limit=1)
            except StacClientError as exc:
                raise click.ClickException(str(exc)) from exc
            count = listing.get("numberMatched", len(listing.get("features", [])))
            confirmed = questionary.confirm(
                f"delete collection {collection_id!r} ({count} items)?", default=False
            ).ask()
            if not confirmed:
                click.echo("aborted")
                return
        try:
            client.delete_collection(collection_id)
        except StacClientError as exc:
            raise click.ClickException(str(exc)) from exc
    click.secho(f"deleted collection {collection_id!r}", fg="green")


@delete.command("items")
@click.argument("collection_id")
@click.argument("item_id")
@_SERVER_OPTION
@_INSECURE_OPTION
@click.option("-y", "--yes", is_flag=True, help="Skip the confirmation prompt.")
def delete_items(
    collection_id: str, item_id: str, server: Optional[str], insecure: bool, yes: bool
) -> None:
    """Delete a single ITEM_ID from COLLECTION_ID."""
    from esm_catalog.client import StacClientError

    settings = _settings_for(server, insecure)
    with _stac_client(settings) as client:
        if not yes:
            confirmed = questionary.confirm(
                f"delete item {item_id!r} in {collection_id!r}?", default=False
            ).ask()
            if not confirmed:
                click.echo("aborted")
                return
        try:
            client.delete_item(collection_id, item_id)
        except StacClientError as exc:
            raise click.ClickException(str(exc)) from exc
    click.secho(f"deleted item {item_id!r} from {collection_id!r}", fg="green")


def _report_new_queryables(
    properties: dict, delta_path: Path, server_url: Optional[str]
) -> None:
    """Print the operator recipe for registering new queryables."""
    count = len(properties)
    host = (server_url or "<pgstac-host>").split("://", 1)[-1].rstrip("/")
    click.secho(
        f"\n{count} new queryable field(s) are not yet registered.", fg="yellow"
    )
    click.echo(
        "Filtering already works without this — registration only makes these\n"
        "fields appear in the STAC Browser filter UI. A privileged operator runs\n"
        "on the pgstac host (adjust the ssh name if it differs from the API host):\n"
    )
    # Absolute path: sudo's secure_path need not include /usr/local/bin.
    click.secho(
        f"  ssh {host} sudo -u stac /usr/local/bin/esm-catalog-load-queryables "
        f"- < {delta_path}\n",
        fg="cyan",
    )


@contextmanager
def _push_progress(enabled: bool, total: int) -> Generator[object, None, None]:
    """A transient rich bar over a push; yields an ``advance(n, detail)`` callback."""
    if not enabled:
        yield lambda advance, detail: None
        return

    from rich.console import Console
    from rich.progress import (
        BarColumn,
        MofNCompleteColumn,
        Progress,
        SpinnerColumn,
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
    with progress:
        task = progress.add_task("pushing", total=total or None)

        def advance(n: int, detail: str) -> None:
            progress.update(task, advance=n, description=f"pushing — {detail}")

        yield advance


@main.command()
@click.option(
    "--exp-root",
    default=".",
    help="Experiment root; may be remote (e.g. sftp://host/path). Defaults to '.'.",
)
def status(exp_root: str) -> None:
    """Show the local catalog's state and the configured server put target.

    Reports what a scan has produced on disk (shards, item counts, incremental
    bookkeeping) and where 'server put' would send it. Does not contact the server —
    'server put' itself is the only thing that knows what has actually been shipped,
    since nothing local tracks put history.
    """
    from upath import UPath

    from esm_catalog.config import Settings
    from esm_catalog.scan.workspace import (
        QUERYABLES_FILENAME,
        catalog_dir,
        load_state,
    )
    from esm_catalog.storage.geoparquet import read_shard

    import json

    catalog = catalog_dir(UPath(exp_root))
    click.echo(f"exp-root:  {exp_root}")
    click.echo(f"catalog:   {catalog}")

    state = load_state(catalog)
    if state is None:
        click.secho("Not yet scanned — run 'esm-catalog scan' first.", fg="yellow")
    else:
        click.echo(f"experiment: {state.experiment_id}")
        click.echo(f"tracked (incremental) files: {len(state.scanned)}")

        collection_path = catalog / "collection.json"
        if collection_path.exists():
            collection_id = json.loads(collection_path.read_text()).get("id", "?")
            click.echo(f"collection: {collection_id}")

        items_dir = catalog / "items"
        shard_paths = sorted(items_dir.glob("*.parquet")) if items_dir.exists() else []
        total_items = sum(read_shard(p).num_rows for p in shard_paths)
        click.echo(f"shards: {len(shard_paths)} ({total_items} item(s) total)")

        queryables_path = catalog / QUERYABLES_FILENAME
        if queryables_path.exists():
            count = len(json.loads(queryables_path.read_text()).get("properties", {}))
            click.echo(f"queryables: {count}")

    try:
        server_url = Settings().server_url
    except Exception:  # noqa: BLE001 — a broken config must not crash status
        server_url = None
    if server_url:
        click.echo(f"server put target: {server_url}")
        from esm_catalog.auth import load_token

        click.echo(
            "logged in: " + ("yes" if load_token(server_url) is not None else "no")
        )
    else:
        click.secho(
            "server put target: not configured (set server_url or ESM_CATALOG_SERVER_URL)",
            fg="yellow",
        )


@main.command("validate-cmip6", hidden=True)
@click.option(
    "--exp-root",
    default=".",
    help="Experiment root; may be remote (e.g. sftp://host/path). Defaults to '.'.",
)
def validate_cmip6(exp_root: str) -> None:
    """Check the scanned catalog's cmip6:* facets against the live esgvoc CV.

    Reads ``collection.json`` (written by 'scan') rather than re-parsing the
    experiment config, and reports whether each declared facet is a real,
    registered term — catching a wrong case or an unregistered model name
    before publication, not after. A no-op if the experiment declares no
    cmip6 facets. Requires the optional 'catalog-esgvoc' extra and a locally
    installed CV (``esgvoc use <project>@latest``).
    """
    from upath import UPath

    from esm_catalog.cmip6 import Cmip6Config
    from esm_catalog.esgvoc_validate import validate_cmip6_config
    from esm_catalog.scan.workspace import catalog_dir

    catalog = catalog_dir(UPath(exp_root))
    collection_path = catalog / "collection.json"
    if not collection_path.exists():
        raise click.ClickException(
            f"no collection.json under {catalog} — run 'esm-catalog scan' first."
        )

    summaries = json.loads(collection_path.read_text()).get("summaries", {})
    prefix = "cmip6:"
    facets = {
        key[len(prefix) :]: values[0]
        for key, values in summaries.items()
        if key.startswith(prefix) and values
    }
    if not facets:
        click.echo("No cmip6:* facets declared — nothing to validate.")
        return

    try:
        issues = validate_cmip6_config(Cmip6Config(**facets))
    except ImportError as exc:
        raise click.ClickException(str(exc)) from exc
    except Exception as exc:  # noqa: BLE001 — e.g. esgvoc's EsgvocNotFoundError
        # when the package is installed but the CV database ("esgvoc use
        # <project>@latest") isn't -- report cleanly, not a raw traceback.
        raise click.ClickException(f"esgvoc CV lookup failed: {exc}") from exc

    if not issues:
        click.secho(f"All {len(facets)} declared cmip6 facet(s) are valid.", fg="green")
        return

    for issue in issues:
        click.secho(
            f"  cmip6:{issue.field}={issue.value!r} is not a registered term "
            f"in '{issue.collection}'",
            fg="red",
        )
    raise click.ClickException(f"{len(issues)} invalid cmip6 facet(s).")


@main.command("list-plugins")
@click.pass_context
def list_plugins(ctx: click.Context) -> None:
    """List registered Item/Collection extension plugins.

    Each extension (datacube, namelist, paleo, contacts, ...) registers
    itself against the item and/or collection contract (see
    esm_catalog.plugins) rather than being hardcoded into item/collection
    building — this shows what is currently registered.
    """
    from esm_catalog.plugins import get_plugin_manager

    pm = get_plugin_manager()
    item_impls = {hi.plugin_name for hi in pm.hook.apply_to_item.get_hookimpls()}
    collection_impls = {
        hi.plugin_name for hi in pm.hook.apply_to_collection.get_hookimpls()
    }

    rows = []
    for name, plugin in pm.list_name_plugin():
        hooks = []
        if name in item_impls:
            hooks.append("item")
        if name in collection_impls:
            hooks.append("collection")
        doc = (plugin.__doc__ or "").strip().splitlines()
        rows.append(
            {"plugin": name, "hooks": hooks, "description": doc[0] if doc else ""}
        )

    if ctx.obj.get("json"):
        click.echo(json.dumps(rows, indent=2))
        return

    from rich.console import Console
    from rich.table import Table

    table = Table()
    table.add_column("Plugin")
    table.add_column("Contract")
    table.add_column("Description")
    for row in rows:
        table.add_row(row["plugin"], ", ".join(row["hooks"]), row["description"])
    Console().print(table)


@main.group(hidden=True)
def distributed() -> None:
    """Render the SLURM + Dask + Singularity pipeline for a large scan.

    'esm-catalog scan --distributed --scheduler tcp://...' attaches to an
    already-running Dask scheduler; it does not create one. This group
    renders the SLURM scripts that do -- see 'render-scripts --help'.

    Hidden: SLURM/Dask cluster orchestration isn't this CLI's job (STAC
    cataloging) -- flagged as a candidate for extraction into its own tool.
    Still fully functional for existing callers.
    """


@distributed.command("render-scripts")
@click.argument(
    "vars_file",
    required=False,
    type=click.Path(exists=True, dir_okay=False, path_type=Path),
)
@click.option(
    "--job-prefix", help="SLURM job name prefix (<prefix>-sched, -worker, ...)."
)
@click.option("--scratch-dir", help="Coordination + container-cache directory.")
@click.option("--image-tag", help="Container tag, e.g. v6.68.0-rc.1-test-0.1.11.")
@click.option("--exp-root", help="Experiment directory the driver will scan.")
@click.option("--catalog-dir", help="Where the driver writes the catalog.")
@click.option(
    "--worker-mode",
    type=click.Choice(["array", "multinode"]),
    help="'array': one SLURM job per worker (default; Albedo-style, no tight "
    "running-job cap). 'multinode': one job, srun fans workers out inside "
    "it (for a site with a tight per-user running-job cap, e.g. Levante).",
)
@click.option(
    "--n-workers", type=int, help="Size of the worker job array (worker_mode=array)."
)
@click.option(
    "--throttle",
    type=int,
    help="Cap concurrent array elements (worker_mode=array; default: n_workers, i.e. no throttle).",
)
@click.option(
    "--n-nodes",
    type=int,
    help="Node count for the one worker job (worker_mode=multinode).",
)
@click.option(
    "--cores-per-node",
    type=int,
    help="Workers per node in multinode mode (default: 128, Levante's compute node).",
)
@click.option("--partition", help="SLURM partition (default: smp).")
@click.option("--qos", help="SLURM QOS (default: 12h).")
@click.option(
    "--account",
    help="SBATCH --account (no default; omitted from scripts unless set).",
)
@click.option("--walltime", help="SLURM time limit (default: 04:00:00).")
@click.option(
    "--bind-path",
    "bind_paths",
    multiple=True,
    help="A -B mount; repeat for multiple (default: /albedo).",
)
@click.option(
    "--container-bin",
    help="Container binary, e.g. singularity or apptainer (default: singularity).",
)
@click.option(
    "--container-module", help="Module to load (default: same as --container-bin)."
)
@click.option(
    "--push-after-scan/--no-push-after-scan",
    default=None,
    help="Have the driver run 'push' after 'scan' (default: off).",
)
@click.option("--server-url", help="Push target when --push-after-scan is set.")
@click.option(
    "--log-dir",
    help="SBATCH -o directory (default: $XDG_STATE_HOME/esm-catalog/logs).",
)
@click.option(
    "--out-dir",
    default=".",
    type=click.Path(file_okay=False, path_type=Path),
    help="Where to write the four .sbatch files.",
)
@click.option(
    "--dump-vars-template",
    is_flag=True,
    help="Print a ready-to-edit vars.yaml skeleton to stdout and exit -- "
    "ignores every other option.",
)
def distributed_render_scripts(
    vars_file: Optional[Path],
    job_prefix: Optional[str],
    scratch_dir: Optional[str],
    image_tag: Optional[str],
    exp_root: Optional[str],
    catalog_dir: Optional[str],
    worker_mode: Optional[str],
    n_workers: Optional[int],
    throttle: Optional[int],
    n_nodes: Optional[int],
    cores_per_node: Optional[int],
    partition: Optional[str],
    qos: Optional[str],
    account: Optional[str],
    walltime: Optional[str],
    bind_paths: tuple[str, ...],
    container_bin: Optional[str],
    container_module: Optional[str],
    push_after_scan: Optional[bool],
    server_url: Optional[str],
    log_dir: Optional[str],
    out_dir: Path,
    dump_vars_template: bool,
) -> None:
    """Render sched/worker/driver/cleanup .sbatch scripts for a distributed scan.

    VARS_FILE is an optional YAML file of defaults (job_prefix, scratch_dir,
    image_tag, exp_root, catalog_dir are always required; n_workers is
    required for worker_mode=array, n_nodes for worker_mode=multinode; the
    rest optional); any --flag overrides what it sets, same resolution order
    as the rest of the CLI. Required values missing from both the file and
    the flags are reported together, not one at a time.
    """
    overrides = {
        "job_prefix": job_prefix,
        "scratch_dir": scratch_dir,
        "image_tag": image_tag,
        "exp_root": exp_root,
        "catalog_dir": catalog_dir,
        "worker_mode": worker_mode,
        "n_workers": n_workers,
        "throttle": throttle,
        "n_nodes": n_nodes,
        "cores_per_node": cores_per_node,
        "partition": partition,
        "qos": qos,
        "account": account,
        "walltime": walltime,
        "bind_paths": list(bind_paths) or None,
        "container_bin": container_bin,
        "container_module": container_module,
        "push_after_scan": push_after_scan,
        "server_url": server_url,
        "log_dir": log_dir,
    }
    overrides = {key: value for key, value in overrides.items() if value is not None}

    if dump_vars_template:
        from esm_catalog.distributed import render_vars_template

        click.echo(render_vars_template(overrides), nl=False)
        return

    import yaml

    from esm_catalog.distributed import render_scripts
    from esm_catalog.xdg import state_dir

    context: dict = {}
    if vars_file is not None:
        context = yaml.safe_load(vars_file.read_text()) or {}

    context.update(overrides)
    context.setdefault("log_dir", str(state_dir() / "logs"))

    try:
        written = render_scripts(context, out_dir)
    except ValueError as exc:
        raise click.ClickException(str(exc)) from exc

    for path in written:
        click.echo(f"wrote {path}")

    defaulted = [
        f"{name}={default}"
        for name, default in (("qos", "12h"), ("partition", "smp"))
        if name not in context
    ]
    if defaulted:
        click.echo(
            f"note: using Albedo defaults not overridden: {', '.join(defaulted)} "
            "-- pass --qos/--partition if your site uses different names"
        )
    if "account" not in context:
        click.echo(
            "note: no --account set -- omitted from every script; pass "
            "--account if your site requires one for sbatch to accept the job"
        )

    sched_path, worker_path, driver_path, cleanup_path = written
    click.echo()
    click.echo(
        "Submit in this order (scheduler, then driver and workers "
        "together, then cleanup dependent on the driver):"
    )
    click.echo(f"  SCHED_JOBID=$(sbatch --parsable {sched_path})")
    click.echo(
        f"  DRIVER_JOBID=$(sbatch --parsable --dependency=after:$SCHED_JOBID {driver_path})"
    )
    click.echo(
        f"  WORKER_JOBID=$(sbatch --parsable --dependency=after:$SCHED_JOBID {worker_path})"
    )
    click.echo(
        "  sbatch --dependency=afterany:$DRIVER_JOBID "
        "--export=ALL,SCHED_JOBID=$SCHED_JOBID,WORKER_JOBID=$WORKER_JOBID "
        f"{cleanup_path}"
    )


if __name__ == "__main__":
    main()
