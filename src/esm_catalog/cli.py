"""esm-catalog command-line interface.

Workflow for one experiment::

    esm-catalog auth login https://stac.awi.de   # once; token cached locally
    esm-catalog scan                             # write stac-geoparquet shards
    esm-catalog validate-cmip6                   # check any declared cmip6:* facets, if present
    esm-catalog push                             # ship new shards -> pgstac
    esm-catalog status                           # what's local, what's configured

A large scan is distributed across SLURM instead::

    esm-catalog distributed render-scripts VARS_FILE   # sched/worker/driver/cleanup .sbatch
    esm-catalog scan --distributed --scheduler tcp://...  # what the driver script runs

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

import rich_click as click

from esm_catalog import __version__

_CONFIG_EPILOG = (
    "Configuration (identity-provider settings, a default server_url) comes "
    "from environment variables prefixed 'ESM_CATALOG_' (e.g. "
    "ESM_CATALOG_SERVER_URL) or a config file — run 'esm-catalog status' to "
    "see what is currently resolved and where the config file would live."
)

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
            in_progress = event.phase == "reading" and event.current < event.total
            if event.phase == last["phase"] and in_progress and now - last["at"] < 30:
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
    "command supports it (currently: push, auth login). Unsupported commands "
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
    "on every 'push' against this server.",
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
        click.secho(
            "Note: no refresh token returned — you will re-login when it expires.",
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


@main.command()
@click.argument(
    "paths",
    nargs=-1,
    required=True,
    type=click.Path(exists=True, path_type=Path),
)
@click.option("--server", default=None, help="Target STAC server (overrides config).")
@click.option(
    "-k",
    "--insecure",
    is_flag=True,
    help="Skip TLS verification (dev self-signed). Not persisted — pass it again "
    "on 'auth login' and on every 'push' against this server.",
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
def push(
    ctx: click.Context,
    paths: tuple[Path, ...],
    server: Optional[str],
    insecure: bool,
    verbose: bool,
    resolve_specs: tuple[str, ...],
) -> None:
    """Push STAC objects to the catalog.

    Each PATH is a Collection/Item JSON, a stac-geoparquet shard, or a directory
    of them. Writes are authenticated (run 'auth login' first) and idempotent
    (upsert) — re-pushing is safe, nothing is deleted.
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
        if json_output:
            count = len(json.loads(delta.read_text()).get("properties", {}))
            unregistered_queryables.append(
                {"directory": str(directory), "count": count, "delta_path": str(delta)}
            )
        else:
            _report_new_queryables(delta, settings.server_url)

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


def _report_new_queryables(delta_path: Path, server_url: Optional[str]) -> None:
    """Print the operator recipe for registering new queryables."""
    count = len(json.loads(delta_path.read_text()).get("properties", {}))
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
    """Show the local catalog's state and the configured push target.

    Reports what a scan has produced on disk (shards, item counts, incremental
    bookkeeping) and where 'push' would send it. Does not contact the server —
    'push' itself is the only thing that knows what has actually been shipped,
    since nothing local tracks push history.
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
        click.echo(f"push target: {server_url}")
        from esm_catalog.auth import load_token

        click.echo(
            "logged in: " + ("yes" if load_token(server_url) is not None else "no")
        )
    else:
        click.secho(
            "push target: not configured (set server_url or ESM_CATALOG_SERVER_URL)",
            fg="yellow",
        )


@main.command("validate-cmip6")
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


@main.group()
def distributed() -> None:
    """Render the SLURM + Dask + Singularity pipeline for a large scan.

    'esm-catalog scan --distributed --scheduler tcp://...' attaches to an
    already-running Dask scheduler; it does not create one. This group
    renders the SLURM scripts that do -- see 'render-scripts --help'.
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
