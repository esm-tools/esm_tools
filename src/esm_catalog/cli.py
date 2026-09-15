"""esm-catalog command-line interface.

Workflow for one experiment::

    esm-catalog auth login https://stac.awi.de   # once; token cached locally
    esm-catalog init  <exp_root>                 # set up <exp_root>/catalog/
    esm-catalog scan                             # write stac-geoparquet shards
    esm-catalog push                             # ship new shards -> pgstac

On disk, ``<exp_root>/catalog/`` holds the catalog PFS-friendly: one
``collection.json`` plus sharded stac-geoparquet (a handful of files, never one
JSON per item), and an ``esm-catalog.json`` workspace-state file (server,
catalog id, which shards are already pushed). ``push`` bulk-loads new shards into
the server's pgstac, which stac-fastapi-pgstac then serves to the web viewer.
"""

from __future__ import annotations

from pathlib import Path

import rich_click as click

from esm_catalog import __version__


def _not_implemented(command: str) -> None:
    """Fail cleanly: *command* is scaffolded but its logic is not written yet."""
    raise click.ClickException(f"'{command}' is not implemented yet.")


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
def init(exp_root: Path, server: str | None) -> None:
    """Set up <EXP_ROOT>/catalog/ (collection.json + workspace state)."""
    _not_implemented("init")


@main.command()
@click.option(
    "--exp-root",
    type=click.Path(file_okay=False, path_type=Path),
    help="Experiment root; defaults to the inited workspace.",
)
def scan(exp_root: Path | None) -> None:
    """Walk the experiment and write stac-geoparquet item shards."""
    _not_implemented("scan")


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
