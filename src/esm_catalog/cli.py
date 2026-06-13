"""esm-catalog command-line interface.

This is the scaffold entry point. Subcommands (scan, serve, ...) are added by
subsequent feature PRs.
"""

from __future__ import annotations

import json
from pathlib import Path

import click

from esm_catalog import __version__


@click.group()
@click.version_option(version=__version__, prog_name="esm-catalog")
def main() -> None:
    """ESM-Tools simulation catalog."""


@main.command()
@click.argument("path", type=click.Path(exists=True))
@click.option("--output", "-o", type=click.Path(),
              help="Write the STAC catalog JSON here (default: stdout).")
@click.option("--db", type=click.Path(),
              help="Persist the catalog to a DuckDB file at this path.")
def scan(path: str, output: str | None, db: str | None) -> None:
    """Scan a run directory and emit an in-memory STAC catalog as JSON."""
    from esm_catalog.scan.ingest import scan_tree

    catalog = scan_tree(Path(path))

    if db:
        from esm_catalog.storage.duckdb import persist_tree
        persist_tree(catalog, Path(db))
        click.echo(f"Persisted {len(catalog['collections'])} collections, "
                   f"{len(catalog['items'])} items to {db}")

    if output:
        text = json.dumps(catalog, indent=2, default=str)
        Path(output).write_text(text)
        click.echo(f"Wrote {len(catalog['collections'])} collections, "
                   f"{len(catalog['items'])} items to {output}")
    elif not db:
        click.echo(json.dumps(catalog, indent=2, default=str))


@main.command()
@click.argument("shards", nargs=-1, required=True, type=click.Path(exists=True))
@click.option("--output", "-o", required=True, type=click.Path(),
              help="Path to the global catalog .duckdb file (created if absent).")
def merge(shards: tuple[str, ...], output: str) -> None:
    """Merge per-experiment shard .duckdb files into a single global catalog.

    Each SHARD is a .duckdb file produced by 'esm-catalog scan --db'.
    Rows are upserted by ID so the command is safe to re-run.

    Example::

        esm-catalog merge exp-alpha/catalog.duckdb exp-beta/catalog.duckdb \\
            --output global.duckdb
    """
    from pathlib import Path as _Path

    from esm_catalog.storage.federation import merge_shards

    n_cols, n_items = merge_shards(list(shards), _Path(output))
    click.echo(
        f"Merged {len(shards)} shard(s) into {output}: "
        f"{n_cols} new collections, {n_items} new items."
    )


if __name__ == "__main__":
    main()
