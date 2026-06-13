"""esm-catalog command-line interface.

This is the scaffold entry point. Subcommands (scan, serve, ...) are added by
subsequent feature PRs.
"""

from __future__ import annotations

import click

from esm_catalog import __version__


@click.group()
@click.version_option(version=__version__, prog_name="esm-catalog")
def main() -> None:
    """ESM-Tools simulation catalog."""


import json as _json
from pathlib import Path as _Path


@main.command()
@click.argument("path", type=click.Path(exists=True))
@click.option("--output", "-o", type=click.Path(),
              help="Write the STAC catalog JSON here (default: stdout).")
def scan(path: str, output: str | None) -> None:
    """Scan a run directory and emit an in-memory STAC catalog as JSON."""
    from esm_catalog.scan.ingest import scan_tree

    catalog = scan_tree(_Path(path))
    text = _json.dumps(catalog, indent=2, default=str)
    if output:
        _Path(output).write_text(text)
        click.echo(f"Wrote {len(catalog['collections'])} collections, "
                   f"{len(catalog['items'])} items to {output}")
    else:
        click.echo(text)


if __name__ == "__main__":
    main()
