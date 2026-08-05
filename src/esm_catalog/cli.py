"""esm-catalog command-line interface.

Subcommands (scan, serve, ...) are registered on the top-level group.
"""

from __future__ import annotations

import click

from esm_catalog import __version__


@click.group()
@click.version_option(version=__version__, prog_name="esm-catalog")
def main() -> None:
    """ESM-Tools simulation catalog."""


if __name__ == "__main__":
    main()
