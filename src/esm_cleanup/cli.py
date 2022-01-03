#!/usr/bin/env python3
import sys

import click
from click_help_colors import (HelpColorsCommand)

@click.command(cls=HelpColorsCommand,
        help_headers_color="yellow",
        help_options_color="green",
        )
@click.option(
    "-f",
    "--folder",
    type=click.Path(exists=True),
    default=".",
    help="Top level directory of the experiment to cleanup. Defaults to the current directory.",
)
@click.option(
    "--runscript",
    type=click.Path(exists=True),
    default=None,
    help="Path to the experiment YAML configuration",
)
@click.option(
    "-e",
    "--expid",
    help="Experiment name",
    default=None,
)
def main(folder=".", runscript=None, expid=None):
    """Cleanup an experiment"""
    return 0


if __name__ == "__main__":
    sys.exit(main())
