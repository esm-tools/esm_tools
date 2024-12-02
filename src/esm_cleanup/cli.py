#!/usr/bin/env python3
import sys

import rich_click as click


@click.command()
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
