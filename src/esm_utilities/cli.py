"""Console script for esm_utilities."""

import sys

import rich_click as click

from . import utils


@click.group()
def main():
    """Various utility functions for esm-tools produced simulations."""
    return 0


@main.command()
@click.argument(
    "log-file",
    type=click.Path(exists=True),
)
def logfile_stats(log_file):
    """Determines simulation throughput, queue time, and compute time

    LOG_FILE is the path to a simulation log file, typically
    found in the log folder.

    """
    utils.logfile_stats(log_file)


if __name__ == "__main__":
    sys.exit(main())  # pragma: no cover
