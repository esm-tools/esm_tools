"""Console script for esm_utilities."""
import sys
import click
from click_help_colors import HelpColorsGroup, HelpColorsCommand

from . import utils


@click.group(
        cls=HelpColorsGroup,
        help_headers_color="yellow",
        help_options_color="green",)
def main():
    """Various utility functions for esm-tools produced simulations."""
    return 0


@main.command(
        cls=HelpColorsCommand,
        help_headers_color="yellow",
        help_options_color="green",)
@click.argument("log-file",
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
