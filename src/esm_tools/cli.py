"""
Functionality for displaying the version number
"""

import sys

import click
import emoji
from click_help_colors import (HelpColorsCommand, HelpColorsGroup,
                               version_option)
from click_repl import register_repl

from . import __version__

EPILOGUE = """The ESM-Tools Developer Team, 2017-2022"""

import esm_tests
import esm_tools

# click.version_option read the PKG_INFO which contains the wrong version
# number. Get it directly from __init__.py
version = esm_tools.__version__


@click.group(
    cls=HelpColorsGroup,
    help_headers_color="yellow",
    help_options_color="green",
    help="""Tools for downloading, compiling, running, analyzing, and visualizing
    simulation experiments with Earth System Models.""",
    epilog=EPILOGUE,
)
@version_option(
    version=__version__,
    prog_name="esm-tools",
    message=emoji.emojize(
        "%(prog)s, version %(version)s\nRunning with Python :snake: v"
        + sys.version.replace("\n", " ")
    ),
    version_color="green",
)
def main(args=None):
    """Main Command Line Interface entry point"""
    return 0


# Needs to happen after definitions:
import esm_cleanup.cli
import esm_utilities.cli

main.add_command(esm_utilities.cli.main, name="utils")
main.add_command(esm_cleanup.cli.main, name="clean")
register_repl(main)

if __name__ == "__main__":
    sys.exit(main())
