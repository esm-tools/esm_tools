"""
Functionality for displaying the version number
"""

import sys

import emoji
import rich_click as click
from click_repl import repl as crepl

import esm_cleanup.cli
import esm_tools
import esm_utilities.cli

EPILOGUE = """The ESM-Tools Developer Team, 2017-2024"""
VERSION = esm_tools.__version__


@click.group(
    help="""Tools for downloading, compiling, running, analyzing, and visualizing
    simulation experiments with Earth System Models.""",
    epilog=EPILOGUE,
)
@click.version_option(
    version=VERSION,
    prog_name="esm-tools",
    message=emoji.emojize(
        "%(prog)s :toolbox: v%(version)s\nRunning with Python :snake: v"
        + sys.version.replace("\n", " ")
    ),
)
def main(args=None):
    """Main Command Line Interface entry point"""
    return 0


@main.command()
def repl():
    """Start an interactive shell"""
    custom_kwargs = {
        "message": "esm-tools > ",
        "prompt_continuation": "... ",
    }
    crepl(click.get_current_context(), prompt_kwargs=custom_kwargs)


main.add_command(esm_utilities.cli.main, name="utils")
main.add_command(esm_cleanup.cli.main, name="clean")

if __name__ == "__main__":
    sys.exit(main())
