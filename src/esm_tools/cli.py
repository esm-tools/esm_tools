"""
Functionality for displaying the version number
"""

import sys

import emoji
import rich_click as click
from click_repl import repl as crepl

import esm_archiving.cli
import esm_cleanup.cli
import esm_database.cli
import esm_master
import esm_master.cli
import esm_plugin_manager.cli
import esm_tools
import esm_utilities.cli

VERSION = esm_tools.__version__
EPILOGUE = f"Version {VERSION}, The ESM-Tools Developer Team, 2017-2024"

click.rich_click.COMMAND_GROUPS = {
    "esm-tools": [
        {
            "name": "esm-master Commands",
            "commands": ["get", "conf", "comp"] + list(esm_master.get_meta_commands()),
            "help": "Commands for downloading, compiling, and installing Models",
            "table_styles": {"row_styles": ["red"]},
        },
        {
            "name": "esm-runscripts Commands",
            "commands": ["run", "clean", "archive"],
            "help": "Commands for running and cleaning up experiments",
            "table_styles": {"row_styles": ["green"]},
        },
        {
            "name": "Developer Utilities",
            "commands": ["list-plugins"],
            "help": "Commands for Developers",
            "table_styles": {"row_styles": ["blue"]},
        },
        {
            "name": "Utilities",
            "commands": ["logfile-stats"],
            "help": "General utilities",
            "table_styles": {"row_styles": ["magenta"]},
        },
        {
            "name": "Cleanup",
            "commands": ["clean"],
            "help": "Cleanup utilities",
            "table_styles": {"row_styles": ["yellow"]},
        },
    ],
}


@click.group(
    help="""Tools for downloading, compiling, running, analyzing, and visualizing
    simulation experiments with Earth System Models.""",
    epilog=EPILOGUE,
)
@click.version_option(
    version=VERSION,
    prog_name="esm-tools",
    message=emoji.emojize(
        "%(prog)s :toolbox: %(version)s\nRunning with Python :snake: "
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


main.add_command(esm_archiving.cli.main, name="archive")
main.add_command(esm_cleanup.cli.main, name="clean")
main.add_command(esm_master.cli.create_command("clean", "Clean compilation artifacts"))
main.add_command(esm_master.cli.create_command("comp", "Compile model"))
main.add_command(
    esm_master.cli.create_command("conf", "Configure code for compilation")
)
main.add_command(esm_master.cli.create_command("get", "Download model source code"))
main.add_command(esm_plugin_manager.cli.main, name="list-plugins")
main.add_command(esm_utilities.cli.logfile_stats, name="logfile-stats")


main.add_command(esm_master.cli.cli, name="master")
main.add_command(esm_utilities.cli.main, name="utilities")
main.add_command(esm_database.cli.main, name="database")

if __name__ == "__main__":
    sys.exit(main())
