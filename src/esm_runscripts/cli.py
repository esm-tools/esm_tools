#!/usr/bin/env python
"""
A small wrapper that combines the shell interface and the Python interface
"""
from . import event_handlers

event_handlers.signal_listener()

# Import from Python Standard Library
import os
import sys

import rich_click as click
from loguru import logger

from esm_motd import check_all_esm_packages
from esm_parser import user_error

from .helpers import SmartSink
from .sim_objects import SimulationSetup

# Configure rich_click styling
click.rich_click.USE_RICH_MARKUP = True
click.rich_click.GROUP_ARGUMENTS_OPTIONS = True
click.rich_click.SHOW_ARGUMENTS = True


@click.command()
@click.argument("runscript", type=click.Path(exists=True))
@click.option("-d", "--debug", help="Print lots of debugging statements", is_flag=True)
@click.option("-v", "--verbose", help="Be verbose", is_flag=True)
@click.option(
    "--contained-run/--open-run",
    help="Run in a virtual environment / Run in default install",
    default=None,
)
@click.option("-e", "--expid", help="The experiment ID to use", default="test")
@click.option(
    "-c",
    "--check",
    help="Run in check mode (don't submit job to supercomputer)",
    is_flag=True,
)
@click.option(
    "-P", "--profile", help="Write profiling information (esm-tools)", is_flag=True
)
@click.option("--modify-config", "-m", help="[m]odify configuration", default="")
@click.option(
    "-j",
    "--last-jobtype",
    help="Write the jobtype this run was called from (esm-tools internal)",
    default="command_line",
)
@click.option(
    "-t",
    "--task",
    help="The task to run. Choose from: prepcompute, post, couple, tidy",
    default="unknown",
)
@click.option(
    "-i",
    "--inspect",
    help="Show some information, choose a keyword from 'overview', 'namelists'",
)
@click.option(
    "-p", "--pid", help="The PID of the task to observe.", default=-666, type=int
)
@click.option(
    "-s",
    "--start-date",
    help="The start_date of the run, overwriting settings in the date file.",
)
@click.option("-x", "--exclude", help="e[x]clude this step")
@click.option("-o", "--only", help="[o]nly do this step")
@click.option(
    "-r",
    "--run-number",
    help="run_number for this run, overwriting settings in date file",
)
@click.option(
    "-U",
    "--update",
    help="[U]date the tools from the current version and the runscript",
    is_flag=True,
)
@click.option(
    "--update-filetypes",
    help="Updates the requested files from external sources in a currently ongoing simulation. "
    "We strongly advise against using this option unless you really know what you are doing.",
    multiple=True,
)
@click.option("--no-motd", help="supress the printing of MOTD", is_flag=True)
@click.option(
    "--ignore-config-warnings",
    help="do not halt in warnings defined in the config files",
    is_flag=True,
)
def main(
    runscript,
    debug,
    verbose,
    contained_run,
    expid,
    check,
    profile,
    modify_config,
    last_jobtype,
    task,
    inspect,
    pid,
    start_date,
    exclude,
    only,
    run_number,
    update,
    update_filetypes,
    no_motd,
    ignore_config_warnings,
):
    """ESM Tools Command Line Interface"""

    use_venv = contained_run
    if sys.argv[1] == "run":
        # NOTE(PG): The new interface allows for esm-tools run <everything else>. We need to throw that out.
        original_command = " ".join(sys.argv[2:])
    else:
        original_command = " ".join(sys.argv[1:])

    # Setup command line config dictionary
    command_line_config = {
        "check": check,
        "profile": profile,
        "update": update,
        "update_filetypes": update_filetypes,
        "expid": expid,
        "launcher_pid": pid,
        "current_date": start_date,
        "run_number": run_number,
        "jobtype": task,
        "last_jobtype": last_jobtype,
        "verbose": verbose,
        "inspect": inspect,
        "use_venv": use_venv,
        "no_motd": no_motd,
        "ignore_config_warnings": ignore_config_warnings,
    }

    if modify_config:
        command_line_config["modify_config_file"] = modify_config

    runscript_full_path = os.path.realpath(runscript)
    runscript_dir, runscript_name = os.path.split(runscript_full_path)
    runscript_dir += "/"

    if not os.path.exists(runscript_full_path):
        user_error(
            "runscript not found",
            f"The runscript ``{runscript}`` does not exists in folder ``{runscript_dir}``. ",
            dsymbols=["``", "'"],
        )

    command_line_config["original_command"] = original_command.strip()
    command_line_config["started_from"] = runscript_dir
    command_line_config["scriptname"] = runscript_name
    command_line_config["runscript_abspath"] = runscript_full_path

    # Define a sink object to store the logs
    trace_sink = SmartSink()
    logger.trace_sink = trace_sink

    logger.remove()
    logger.add(trace_sink.sink, level="TRACE")

    if verbose:
        logger.add(sys.stdout, level="DEBUG", format="{message}")
        logger.debug(f"Started from: {command_line_config['started_from']}")
        logger.debug(f"starting (jobtype): {task}")
        logger.debug(command_line_config)
    else:
        logger.add(sys.stdout, level="INFO", format="{message}")

    my_simulation = SimulationSetup(command_line_config=command_line_config)
    if not my_simulation.config["general"]["submitted"] and not no_motd:
        check_all_esm_packages()
    my_simulation()


if __name__ == "__main__":
    main()
