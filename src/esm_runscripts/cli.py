#!/usr/bin/env python
"""
A small wrapper that combines the shell interface and the Python interface
"""
from . import event_handlers

event_handlers.signal_listener()

# Import from Python Standard Library
import argparse
import os
import sys

from loguru import logger

from esm_motd import check_all_esm_packages
from esm_tools import user_error

from .helpers import SmartSink
from .sim_objects import *


def parse_shargs():
    """The arg parser for interactive use"""
    parser = argparse.ArgumentParser()
    parser.add_argument("runscript", default=None)

    parser.add_argument(
        "-d",
        "--debug",
        help="Print lots of debugging statements",
        action="store_true",
        default=False,
    )

    parser.add_argument(
        "--trace",
        help="Print even more debugging statements (trace level, most of it for esm_parser)",
        action="store_true",
        default=False,
    )

    parser.add_argument(
        "--no-task-log-files",
        help="Do not write task-specific log files",
        action="store_false",
        default=True,
        dest="task_log_files",
    )

    parser.add_argument(
        "-v",
        "--verbose",
        help="Be verbose",
        action="store_true",
        default=False,
    )

    parser.add_argument(
        "--contained-run",
        help="Run in a virtual environment",
        action="store_true",
        default=None,
    )

    parser.add_argument(
        "--open-run",
        help="Run in default install (not in virtual environment)",
        action="store_true",
        default=None,
    )

    parser.add_argument(
        "-e", "--expid", help="The experiment ID to use", default="test"
    )

    parser.add_argument(
        "-c",
        "--check",
        help="Run in check mode (don't submit job to supercomputer)",
        default=False,
        action="store_true",
    )

    parser.add_argument(
        "-P",
        "--profile",
        help="Write profiling information (esm-tools)",
        default=None,
        action="store_true",
    )

    parser.add_argument(
        "--modify-config",
        "-m",
        dest="modify_config_file",
        help="[m]odify configuration",
        default=None,  # kh 15.07.20 "usermods.yaml"
    )

    parser.add_argument(
        "-j",
        "--last-jobtype",
        help="Write the jobtype this run was called from (esm-tools internal)",
        default="command_line",
    )

    parser.add_argument(
        "-t",
        "--task",
        help="The task to run. Choose from: prepcompute, post, couple, tidy",
        default="unknown",
    )

    parser.add_argument(
        "-i",
        "--inspect",
        help="Show some information, choose a keyword from 'overview', 'namelists'",
        default=None,
    )

    parser.add_argument(
        "-p",
        "--pid",
        help="The PID of the task to observe.",
        dest="launcher_pid",
        default=-666,
    )

    parser.add_argument(
        "-s",
        "--start_date",
        help="The start_date of the run, overwriting settings in the date file.",
        dest="current_date",
        default=None,
    )

    parser.add_argument("-x", "--exclude", help="e[x]clude this step", default=None)
    parser.add_argument("-o", "--only", help="[o]nly do this step", default=None)
    parser.add_argument(
        "-r",
        "--run_number",
        help="run_number for this run, overwriting settings in date file",
        default=None,
    )

    # PG: Might not work anymore:
    parser.add_argument(
        "-U",
        "--update",
        help="[U]date the tools from the current version and the runscript",
        default=False,
        action="store_true",
    )

    parser.add_argument(
        "--update-filetypes",
        help="Updates the requested files from external sources in a currently ongoing "
        "simulation. We strongly advise against using this option unless you "
        "really know what you are doing.",
        nargs="+",
        default=[],
    )

    parser.add_argument(
        "--no-motd",
        help="supress the printing of MOTD",
        default=True,
        dest="motd",
        action="store_false",
    )

    parser.add_argument(
        "--ignore-config-warnings",
        help="do not halt in warnings defined in the config files",
        default=False,
        action="store_true",
    )

    return parser.parse_args()


def main():
    logger.add(sys.stdout, level="WARNING", format="{message}")

    ARGS = parse_shargs()
    parsed_args = vars(ARGS)

    jobtype = parsed_args["task"]
    verbose = parsed_args["verbose"]
    debug = parsed_args["debug"]
    trace = parsed_args["trace"]
    task_log_files = parsed_args["task_log_files"]
    motd = parsed_args["motd"]

    use_venv = None
    if parsed_args["contained_run"] is not None:
        use_venv = parsed_args["contained_run"]
    if parsed_args["open_run"] is not None:
        use_venv = not parsed_args["open_run"]

    original_command = " ".join(sys.argv[1:])

    if parsed_args["contained_run"] and parsed_args["open_run"]:
        logger.error(
            "You have set both --contained-run and --open-run, this makes no sense."
        )
        logger.error(parsed_args)
        sys.exit(1)

    command_line_config = parsed_args
    command_line_config["jobtype"] = jobtype
    command_line_config["use_venv"] = use_venv

    runscript_full_path = os.path.realpath(ARGS.runscript)
    runscript_dir, runscript = os.path.split(runscript_full_path)
    runscript_dir += "/"
    if not os.path.exists(runscript_full_path):
        user_error(
            "runscript not found",
            f"The runscript ``{ARGS.runscript}`` does not exists in folder ``{runscript_dir}``. ",
            dsymbols=["``", "'"],
        )

    # this might contain the relative path but it will be taken care of later
    command_line_config["original_command"] = original_command.strip()
    command_line_config["started_from"] = runscript_dir

    # only the yaml file, without the path
    command_line_config["scriptname"] = runscript
    # full path including the yaml file: runscript_dir + runscript
    command_line_config["runscript_abspath"] = runscript_full_path

    # Define a sink object to store the logs. Path of the logs can be later specified
    # by using <sink_obj>.def_path(<path>)
    #task_sink = SmartSink() #trace_sink
    #logger.task_sink = task_sink

    logger.remove()
    #logger.add(task_sink.sink, level="TRACE")

    # Redirect stdout to a SmartSink if the jobtype is observe_compute to avoid printing
    # in the slurm log at the same time that compute is printing.
    if command_line_config["jobtype"] in ["observe_compute"]:
        stdout_sink = SmartSink(print_in_stdout=False, task_log_files=task_log_files)
    else:
        stdout_sink = SmartSink(print_in_stdout=True, task_log_files=task_log_files)

    # Store the sink in the logger
    logger.stdout_sink = stdout_sink
    stdout = stdout_sink.sink

    # Set logging level based on the command line arguments
    if trace:
        logger.add(stdout, level="TRACE")
        logger.trace(f"Started from: {command_line_config['started_from']}")
        logger.trace(f"starting (jobtype): {jobtype}")
        logger.trace(command_line_config)
    elif verbose or debug:
        logger.add(stdout, level="DEBUG", format="{message}")
        logger.debug(f"Started from: {command_line_config['started_from']}")
        logger.debug(f"starting (jobtype): {jobtype}")
        logger.debug(command_line_config)
    else:
        logger.add(stdout, level="INFO", format="{message}")

    setup = SimulationSetup(command_line_config=command_line_config)
    # if not Setup.config['general']['submitted']:
    if not setup.config["general"]["submitted"] and motd:
        check_all_esm_packages()
    setup()
