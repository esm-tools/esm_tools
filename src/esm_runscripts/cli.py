#!/usr/bin/env python
"""
A small wrapper that combines the shell interface and the Python interface
"""
from . import event_handlers

event_handlers.signal_listener()

# Import from Python Standard Library
import os
import sys

import click
from loguru import logger

from esm_motd import check_all_esm_packages
from esm_tools import user_error

from .logfiles import SmartSink, initialize_logging
from .sim_objects import *
from .simulation_config import ConfigBuilder


@click.command(context_settings={"help_option_names": ["-h", "--help"]})
@click.argument("runscript")
@click.option(
    "-d", "--debug", is_flag=True, default=False,
    help="Print lots of debugging statements",
)
@click.option(
    "--trace", is_flag=True, default=False,
    help="Print even more debugging statements (trace level, most of it for esm_parser)",
)
@click.option(
    "--task-log-files", "task_log_files", is_flag=True, default=False,
    help="Do not write task-specific log files",
)
@click.option(
    "-v", "--verbose", is_flag=True, default=False,
    help="Be verbose",
)
@click.option(
    "--contained-run", "contained_run", is_flag=True, default=None,
    help="Run in a virtual environment",
)
@click.option(
    "--open-run", "open_run", is_flag=True, default=None,
    help="Run in default install (not in virtual environment)",
)
@click.option(
    "-e", "--expid", default="test",
    help="The experiment ID to use",
)
@click.option(
    "-c", "--check", is_flag=True, default=False,
    help="Run in check mode (don't submit job to supercomputer)",
)
@click.option(
    "-P", "--profile", is_flag=True, default=None,
    help="Write profiling information (esm-tools)",
)
@click.option(
    "-m", "--modify-config", "modify_config_file", default=None,  # kh 15.07.20 "usermods.yaml"
    help="[m]odify configuration",
)
@click.option(
    "-j", "--last-jobtype", "last_jobtype", default="command_line",
    help="Write the jobtype this run was called from (esm-tools internal)",
)
@click.option(
    "-t", "--task", default="unknown",
    help="The task to run. Choose from: prepcompute, post, couple, tidy",
)
@click.option(
    "-i", "--inspect", default=None,
    help="Show some information, choose a keyword from 'overview', 'namelists'",
)
@click.option(
    "-p", "--pid", "launcher_pid", type=int, default=-666,
    help="The PID of the task to observe.",
)
@click.option(
    "-s", "--start_date", "current_date", default=None,
    help="The start_date of the run, overwriting settings in the date file.",
)
@click.option("-x", "--exclude", default=None, help="e[x]clude this step")
@click.option("-o", "--only", default=None, help="[o]nly do this step")
@click.option(
    "-r", "--run_number", default=None,
    help="run_number for this run, overwriting settings in date file",
)
# PG: Might not work anymore:
@click.option(
    "-U", "--update", is_flag=True, default=False,
    help="[U]date the tools from the current version and the runscript",
)
@click.option(
    "--update-filetypes", "update_filetypes", multiple=True, default=(),
    help=(
        "Updates the requested files from external sources in a currently ongoing "
        "simulation. We strongly advise against using this option unless you "
        "really know what you are doing."
    ),
)
@click.option(
    "--no-motd", "no_motd", is_flag=True, default=False,
    help="supress the printing of MOTD",
)
@click.option(
    "--ignore-config-warnings", "ignore_config_warnings", is_flag=True, default=False,
    help="do not halt in warnings defined in the config files",
)
@click.option(
    "--wf-engine", "wf_engine",
    type=click.Choice(["legacy", "pplan"]), default="legacy",
    help=(
        "Workflow engine to use. 'legacy' (default) runs the job as usual. "
        "'pplan' assembles the workflow, snapshots it into the pydantic "
        "Plan/SubPlan/Workflow view, prints it, and drops into a debug "
        "session instead of submitting anything."
    ),
)
def main(**command_line_config):
    """esm_runscripts: build a simulation's config, then run or debug it."""
    logger.add(sys.stdout, level="WARNING", format="{message}")

    command_line_config["jobtype"] = command_line_config["task"]
    command_line_config["use_venv"] = _resolve_use_venv(command_line_config)
    command_line_config["motd"] = not command_line_config.pop("no_motd")
    command_line_config["original_command"] = " ".join(sys.argv[1:]).strip()

    _resolve_runscript_paths(command_line_config)
    initialize_logging(command_line_config)

    config = _build_config(command_line_config)
    sim = Simulation.from_config(config)

    if command_line_config["wf_engine"] == "pplan":
        drop_into_pplan_debug(sim.plan, sim.config)
        return

    if not sim.config["general"]["submitted"] and command_line_config["motd"]:
        check_all_esm_packages()
    sim.run()


def _resolve_use_venv(command_line_config):
    """Reconcile ``--contained-run``/``--open-run`` into a single ``use_venv``
    flag, or exit if both were given (they contradict each other)."""
    if command_line_config["contained_run"] and command_line_config["open_run"]:
        logger.error(
            "You have set both --contained-run and --open-run, this makes no sense."
        )
        logger.error(command_line_config)
        sys.exit(1)

    use_venv = None
    if command_line_config["contained_run"] is not None:
        use_venv = command_line_config["contained_run"]
    if command_line_config["open_run"] is not None:
        use_venv = not command_line_config["open_run"]
    return use_venv


def _resolve_runscript_paths(command_line_config):
    """Resolve the runscript argument to an absolute path, split it into
    directory/filename, and store both plus the full path back into
    ``command_line_config`` -- mutates in place, matching every other
    ``*_config`` builder step in this module."""
    runscript_full_path = os.path.realpath(command_line_config["runscript"])
    runscript_dir, runscript = os.path.split(runscript_full_path)
    runscript_dir += "/"
    if not os.path.exists(runscript_full_path):
        user_error(
            "runscript not found",
            f"The runscript ``{command_line_config['runscript']}`` does not "
            f"exists in folder ``{runscript_dir}``. ",
            dsymbols=["``", "'"],
        )

    # this might contain the relative path but it will be taken care of later
    command_line_config["started_from"] = runscript_dir
    # only the yaml file, without the path
    command_line_config["scriptname"] = runscript
    # full path including the yaml file: runscript_dir + runscript
    command_line_config["runscript_abspath"] = runscript_full_path


def _build_config(command_line_config):
    """Run the full ``ConfigBuilder`` chain for a CLI invocation."""
    return (
        ConfigBuilder.from_runscript(
            command_line_config["runscript_abspath"], **command_line_config
        )
        .with_interactive_info()
        .with_iterative_coupling()
        .with_execution_mode_default()
        .expand_from_esm_tools_configs()
        .with_runscript_defaults()
        .validate_account()
        .with_inspect_info()
        .remember_command_line()
        .build()
    )


def drop_into_pplan_debug(workflow, config):
    """Print the resolved ``Workflow`` that ``Simulation.from_config`` built
    for this job type, and drop into a debug session for poking around.

    Does not submit or run anything -- this is purely for exploring where
    recipe steps hook into the new ``plan.py`` models. Legacy
    ``prepare.run_job``/``assemble_workflow`` never ran to get here (see
    ``Simulation.from_config``'s ``wf_engine`` branch); ``workflow`` is
    built natively by ``plan.Workflow.resolve`` instead -- there is no
    separate ``SubPlan("prepare")`` stub anymore, ``workflow.sub_plans``
    holds every phase (e.g. ``prepcompute``, ``compute``, ``tidy``)
    directly.
    """
    print(workflow)
    for sub_plan in workflow.sub_plans.values():
        print(" ", sub_plan)
        for step in sub_plan.steps:
            print("   ", step)

    for cluster in workflow.plans.values():
        print(" ", cluster)

    try:
        from IPython import embed

        embed(
            header="esm_runscripts --wf-engine=pplan debug session\n"
            "`workflow` is the resolved Workflow; `config` is its config."
        )
    except ImportError:
        logger.warning("IPython not available, falling back to pdb")
        import pdb

        pdb.set_trace()
