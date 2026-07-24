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

from .logfiles import SmartSink, initialize_logging
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
        "--task-log-files",
        help="Do not write task-specific log files",
        action="store_true",
        default=False,
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

    parser.add_argument(
        "--coupling-chain",
        help="concurrent iterative coupling: setup_name of the model chain this "
        "invocation drives (one esm_runscripts chain per model)",
        default=None,
        dest="coupling_chain",
    )

    return parser.parse_args()


def _fan_out_coupling_chains(parsed_args):
    """--coupling-chain all: chain doctor. Launch every model chain of the
    concurrent-coupling driver that is not already in the batch queue. Cold
    start, crash recovery and post-outage restart are all this one command."""
    import subprocess

    import yaml

    # runscripts carry esm-tools tags (!ENV ...) that SafeLoader rejects; we only
    # need the modelN setup_names, so resolve unknown tags to their raw value
    class _TolerantLoader(yaml.SafeLoader):
        pass

    def _keep_raw(loader, tag_suffix, node):
        if isinstance(node, yaml.ScalarNode):
            return loader.construct_scalar(node)
        if isinstance(node, yaml.SequenceNode):
            return loader.construct_sequence(node)
        return loader.construct_mapping(node)

    _TolerantLoader.add_multi_constructor("", _keep_raw)

    with open(os.path.realpath(parsed_args["runscript"])) as fid:
        driver = yaml.load(fid, Loader=_TolerantLoader)
    chains, index = [], 1
    while f"model{index}" in driver:
        chains.append(driver[f"model{index}"]["setup_name"])
        index += 1
    if not chains:
        user_error(
            "Concurrent iterative coupling",
            f"--coupling-chain all needs an iterative-coupling driver runscript "
            f"with model1/model2 blocks; ``{parsed_args['runscript']}`` has none.",
        )

    expid = parsed_args["expid"]
    try:
        # active states only: a COMPLETING/CANCELLED job still shows in squeue and
        # would make the guard skip a chain that is actually gone
        queued = subprocess.check_output(
            ["squeue", "-h", "-u", os.environ.get("USER", ""),
             "-t", "PENDING,RUNNING,CONFIGURING,SUSPENDED", "-o", "%j"],
            stderr=subprocess.DEVNULL,
        ).decode()
        queued_names = set(queued.split())
    except (OSError, subprocess.CalledProcessError):
        queued_names = set()
        logger.warning("chain doctor: squeue not available -- launching without guard")

    base_command, skip_next = [], False
    for arg in sys.argv:
        if skip_next:
            skip_next = False
            continue
        if arg == "--coupling-chain":
            skip_next = True
            continue
        if arg.startswith("--coupling-chain="):
            continue
        base_command.append(arg)

    for chain in chains:
        if f"{expid}_{chain}" in queued_names or f"{expid}_{chain}_launch" in queued_names:
            logger.info(f"chain doctor: {chain} already in the queue -- skipping")
            continue
        logger.info(f"chain doctor: launching chain {chain}")
        _launch_coupling_chain(base_command, chain, expid, driver)
    sys.exit(0)


def _launch_coupling_chain(base_command, chain, expid, driver):
    """Start one chain's esm_runscripts.

    A chain launch runs newrun/couple_in/prepcompute IN-PROCESS, and those
    coupling steps execute real binaries (dEBM, fesom_meshpart, OASIS weight
    regen). On a login node the site watchdog kills them, so unless we are
    already inside an allocation the launch goes through a small batch job.
    """
    import shlex
    import subprocess
    import tempfile

    command = " ".join(shlex.quote(a) for a in base_command + ["--coupling-chain", chain])

    if os.environ.get("SLURM_JOB_ID"):
        logger.info("  (inside an allocation -- running inline)")
        subprocess.call(base_command + ["--coupling-chain", chain])
        return

    general = driver.get("general", {})
    account = general.get("account", "")
    # exclusive node: couple_in runs dEBM + heavy cdo, which OOM on a shared core
    partition = general.get("coupling_launcher_partition", "compute")
    walltime = general.get("coupling_launcher_time", "00:30:00")
    logfile = os.path.join(os.getcwd(), f"{expid}_{chain}_launch_%j.log")

    script = (
        "#!/bin/bash -l\n"
        f"#SBATCH --job-name={expid}_{chain}_launch\n"
        + (f"#SBATCH --account={account}\n" if account else "")
        + f"#SBATCH --partition={partition}\n"
        "#SBATCH --nodes=1\n"
        "#SBATCH --exclusive\n"
        f"#SBATCH --time={walltime}\n"
        f"#SBATCH --output={logfile}\n"
        f"cd {shlex.quote(os.getcwd())}\n"
        f"{command}\n"
    )
    with tempfile.NamedTemporaryFile(
        "w", suffix=f"_{expid}_{chain}.sbatch", delete=False
    ) as fid:
        fid.write(script)
        sbatch_file = fid.name

    try:
        out = subprocess.check_output(
            ["sbatch", "--export=ALL", sbatch_file], stderr=subprocess.STDOUT
        ).decode().strip()
        logger.info(f"  {out} (launcher job; chain log -> {logfile})")
    except (OSError, subprocess.CalledProcessError) as error:
        logger.warning(
            f"  sbatch unavailable/failed ({error}); running inline -- note that "
            f"coupling binaries may be killed on a login node"
        )
        subprocess.call(base_command + ["--coupling-chain", chain])


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

    if parsed_args.get("coupling_chain") == "all":
        _fan_out_coupling_chains(parsed_args)

    # this might contain the relative path but it will be taken care of later
    command_line_config["original_command"] = original_command.strip()
    command_line_config["started_from"] = runscript_dir

    # only the yaml file, without the path
    command_line_config["scriptname"] = runscript
    # full path including the yaml file: runscript_dir + runscript
    command_line_config["runscript_abspath"] = runscript_full_path

    initialize_logging(command_line_config)

    setup = SimulationSetup(command_line_config=command_line_config)
    # if not Setup.config['general']['submitted']:
    if not setup.config["general"]["submitted"] and motd:
        check_all_esm_packages()
    setup()
