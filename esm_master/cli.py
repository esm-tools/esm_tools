"""Console script for esm_master."""
import argparse
import sys


#import logging
#logging.basicConfig(level=logging.DEBUG)
check = False
verbose = 0

from .esm_master import *
from . import database_actions

def main():

    #global check, verbose

    parser = argparse.ArgumentParser(
        prog="esm_master",
        description="tool for downloading, configuring and compiling.",
    )
    parser.add_argument(
        "target",
        metavar="target",
        nargs="?",
        type=str,
        help="name of the target (leave empty for full list of targets)",
    )
    parser.add_argument(
        "--check",
        "-c",
        action="store_true",
        default=False,
        help="show what would be done, not doing anything",
    )
    parser.add_argument(
        "--verbose", "-v", action="count", default=0, help="toggle verbose mode"
    )
    parser.add_argument(
        "--version", action="version", version="%(prog)s 3.0 (Oct 01, 2019)"
    )
    parser.add_argument(
            "--keep-task-script", "-k", dest="keep", action="store_true", default=False,
            help="Keep shell script generated to perform compilation/configuration jobs"
            )
    parsed_args = vars(parser.parse_args())

    check = False
    verbose = 0
    target = ""

    if parsed_args:
        if "target" in parsed_args:
            target = parsed_args["target"]
        if "check" in parsed_args:
            check = parsed_args["check"]
        if "verbose" in parsed_args:
            verbose = parsed_args["verbose"]
        if "keep" in parsed_args:
            keep = parsed_args["keep"]

    if not target:
        target = ""

    main_infos = general_infos()
    vcs = version_control_infos()
    setups2models = setup_and_model_infos(vcs, main_infos)


    setups2models.config = setups2models.reduce(target)

    user_config = setups2models.write_minimal_user_config()

    from esm_runscripts.esm_sim_objects import SimulationSetup
    complete_setup = SimulationSetup(user_config = user_config)
    complete_config = complete_setup.config

    env = esm_environment.environment_infos("compiletime", complete_config)

    setups2models.replace_last_vars(env)

    user_task = task(target, setups2models, vcs, main_infos)
    if verbose > 0:
        user_task.output()

    user_task.output_steps()

    if check:
        return 0
    user_task.validate()
    env.write_dummy_script()

    user_task.execute(env)
    database = database_actions.database_entry(user_task.todo, user_task.package.raw_name, ESM_MASTER_DIR)
    database.connection.close()

    if not keep:
        env.cleanup_dummy_script()
        user_task.cleanup_script()

    return 0


if __name__ == "__main__":
    main(sys.argv[1:])
