#!/usr/bin/env python
"""
A small wrapper that combines the shell interface and the Python interface
"""

# Import from Python Standard Library
import argparse
from loguru import logger

from .initialization import *
from .tests import *
from .test_utilities import *
from .read_shipped_data import *

import os
import sys


def main():
    # Logger
    logger.remove()
    logger.add(
        sys.stderr,
        filter={"": "WARNING", "esm_tests": "DEBUG"},
        format="<level>{message}</level>",
    )
    if os.environ.get("CI", False):
        logger.add(
            "out.log",
            filter={"": "WARNING", "esm_tests": "DEBUG"},
            backtrace=True,
            diagnose=True,
        )

    # Parsing
    parser = argparse.ArgumentParser(description="Automatic testing for ESM-Tools devs")
    parser.add_argument(
        "-n",
        "--no-user",
        default=False,
        help="Avoid loading user config",
        action="store_true",
    )
    parser.add_argument(
        "-c",
        "--check",
        default=False,
        help="Check mode on (does not compile or run, but produces some files that can "
        + "be compared to previous existing files in 'last_tested' folder)",
        action="store_true",
    )
    parser.add_argument(
        "-d",
        "--delete",
        default=False,
        help="Delete previous tests",
        action="store_true",
    )
    # parser.add_argument(
    #    "-k",
    #    "--keep",
    #    default=False,
    #    help="Keep run_, outdata and restart folders for runs",
    #    action="store_true",
    # )
    parser.add_argument(
        "-s",
        "--save",
        default="Not defined",
        help="Save files for comparisson in 'last_tested' folder",
    )
    parser.add_argument(
        "-t",
        "--state",
        default=False,
        help="Print the state stored in state.yaml",
        action="store_true",
    )
    parser.add_argument(
        "-o",
        "--hold",
        default=False,
        help="Hold before operation, to give time to check the output",
        action="store_true",
    )
    parser.add_argument(
        "-b",
        "--bulletpoints",
        default=False,
        help="bullet points for printing the results",
        action="store_true",
    )

    args = vars(parser.parse_args())

    save_flag = args["save"]
    print_state = args["state"]
    delete_tests = args["delete"]

    info = {}

    info["ignore_user_info"] = args["no_user"]
    info["actually_compile"] = not args["check"]
    info["actually_run"] = not args["check"]
    # info["keep_run_folders"] = args["keep"]
    info["hold"] = args["hold"]
    info["bulletpoints"] = args["bulletpoints"]

    info["script_dir"] = os.path.join(os.path.dirname(os.path.realpath(__file__)), ".")
    info["last_tested_dir"] = get_last_tested_dir()
    info["this_computer"] = (
        determine_computer_from_hostname().split("/")[-1].replace(".yaml", "")
    )

    # Predefined for later
    user_scripts = dict(comp={}, run={})

    # Print state if necessary
    if print_state:
        current_state = get_state_yaml()
        print_results(current_state, info)
        sys.exit(1)

    # Get user info for testing
    info = user_config(info)

    # Define lines to be ignored during comparison
    try:
        info["ignore"] = get_ignore_compare_yaml()
    except FileNotFoundError as e:
        print("Whoops, that did not work... I was looking here:")
        print(f"{info['script_dir']}/ignore_compare.yaml")
        for f in os.listdir(info["script_dir"]):
            print(f)
        print(e)
        raise

    logger.debug(f"User info: {info.get('user')}")
    logger.debug(f"Actually compile: {info.get('actually_compile')}")
    logger.debug(f"Actually run: {info.get('actually_run')}")

    # Gather scripts
    info = get_scripts(info)

    # Complete scripts_info
    info = read_info_from_rs(info)

    # Delete previous test
    if delete_tests:
        del_prev_tests(info)

    # Compile
    comp_test(info)

    # Run
    run_test(info)

    # Print results
    print_results(format_results(info), info)

    # Save files
    if save_flag == "Not defined":
        save_files(info, False)
    elif save_flag == "true" or save_flag == "True":
        save_files(info, True)

    # yprint(info["scripts"])
