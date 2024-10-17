#!/usr/bin/env python
"""
A small wrapper that combines the shell interface and the Python interface
"""

# Import from Python Standard Library
from loguru import logger

from .info import *
from .initialization import *
from .output import *
from .read_shipped_data import *
from .repos import *
from .test_utilities import *
from .tests import *

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

    logger.info("")
    logger.info("Welcome to ESM-Tests!")
    logger.info("=====================")
    logger.info("")

    if os.environ.get("CI", False):
        logger.add(
            "out.log",
            filter={"": "WARNING", "esm_tests": "DEBUG"},
            backtrace=True,
            diagnose=True,
        )

    # Info instancing
    info = Info()

    # Parsing
    save_flag, print_state, delete_tests = info.argparse()
    print(info["resources_branch"])

    # Update ``resources``
    update_resources_submodule(info)

    info["this_computer"] = (
        determine_computer_yaml_from_hostname().split("/")[-1].replace(".yaml", "")
    )
    info["last_tested_dir"] = get_last_tested_dir()

    # Predefined for later
    user_scripts = dict(comp={}, run={})

    # Print state if necessary
    if print_state:
        current_state = get_state_yaml()
        print_results(current_state, info)
        sys.exit(1)

    # Get user info for testing
    user_config(info)

    # User-specific info to remove from the files ``last_tested`` files
    info["rm_user_info"] = {
        "TEST_DIR": info["user"]["test_dir"],
        "HOME_DIR": f"{os.path.expanduser('~')}",
    }

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

    # Special actions for running from GitHub servers
    if info["in_github"]:
        info["rm_user_info"]["HOME_DIR"] = "/__w/esm_tools"
        # Ignore the globbing variables
        info["ignore"]["finished_config"].append("_glob_[0-9]*: ")

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
    results = format_results(info)
    print_results(results, info)

    # Check if all tests passed
    if info["system_exit_on_errors"]:
        check_perfect(info, results)

    # Save files
    if save_flag == "Not defined":
        save_files(info, False)
    elif save_flag == "true" or save_flag == "True":
        save_files(info, True)

    # yprint(info["scripts"])
