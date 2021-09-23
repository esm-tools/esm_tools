#!/usr/bin/env python
"""
A small wrapper that combines the shell interface and the Python interface
"""

# Import from Python Standard Library
from .esm_tests import *

def main():

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
    parser.add_argument(
        "-k",
        "--keep",
        default=False,
        help="Keep run_, outdata and restart folders for runs",
        action="store_true",
    )
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

    info = {}

    args = vars(parser.parse_args())
    ignore_user_info = args["no_user"]
    info["actually_compile"] = not args["check"]
    info["actually_run"] = not args["check"]
    delete_tests = args["delete"]
    info["keep_run_folders"] = args["keep"]
    save_flag = args["save"]
    print_state = args["state"]

    info["script_dir"] = os.path.join(os.path.dirname(os.path.realpath(__file__)), "..")
    info["last_tested_dir"] = f"{info['script_dir']}/last_tested/"
    info["this_computer"] = (
        determine_computer_from_hostname().split("/")[-1].replace(".yaml", "")
    )

    # Predefined for later
    user_scripts = dict(comp={}, run={})

    # Print state if necessary
    if print_state:
        with open(f"{info['script_dir']}/state.yaml", "r") as st:
            current_state = yaml.load(st, Loader=yaml.FullLoader)
        print_results(current_state)
        sys.exit(1)

    # Get user info for testing
    if not ignore_user_info:
        info["user"] = user_config(info)
    else:
        info["user"] = None

    # Define lines to be ignored during comparison
    with open(f"{info['script_dir']}/ignore_compare.yaml", "r") as i:
        info["ignore"] = yaml.load(i, Loader=yaml.FullLoader)

    logger.debug(f"User info: {info['user']}")
    logger.debug(f"Actually compile: {info['actually_compile']}")
    logger.debug(f"Actually run: {info['actually_run']}")

    # Gather scripts
    scripts_info = get_scripts(info)

    # Complete scripts_info
    scripts_info = read_info_from_rs(scripts_info)

    # Delete previous test
    if delete_tests:
        del_prev_tests(info, scripts_info)

    # Compile
    comp_test(scripts_info, info)

    # Run
    run_test(scripts_info, info)

    # Print results
    print_results(format_results(info, scripts_info))

    # Save files
    if save_flag == "Not defined":
        save_files(scripts_info, info, False)
    elif save_flag == "true" or save_flag == "True":
        save_files(scripts_info, info, True)

    # yprint(scripts_info)
