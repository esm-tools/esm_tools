import argparse
import os

from .esm_cleanup import *


def evaluate_arguments():

    """The arg parser for interactive use"""
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-r", "--runscript", help="Name of the experiment's runscript", default=None
    )

    parser.add_argument(
        "-f",
        "--folder",
        help="Location to experiment's toplevel folder",
        default=".",
    )

    parser.add_argument(
        "-e",
        "--expid",
        help="Experiment name",
        default=None,
    )

    args = vars(parser.parse_args())

    runscript = None
    folder = "."

    if "runscript" in args and args["runscript"] and "expid" in args and args["expid"]:
        runscript_dict = read_in_yaml_file(args["runscript"])
        folder = runscript_dict["general"]["base_dir"] + "/" + args["expid"]
    elif "folder" in args:
        folder = args["folder"]
    else:
        print(
            "Either specify runscript and expid, or experiment folder to identify experiment to clean up."
        )
        sys.exit(-1)

    if folder == ".":
        folder = os.getcwd()

    return folder


def main_loop(folder):
    saved_space = 0
    while True:
        print_folder_content(folder, saved_space)
        action = ask_for_action(folder)

        if action == "remove_subfolder":
            saved_space = remove_subfolder(folder, saved_space)
        elif action == "remove_run_subfolders":
            saved_space = remove_run_subfolders(folder, saved_space)
        elif action == "remove_some_files":
            saved_space = remove_some_files(folder, saved_space)
        else:
            sys.exit(0)


def main():

    print_disclaimer()
    orgfolder = evaluate_arguments()

    while True:
        folder = orgfolder
        while not is_experiment_folder(folder):
            answer = pick_experiment_folder(folder)
            if answer == folder:
                break
            folder = answer

        if assert_question(f"Continue to work on folder {folder}?"):
            break

    check_if_folder_exists(folder)
    main_loop(folder)
