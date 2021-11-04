import filecmp
import os
import glob
import sys

from esm_parser import pprint_config
from .helpers import evaluate
from .prepcompute import _show_simulation_info
from .namelists import Namelist
from . import workflow


def run_job(config):
    config = evaluate(config, "inspect", "inspect_recipe")
    return config


def inspect_workflow(config):
    if config["general"]["inspect"] == "workflow":

        config = workflow.display_nicely(config)
        sys.exit(0)
    return config


def inspect_overview(config):
    if config["general"]["inspect"] == "overview":
        config = _show_simulation_info(config)
        sys.exit(0)
    return config


def inspect_namelists(config):
    if config["general"]["inspect"] == "namelists":
        for model in config["general"]["valid_model_names"]:
            config[model] = Namelist.nmls_load(config[model])
            config[model] = Namelist.nmls_output(config[model])
        sys.exit(0)
    return config


def inspect_config(config):
    if config["general"]["inspect"] == "config":
        pprint_config(config)
        sys.exit(0)
    return config


def inspect_size(config):
    if config["general"]["inspect"] == "size":
        total_size = dir_size(config["general"]["experiment_dir"])
        unit = "B"
        if total_size >= 1024:
            total_size = total_size / 1024.0
            unit = "kB"
        if total_size >= 1024:
            total_size = total_size / 1024.0
            unit = "MB"
        if total_size >= 1024:
            total_size = total_size / 1024.0
            unit = "GB"
        if total_size >= 1024:
            total_size = total_size / 1024.0
            unit = "TB"
        print(f"Total size: {total_size:.2f} {unit}")
        sys.exit(0)
    return config


def inspect_folder(config):
    checkpath = config["general"]["thisrun_dir"] + "/" + config["general"]["inspect"]
    if os.path.isdir(checkpath):
        all_files = os.listdir(checkpath)
        print(f"Files in folder {checkpath}:")
        for thisfile in sorted(all_files):
            print(f" -- {thisfile}")
        sys.exit(0)
    return config


def inspect_file(config):
    exclude = ["work"]
    knownfiles = {}
    search_dir = config["general"]["thisrun_dir"]
    if config["general"]["inspect"] == "lastlog":
        maybe_file = config["computer"]["thisrun_logfile"].replace(
            "@jobtype@", "compute"
        )
    elif config["general"]["inspect"] == "explog":
        maybe_file = (
            f"{config['general']['expid']}_{config['general']['setup_name']}.log"
        )
        search_dir = config["general"]["experiment_dir"]
    elif config["general"]["inspect"] == "datefile":
        maybe_file = (
            f"{config['general']['expid']}_{config['general']['setup_name']}.date"
        )
        search_dir = config["general"]["experiment_dir"]
    else:
        maybe_file = config["general"]["inspect"]

    for path, subdirs, files in os.walk(search_dir):
        if not path.endswith("work"):  # skip work for now
            for full_filepath in glob.iglob(os.path.join(path, maybe_file)):
                cat_file(full_filepath)
                knownfiles.update({os.path.basename(full_filepath): full_filepath})

    for path, subdirs, files in os.walk(config["general"]["thisrun_dir"] + "/work"):
        for full_filepath in glob.iglob(os.path.join(path, maybe_file)):
            somefile = os.path.basename(full_filepath)
            if somefile in knownfiles:
                if filecmp.cmp(knownfiles[somefile], full_filepath):
                    print(
                        f"File {full_filepath} is identical to {knownfiles[somefile]}, skipping."
                    )
                    continue
                else:
                    print(f"File {full_filepath} differs from {knownfiles[somefile]}.")
            cat_file(full_filepath)
    return config


def dir_size(somepath):
    size = 0
    for path, subdirs, files in os.walk(somepath):
        for somefile in files:
            full_filepath = os.path.join(path, somefile)
            if not os.path.islink(full_filepath):
                size += os.path.getsize(full_filepath)
    return size


def cat_file(full_filepath):
    if os.path.isfile(full_filepath):
        print(f"Content of {full_filepath}:")
        with open(full_filepath, "r") as log:
            print(log.read())
