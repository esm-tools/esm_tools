#!/usr/bin/env python
# import fileinput, os, sys, getopt

import subprocess
import sys
import os
import yaml

from . import database_actions

from .general_stuff import (
    GeneralInfos,
    version_control_infos,
    tab_completion,
    write_minimal_user_config,
    ESM_MASTER_DIR,
)

from .compile_info import setup_and_model_infos

from .task import Task

from esm_parser import yaml_dump

def main_flow(parsed_args, target):

    # Creates an object of the class GeneralInfos
    main_infos = GeneralInfos(parsed_args)
    vcs = version_control_infos(parsed_args)

    setups2models = setup_and_model_infos(vcs, main_infos, parsed_args)
    tab_completion(parsed_args, setups2models)
    setups2models.config = setups2models.reduce(target)

    user_config = write_minimal_user_config(setups2models.config)
    # Miguel: Move this somewhere else after talking to Paul and Dirk
    user_config["general"]["run_or_compile"] = "compiletime"

    # deniz: verbose is supposed to be a boolean right? It is initialized as
    # 0 in cli.py. Is it then a debug_level?
    if parsed_args.get("verbose", False):
        user_config["general"]["verbose"] = True

    # kh 27.11.20
    if "modify" in parsed_args:
        if "general" in user_config:
            user_config["general"]["modify_config_file"] = parsed_args["modify"]

    if "ignore" in parsed_args:
        ignore_errors = parsed_args["ignore"]
    else:
        ignore_errors = False

    from esm_runscripts.sim_objects import SimulationSetup

    complete_setup = SimulationSetup(user_config=user_config)
    complete_config = complete_setup.config

    setups2models.update_relevant_entries_with_config(complete_config)

    # This will be a problem later with GEOMAR
    # setups2models.replace_last_vars(env)

    # PG: multi-cluster
    # This is probably not the best name for this...
    #
    # Also note, stuff like recomp probably won't work correctly:
    # $ esm_master recomp-awiesm-2.2/pism
    multi_cluster_job = complete_config.get("general", {}).get("multi_cluster_job")
    if multi_cluster_job:
        original_target = target
        original_task = original_target.split("-")[0]
        original_setup = "-".join(original_target.split("-")[1:])
        os.makedirs(original_setup, exist_ok=True)
        os.chdir(original_setup)
        for realm in multi_cluster_job:
            os.makedirs(realm, exist_ok=True)
            os.chdir(realm)
            subprocess.check_call(
                f"esm_master {original_task}-{multi_cluster_job[realm]}", shell=True
            )
            os.chdir("..")
        return 0

    user_task = Task(
        target, setups2models, vcs, main_infos, complete_config, parsed_args
    )
    if parsed_args.get("verbose", False):
        user_task.output()

    user_task.output_steps()
    user_task.validate()
    user_task.generate_task_script()

    # Print config
    current_path = os.getcwd()
    model_dir_rel_pwd = complete_config["general"]["model_dir"].replace(
        f"{current_path}/", ""
    )
    model_name = model_dir_rel_pwd.split("/")[0]
    finished_config_path = f"{current_path}/{model_name}-finished_config.yaml"
    yaml_dump(complete_config, config_file_path=finished_config_path)

    if parsed_args.get("check", False):
        print("esm_master: check mode is activated. Not executing the actions above")
        return 0


    user_task.execute(ignore_errors)  # env)

    database = database_actions.database_entry(
        complete_config, user_task.todo, user_task.package.raw_name, ESM_MASTER_DIR
    )
    if database:
        database.connection.close()

    if not parsed_args["keep"]:
        user_task.cleanup_script()

    return 0
