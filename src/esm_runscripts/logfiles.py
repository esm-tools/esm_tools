import os
import sys

import _io

from . import helpers


def initialize_logfiles(config, org_jobtype):
    global logfile_run_number

    logfile_run_number = str(config["general"]["run_number"])

    log_stuff = False
    if os.path.isdir(os.path.dirname(config["general"]["experiment_log_file"])):
        if not org_jobtype == "inspect":
            log_stuff = True

    config = set_logfile_name(config, "")

    if log_stuff:

        helpers.write_to_log(
            config,
            [
                org_jobtype,
                logfile_run_number,
                str(config["general"]["current_date"]),
                str(config["general"]["jobid"]),
                "- start",
            ],
        )

    return config


def finalize_logfiles(config, org_jobtype):

    if os.path.isdir(os.path.dirname(config["general"]["experiment_log_file"])):
        log_stuff = True

    if log_stuff:
        helpers.write_to_log(
            config,
            [
                org_jobtype,
                logfile_run_number,
                str(config["general"]["current_date"]),
                str(config["general"]["jobid"]),
                "- done",
            ],
        )

    return config


def set_logfile_name(config, jobtype=None):

    if not jobtype:
        jobtype = config["general"]["jobtype"]

    expid = config["general"]["expid"]
    setup_name = config["general"]["setup_name"]
    filejobtype = jobtype
    run_datestamp = config["general"]["run_datestamp"]
    experiment_log_dir = config["general"]["experiment_log_dir"]
    thisrun_log_dir = config["general"]["thisrun_log_dir"]

    filename = f"{expid}_{setup_name}_{filejobtype}_{run_datestamp}.log"

    config["general"]["logfile_path"] = f"{experiment_log_dir}/{filename}"
    config["general"]["logfile_path_in_run"] = f"{thisrun_log_dir}/{filename}"

    return config

