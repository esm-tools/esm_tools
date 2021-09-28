import os, sys
from . import helpers


def initialize_logfiles(config, org_jobtype):
    global logfile_handle
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
        logfile = open(
            config["general"]["logfile_path"],
            "w",
            buffering=1,
        )
    else:
        logfile = sys.stdout

    logfile_handle = logfile
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

    if not logfile_handle == sys.stdout:
        logfile_handle.close()
    return config


def set_logfile_name(config, jobtype=None):

    if not jobtype:
        jobtype = config["general"]["jobtype"]

    filejobtype = jobtype
    # if "observe" in filejobtype:
    #    filejobtype = filejobtype.replace("observe_", "")

    # if "newrun" in filejobtype:
    #    filejobtype = config["general"]["workflow"]["subjob_clusters"][jobtype].get("next_submit")[0]

    # if filejobtype == "prepcompute":
    #    filejobtype = "compute"

    # if "_" + config["general"]["setup_name"] in filejobtype:
    #    filejobtype = filejobtype.replace("_" + config["general"]["setup_name"], "")

    # called_from = config["general"]["workflow"]["subjob_clusters"][jobtype].get("called_from", "SOMETHINgUSELESS")

    # if "_" + called_from in filejobtype:
    #    filejobtype = filejobtype.replace("_" + called_from, "")

    filename = (
        config["general"]["expid"]
        + "_"
        + config["general"]["setup_name"]
        + "_"
        + filejobtype
        + "_"
        + config["general"]["run_datestamp"]
        + ".log"
    )

    config["general"]["logfile_path"] = (
        config["general"]["experiment_log_dir"] + "/" + filename
    )

    config["general"]["logfile_path_in_run"] = (
        config["general"]["thisrun_log_dir"] + "/" + filename
    )

    # if os.path.isfile(config["general"]["logfile_path"]):
    #    if not os.path.isfile(config["general"]["logfile_path_in_run"]):
    #        os.symlink(
    #            config["general"]["logfile_path"],
    #            config["general"]["logfile_path_in_run"]
    #            )

    return config
