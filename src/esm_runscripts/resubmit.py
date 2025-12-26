import os

from loguru import logger

from . import chunky_parts, helpers, logfiles, workflow


def submit(config):
    from . import batch_system

    logger.debug("\n", 40 * "+ ")

    batch_obj = config["general"]["batch"]

    # Check if we're in Jacamar mode
    if hasattr(batch_obj, 'submit_mode') and batch_obj.submit_mode == "jacamar":
        logger.info("Submitting via GitLab/Jacamar trigger...")

        # Extract SBATCH headers for Jacamar
        cluster = config["general"]["jobtype"]
        headers = batch_system.get_batch_header(config, cluster, for_jacamar=True)

        # Get the collected script sections
        if "jacamar_sections" not in config["general"]:
            logger.error("Jacamar sections not found in config")
            raise Exception(
                "Script sections were not collected during generation. "
                "This is an internal error."
            )

        script_sections = config["general"]["jacamar_sections"]

        # Submit via GitLab API
        pipeline_data = batch_obj.jacamar.submit_via_api(headers, script_sections, config)

        # Store pipeline URL for user reference
        config["general"]["gitlab_pipeline_url"] = pipeline_data.get("web_url")
        config["general"]["gitlab_pipeline_id"] = pipeline_data.get("id")

    else:
        # Traditional direct submission via sbatch
        logger.info("Submitting jobscript to batch system...")
        logger.info(f"Output written by {config['computer']['batch_system']}:")
        logger.debug("\n", 40 * "+ ")
        for command in config["general"]["submit_command"]:
            logger.debug(command)
        for command in config["general"]["submit_command"]:
            os.system(command)

    return config


def resubmit_batch_or_shell(config, batch_or_shell, cluster=None):
    config = config["general"]["batch"].write_simple_runscript(
        config, cluster, batch_or_shell
    )
    if not check_if_check(config):
        config = submit(config)
    return config


def resubmit_SimulationSetup(config, cluster=None):
    # Jobs that should be started directly from the compute job:

    jobtype = config["general"]["jobtype"]

    command_line_config = config["general"]["command_line_config"]
    command_line_config["jobtype"] = cluster

    logfiles.initialize_logging(command_line_config)

    logger.debug(f"{cluster} for this run:")
    logger.debug(f"Initializing {cluster} object with:")
    logger.debug(str(command_line_config))
    # NOTE(PG) Non top level import to avoid circular dependency:

    os.chdir(config["general"]["started_from"])
    from .sim_objects import SimulationSetup

    cluster_obj = SimulationSetup(command_line_config)

    logger.debug(f"{cluster} object built....")

    if f"{cluster}_update_{jobtype}_config_before_resubmit" in cluster_obj.config:
        logger.debug(f"{cluster} object needs to update the calling job config:")
        # FIXME(PG): This might need to be a deep update...?
        config.update(
            cluster_obj.config[f"{cluster}_update_{jobtype}_config_before_resubmit"]
        )

    if not check_if_check(config):
        logger.debug(f"Calling {cluster} job:")
        config["general"]["experiment_over"] = cluster_obj(kill_after_submit=False)

    return config


def get_submission_type(cluster, config):
    # Figure out if next job is resubmitted to batch system,
    # just executed in shell or invoked as new SimulationSetup
    # object

    clusterconf = config["general"]["workflow"]["subjob_clusters"][cluster]

    if clusterconf.get("submit_to_batch_system", False):
        submission_type = "batch"
    elif cluster in ["newrun", "prepcompute", "tidy", "inspect", "viz"]:
        submission_type = "SimulationSetup"
    else:
        submission_type = "shell"

    return submission_type


def end_of_experiment(config):
    if config["general"]["next_date"] >= config["general"]["final_date"]:
        logger.info("Reached the end of the simulation, quitting...")
        config["general"]["experiment_over"] = True
        logger.progress("Experiment over")
        return True
    return False


def end_of_experiment_all_models(config):
    index = 1
    expid = config["general"]["expid"]
    while "model" + str(index) in config["general"]["original_config"]:
        if (
            not config["model" + str(index)]["setup_name"]
            == config["general"]["setup_name"]
        ):
            experiment_done = False
            setup_name = config["model" + str(index)]["setup_name"]
            logger.info(f"Testing if {setup_name} is already done...")
            logfile = (
                config["general"]["experiment_log_dir"]
                + "/"
                + expid
                + "_"
                + setup_name
                + ".log"
            )
            if os.path.isfile(logfile):
                with open(logfile, "r") as open_logfile:
                    logfile_array = open_logfile.readlines()
                    for line in logfile_array:
                        if "# Experiment over" in line:
                            logger.info(f"    ...{setup_name} is done.")
                            experiment_done = True
                            break
            if not experiment_done:
                logger.info("Still something left to do...")
                return False
        index += 1
    logger.info("Nothing left to do...")
    return True


def check_if_check(config):
    if config["general"]["check"]:
        logger.info(
            "Actually not submitting anything, this job preparation was launched in 'check' mode (-c)."
        )
        return True
    else:
        return False


def maybe_resubmit(config):
    jobtype = config["general"]["jobtype"]

    nextrun = resubmit_recursively(config, jobtype=jobtype)

    if nextrun:  # submit list contains stuff from next run
        config = _increment_date_and_run_number(config)
        config = _write_date_file(config)

        if end_of_experiment(config):
            if config["general"].get("iterative_coupling", False):
                if end_of_experiment_all_models(config):
                    return config
            else:
                # config = chunky_parts._update_chunk_date_file(config)
                return config

        cluster = config["general"]["workflow"]["first_task_in_queue"]
        nextrun = resubmit_recursively(
            config, list_of_clusters=[cluster], nextrun_in=True
        )

    return config


def resubmit_recursively(config, jobtype=None, list_of_clusters=None, nextrun_in=False):
    nextrun = False

    if not list_of_clusters:
        list_of_clusters = config["general"]["workflow"]["subjob_clusters"][
            jobtype
        ].get("next_submit", [])

    for cluster in list_of_clusters:
        if (
            cluster == config["general"]["workflow"]["first_task_in_queue"]
            and not nextrun_in
        ):
            nextrun = True
        else:
            if not workflow.skip_cluster(cluster, config):
                submission_type = get_submission_type(cluster, config)
                if submission_type == "SimulationSetup":
                    resubmit_SimulationSetup(config, cluster)
                elif submission_type in ["batch", "shell"]:
                    resubmit_batch_or_shell(config, submission_type, cluster)
            else:
                logger.info(f"Skipping {cluster}")
                nextrun = (
                    resubmit_recursively(config, jobtype=cluster, nextrun_in=nextrun_in)
                    or nextrun
                )
    return nextrun


def _increment_date_and_run_number(config):
    config["general"]["run_number"] += 1
    config["general"]["current_date"] += config["general"]["delta_date"]

    config["general"]["command_line_config"]["current_date"] = config["general"][
        "current_date"
    ].format(form=9, givenph=False, givenpm=False, givenps=False)

    config["general"]["command_line_config"]["run_number"] = config["general"][
        "run_number"
    ]

    config = chunky_parts.update_command_line_config(config)

    return config


def _write_date_file(config):  # self, date_file=None):

    # if not date_file:
    date_file = (
        f"{config['general']['experiment_scripts_dir']}"
        f"/{config['general']['expid']}_{config['general']['setup_name']}.date"
    )

    with open(date_file, "w") as date_file:
        date_file.write(
            config["general"]["current_date"].output()
            + " "
            + str(config["general"]["run_number"])
        )
    logger.debug("writing date file")
    return config
