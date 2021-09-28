def resubmit_SimulationSetup(config, cluster=None):
    monitor_file = config["general"]["log_file"]
    # Jobs that should be started directly from the compute job:

    jobtype = config["general"]["jobtype"]
    #
    #    do_jobtype = False
    #    for model in config:
    #        # Allows for both "do_post: True" or "post: True" in config:
    #        if (
    #            config[model].get(f"do_{jobtype}", False) or
    #            config[model].get(jobtype, False)
    #        ):
    #            do_jobtype = True
    #    if do_jobtype:

    monitor_file.write(f"{cluster} for this run:\n")
    command_line_config = config["general"]["command_line_config"]
    command_line_config["jobtype"] = cluster

    # not necessarily from compute...
    # command_line_config["original_command"] = command_line_config[
    #    "original_command"
    # ].replace(jobtype, cluster)

    monitor_file.write(f"Initializing {cluster} object with:\n")
    monitor_file.write(str(command_line_config))
    # NOTE(PG) Non top level import to avoid circular dependency:

    from .sim_objects import SimulationSetup

    cluster_obj = SimulationSetup(command_line_config)

    monitor_file.write("f{cluster} object built....\n")

    if f"{cluster}_update_{jobtype}_config_before_resubmit" in cluster_obj.config:
        monitor_file.write(
            f"{cluster} object needs to update the calling job config:\n"
        )
        # FIXME(PG): This might need to be a deep update...?
        config.update(
            cluster_obj.config[f"{cluster}_update_{jobtype}_config_before_resubmit"]
        )

    monitor_file.write(f"Calling {cluster} job:\n")
    cluster_obj()

    return config


# def resubmit_SimulationSetup(config):
#    # this is restarting the whole workflow
#    monitor_file = config["general"]["monitor_file"]
#    monitor_file.write("resubmitting \n")
#    command_line_config = config["general"]["command_line_config"]
#    command_line_config["jobtype"] = "compute"
#
#    # not necessarily from tidy, and not nec. to compute
#    command_line_config["original_command"] = command_line_config[
##        "original_command"
#        ].replace("tidy", "compute")
#
#    # seb-wahl: end_date is by definition (search for 'end_date') smaller than final_date
#    # hence we have to use next_date = current_date + increment
#    monitor_file.write("Init for next run:\n")
#    # NOTE(PG) Non top level import to avoid circular dependency:
#    from .sim_objects import SimulationSetup
#
#    next_compute = SimulationSetup(command_line_config)
#    next_compute(kill_after_submit=False)
#    return config


##def start_post_job(config):
#    monitor_file = config["general"]["monitor_file"]
#    do_post = False
##    for model in config:,
#        if "post_processing" in config[model]:
#            if config[model]["post_processing"]:
#                do_post = True
#
#    if do_post:
#        monitor_file.write("Post processing for this run:\n")
#        command_line_config = config["general"]["command_line_config"]
#        command_line_config["jobtype"] = "post"
#        command_line_config["original_command"] = command_line_config[
#            "original_command"
#        ].replace("compute", "post")
#        monitor_file.write ("Initializing post object with:\n")
#        monitor_file.write(str(command_line_config))
#        # NOTE(PG) Non top level import to avoid circular dependency:
#        from .sim_objects import SimulationSetup
#        this_post = SimulationSetup(command_line_config)
#        monitor_file.write("Post object built; calling post job:\n")
#        this_post()
#    return config
#
#
