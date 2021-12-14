from . import helpers


def run_job(config):
    config["general"]["relevant_filetypes"] = [
        "log",
        "mon",
        "outdata",
        "restart_out",
        "bin",
        "config",
        "forcing",
        "input",
        "restart_in",
        "ignore",
    ]
    helpers.evaluate(config, "dataprocess", "data_recipe")
    return config


def _assemble_dataprocess_tasks(config):
    """
    Generates all tasks for data processing which will be written to the run file.

    Parameters
    ----------
    data_file
        File handle to which information should be written.

    Returns
    -------
    data_task_list : list
        The list of post commands which will be executed. These are written
        to the run file.
    """
    datafile = config["general"]["post_file"]
    data_task_list = []

    # First find the correct subjob_cluster:

    this_cluster = config["general"]["job_type"]
    config["general"]["this_cluster"] = copy.deepcopy(
        config["general"]["workflow"]["subjob_clusters"][this_cluster]
    )

    # gather the information on the clusters to be submitted by this job.
    config["general"]["next_clusters"] = {}
    for cluster in config["general"]["this_cluster"]["next_submit"]:
        config["general"]["next_clusters"].update(
            config["general"]["workflow"]["subjob_clusters"][cluster]
        )

    for subcluster in config["general"]["next_clusters"]:
        data_task_list.append(tasks_of_one_cluster(config, subcluster))

    return data_task_list


def add_environment(env, config):

    env_dict = env(config)  # that wont work and needs correction!
    return export_string(env_dict)


def assemble_srun_command(scriptcall, config):
    ...


def add_scriptcall(scriptcall, cluster, config):

    submit = cluster.get("submit_to_batch_system", False)
    order = cluster.get("order_in_cluster", False)

    if submit:
        scriptcall = assemble_srun_command(script)

    if order == "concurrent":
        scriptcall = scriptcall + ";"

    return scriptcall


def tasks_of_one_subjob(config, cluster, subjob):
    task_list = []
    subjob_config = config["general"]["worksflow"]["subjobs"][subjob]

    env_preparation = subjob_config.get("env_preparation", False)
    scriptdir = subjob_config.get("script_dir", False)
    script = subjob_config.get(script, False)

    if env_preparation:
        env_preparation = assemble_filename(env_preparation, scriptdir, config)
        task_list.append(add_environment(env_preparation, config))

    if script:
        script = assemble_filename(script, scriptdir, config)
        task_list.append(add_scriptcall(script, cluster, config))

    return task_list


def tasks_of_one_cluster(config, cluster):
    task_list = []
    clusterconfig = config["general"]["next_clusters"][cluster]
    for subjob in clusterconfig["subjobs"]:
        task_list.append(tasks_of_one_subjob(config, cluster, subjob))

    return task_list

    for component in config["general"]["valid_model_names"]:
        post_file.write(40 * "+ " + "\n")
        post_file.write("Generating post-processing tasks for: %s \n" % component)

        post_task_list.append("\n#Postprocessing %s\n" % component)
        post_task_list.append(
            "cd " + config[component]["experiment_outdata_dir"] + "\n"
        )

        pconfig_tasks = config[component].get("postprocess_tasks", {})
        pconfig_scripts = config[component].get("postprocessing_scripts", {})

        post_file.write("Configuration for post processing: %s \n" % pconfig_tasks)

        for script in pconfig_scripts:
            postscript_name = pconfig_scripts.get("postprocessing_script_name", None)
            postscript_dir = pconfig_scripts.get("postprocessing_dir", None)

            envscript_name = pconfig_scripts.get("postprocessing_envscript_name", None)

            postscript_name = assemble_filename(postscript_name, postscript_dir, config)
            envscript_name = assemble_filename(envscript_name, postscript_dir, config)

            if envscript_name:
                environment_dict = envscript_name(config)
                post_task_list += export_string(environment_dict)

            if postscript_name:
                post_task_list.append(postscript_name)

        post_task_list.append("cd -\n")
        config["general"]["post_task_list"] = post_task_list
    return config


def assemble_filename(filename, dirname, config):
    if filename.startswith("/"):
        return filename
    if filename.startswith(".") or dirname == "." or dirname == "./":
        return os.path.join(["general"]["started_from"], filename)
    if dirname:
        return os.path.join(dirname, filename)
    return os.path.join(["general"]["started_from"], filename)


def export_string(environment_dict):
    export_string = []
    for entry in environment_dict:
        value = environment_dict[entry]
        export_string.append([f"export {entry}={value}"])
    return export_string


# ?????
# def write_simple_postscript(config):
#    batch_system.write_simple_runscript(config)
#    return config
