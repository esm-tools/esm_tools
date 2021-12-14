import os
import importlib.util


def subjob_environment(config, subjob):

    task_list = []
    subjob_config = config["general"]["workflow"]["subjobs"][subjob]

    env_preparation = subjob_config.get("env_preparation", False)
    scriptdir = subjob_config.get("script_dir", False)

    if env_preparation:
        env = assemble_filename(env_preparation, scriptdir, config)
        spec = importlib.util.spec_from_file_location(subjob, env)
        envmodule = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(envmodule)

        env_dict = getattr(envmodule, "prepare_environment")(config)
        task_list += export_string(env_dict)

    return task_list


def subjob_tasks(config, subjob, batch_or_shell):

    task_list = []
    subjob_config = config["general"]["workflow"]["subjobs"][subjob]

    old_logfile = config["general"]["logfile_path"]
    logfile_dir = os.path.dirname(old_logfile)
    if config["general"]["setup_name"] in subjob:
        bare_subjob = subjob.replace("_" + config["general"]["setup_name"], "")
    else:
        bare_subjob = subjob
    logfile_name = os.path.basename(old_logfile).replace(
        config["general"]["jobtype"], bare_subjob
    )

    new_logfile = os.path.join(logfile_dir, logfile_name)

    scriptdir = subjob_config.get("script_dir", False)
    script = subjob_config.get("script", False)
    call_function = subjob_config.get("call_function", False)

    if script:
        script = assemble_filename(script, scriptdir, config)
        # task_list += add_scriptcall(script, cluster, config)
        if batch_or_shell == "batch":
            if "calc_launcher_flags" in dir(config['general']["batch"].bs):
                launcher_flags = config['general']["batch"].bs.calc_launcher_flags(
                    {
                        "dataprocess": subjob_config,
                        "computer": config["computer"]
                    },
                    'dataprocess',
                    'pp',
                )
            else:
                launcher_flags = config['computer']['launcher_flags']
            task_list += [
                f"time {config['computer']['launcher']} "
                f"{launcher_flags} {script} 2>&1 &"
            ]
        else:
            if call_function:
                task_list += [". " + script]
                task_list += [call_function + " > " + new_logfile + " 2>&1 &"]
            else:
                task_list += [script + " > " + new_logfile + " 2>&1 &"]

    return task_list


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
        export_string.append(f"export {entry}={value}")
    return export_string


# def run_job(config):
#    config["general"]["relevant_filetypes"] = ["log", "mon", "outdata", "restart_out","bin", "config", "forcing", "input", "restart_in", "ignore"]
#    helpers.evaluate(config, "dataprocess", "data_recipe")
#    return config
#
#
# def _assemble_dataprocess_tasks(config):
##    """
#    Generates all tasks for data processing which will be written to the run file.
#
#    Parameters
#    ----------
#    data_file
#        File handle to which information should be written.
#
#    Returns
#    -------
#    data_task_list : list
##        The list of post commands which will be executed. These are written
#        to the run file.
#    """
#    datafile = config["general"]["post_file"]
#    data_task_list = []
#
#    # First find the correct subjob_cluster:
#
#    this_cluster = config["general"]["job_type"]
#    config["general"]["this_cluster"] = copy.deepcopy(config["general"]["workflow"]["subjob_clusters"][this_cluster])
##
#    # gather the information on the clusters to be submitted by this job.
#    config["general"]["next_clusters"] = {}
#    for cluster in config["general"]["this_cluster"]["next_submit"]:
#        config["general"]["next_clusters"].update(config["general"]["workflow"]["subjob_clusters"][cluster])
#
##    for subcluster in config["general"]["next_clusters"]:
#        data_task_list.append(tasks_of_one_cluster(config, subcluster))
#
#    return data_task_list
#
#
#
#
# def cluster_scriptcall(scriptcall, cluster, config):
#
#    submit = cluster.get("submit_to_batch_system", False)
#    order = cluster.get("order_in_cluster", False)
#
#    if submit:
#        scriptcall = assemble_srun_command(script)
#
#    if order == "concurrent":
#        scriptcall = scriptcall + ";"
#
#    return scriptcall
#


# def tasks_of_one_subjob(config, cluster, subjob):
#    task_list = []
#    subjob_config = config["general"]["worksflow"]["subjobs"][subjob]
#
#
#    env_preparation = subjob_config.get("env_preparation", False)
#    scriptdir = subjob_config.get("script_dir", False)
#    script = subjob_config.get(script, False)
#
#    if env_preparation:
#        env_preparation = assemble_filename(env_preparation, scriptdir, config)
#        task_list.append(add_environment(env_preparation, config))
#
##    if script:
#        script = assemble_filename(script, scriptdir, config)
#        task_list.append(add_scriptcall(script, cluster, config))
#
#    return task_list
#
#
#
# def tasks_of_one_cluster(config, cluster):
#    task_list = []
#    clusterconfig = config["general"]["next_clusters"][cluster]
#    for subjob in clusterconfig["subjobs"]:
#        task_list.append(tasks_of_one_subjob(config, cluster, subjob))
#
##    return task_list


#    for component in config["general"]["valid_model_names"]:
#        post_file.write(40*"+ "+"\n")
#        post_file.write("Generating post-processing tasks for: %s \n" % component)
#
#        post_task_list.append("\n#Postprocessing %s\n" % component)
#        post_task_list.append("cd "+ config[component]["experiment_outdata_dir"]+"\n")
#
#       pconfig_tasks = config[component].get('postprocess_tasks', {})
##       pconfig_scripts = config[component].get('postprocessing_scripts', {})
#
#        post_file.write("Configuration for post processing: %s \n" % pconfig_tasks)
#
#       for script in pconfig_scripts:
#           postscript_name = pconfig_scripts.get("postprocessing_script_name", None)
##           postscript_dir = pconfig_scripts.get("postprocessing_dir", None)
#
#            envscript_name = pconfig_scripts.get("postprocessing_envscript_name", None)
#
#            postscript_name = assemble_filename(postscript_name, postscript_dir, config)
#           envscript_name = assemble_filename(envscript_name, postscript_dir, config)
##
#            if envscript_name:
#                environment_dict = envscript_name(config)
#                post_task_list += export_string(environment_dict)
#
##            if postscript_name:
#                post_task_list.append(postscript_name)
#
#        post_task_list.append("cd -\n")
#        config["general"]["post_task_list"] = post_task_list
#    return config
#
#
#

# ?????
# def write_simple_postscript(config):
#    batch_system.write_simple_runscript(config)
#    return config
