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
    helpers.evaluate(config, "preprocess", "pre_recipe")
    return config


def _assemble_preprocess_tasks(config):
    """
    Generates all tasks for post processing which will be written to the sad file.

    Parameters
    ----------
    post_file
        File handle to which information should be written.

    Returns
    -------
    post_task_list : list
        The list of post commands which will be executed. These are written
        to the sad file.
    """
    prefile = config["general"]["pre_file"]

    pre_task_list = []
    for component in config["general"]["valid_model_names"]:
        pre_file.write(40 * "+ " + "\n")
        pre_file.write("Generating pre-processing tasks for: %s \n" % component)

        pre_task_list.append("\n#Preprocessing %s\n" % component)
        pre_task_list.append("cd " + config[component]["experiment_outdata_dir"] + "\n")

        pconfig_tasks = config[component].get("preprocess_tasks", {})
        pconfig_scripts = config[component].get("preprocessing_scripts", {})

        pre_file.write("Configuration for pre processing: %s \n" % pconfig_tasks)
        # for outfile in pconfig_tasks:
        #    post_file.write("Generating task to create: %s \n" % outfile)
        #    ofile_config = pconfig_tasks[outfile]
        #    # TODO(PG): This can be cleaned up. I probably actually want a
        #    # ChainMap here for more than just the bottom...
        #    #
        #    # Run CDO tasks (default)
        #    task_definition = config[component].get("postprocess_task_definitions", {}).get(ofile_config['post_process'])
        #    method_definition = config[component].get("postprocess_method_definitions", {}).get(task_definition['method'])
        #
        #    program = method_definition.get("program", task_definition["method"])
        #
        #    possible_args = method_definition.get("possible_args", [])
        #    required_args = method_definition.get("required_args", [])
        #
        #    possible_flags = method_definition.get("possible_flags", [])
        #    required_flags = method_definition.get("required_flags", [])
        #
        #    outfile_flags = ofile_config.get("flags")
        #    outfile_args = ofile_config.get("args")
        #
        #    task_def_flags = task_definition.get("flags")
        #    task_def_args = task_definition.get("args")
        #
        #    args = collections.ChainMap(outfile_args, task_def_args)
        #    flags = outfile_flags + task_def_flags
        #    flags = ["-"+flag for flag in flags]
        #
        #    # See here: https://stackoverflow.com/questions/21773866/how-to-sort-a-dictionary-based-on-a-list-in-python
        #    all_call_things = {"program": program, "outfile": outfile, **args, "flags": flags}
        #    print(all_call_things)
        #    index_map = {v: i for i, v in enumerate(method_definition["call_order"])}
        #    call_list = sorted(all_call_things.items(), key=lambda pair: index_map[pair[0]])
        #    call = []
        #    for call_id, call_part in call_list:
        #        if isinstance(call_part, str):
        #            call.append(call_part)
        #        elif isinstance(call_part, list):
        #            call.append(" ".join(call_part))
        #        else:
        #            raise TypeError("Something straaaange happened. Consider starting the debugger.")
        #    post_file.write(" ".join(call)+"\n")
        #    post_task_list.append(" ".join(call))

        for script in pconfig_scripts:
            prescript_name = pconfig_scripts.get("preprocessing_script_name", None)
            prescript_dir = pconfig_scripts.get("preprocessing_dir", None)

            envscript_name = pconfig_scripts.get("preprocessing_envscript_name", None)

            prescript_name = assemble_filename(prescript_name, prescript_dir, config)
            envscript_name = assemble_filename(envscript_name, prescript_dir, config)

            if envscript_name:
                environment_dict = envscript_name(config)
                pre_task_list += export_string(environment_dict)

            if prescript_name:
                pre_task_list.append(prescript_name)

        pre_task_list.append("cd -\n")
        config["general"]["pre_task_list"] = pre_task_list
    return config
