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
        return os.path.join(config["general"]["started_from"], filename)
    if dirname:
        return os.path.join(dirname, filename)
    return os.path.join(config["general"]["started_from"], filename)


def export_string(environment_dict):
    export_string = []
    for entry in environment_dict:
        value = environment_dict[entry]
        export_string.append(f"export {entry}={value}")
    return export_string
