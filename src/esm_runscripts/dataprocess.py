import os
import importlib.util


def sub_plan_environment(config, sub_plan):

    task_list = []
    sub_plan_config = config["general"]["workflow"]["sub_plans"][sub_plan]

    env_preparation = sub_plan_config.get("env_preparation", False)
    scriptdir = sub_plan_config.get("script_dir", False)

    if env_preparation:
        env = assemble_filename(env_preparation, scriptdir, config)
        spec = importlib.util.spec_from_file_location(sub_plan, env)
        envmodule = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(envmodule)

        env_dict = getattr(envmodule, "prepare_environment")(config)
        task_list += export_string(env_dict)

    return task_list


def sub_plan_tasks(config, sub_plan, batch_or_shell):

    task_list = []
    sub_plan_config = config["general"]["workflow"]["sub_plans"][sub_plan]

    old_logfile = config["general"]["logfile_path"]
    logfile_dir = os.path.dirname(old_logfile)
    if config["general"]["setup_name"] in sub_plan:
        bare_sub_plan = sub_plan.replace("_" + config["general"]["setup_name"], "")
    else:
        bare_sub_plan = sub_plan
    logfile_name = os.path.basename(old_logfile).replace(
        config["general"]["jobtype"], bare_sub_plan
    )

    new_logfile = os.path.join(logfile_dir, logfile_name)

    scriptdir = sub_plan_config.get("script_dir", False)
    script = sub_plan_config.get("script", False)
    call_function = sub_plan_config.get("call_function", False)

    if script:
        script = assemble_filename(script, scriptdir, config)
        # task_list += add_scriptcall(script, cluster, config)
        if batch_or_shell == "batch":
            if "calc_launcher_flags" in dir(config['general']["batch"].bs):
                launcher_flags = config['general']["batch"].bs.calc_launcher_flags(
                    {
                        "dataprocess": sub_plan_config,
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
