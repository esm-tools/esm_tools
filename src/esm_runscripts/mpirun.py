def write_hostfile(config):
    path = "hostfile_srun"
    if "multi_srun" in config["general"]:
        for run_type in list(config["general"]["multi_srun"]):
            current_hostfile = path + "_" + run_type
            write_one_hostfile(current_hostfile, config)
    else:
        write_one_hostfile(path, config)
    return config


def write_one_hostfile(hostfile, config):
    mpirun_options = ""

    for model in config["general"]["valid_model_names"]:
        end_core = config[model].get("end_proc", None)
        start_proc = config[model].get("start_proc", None)

        no_cpus = end_proc - start_proc

        if "execution_command" in config[model]:
            mpirun_options += (
                " -np " + no_cpus + " ./" + config[model]["execution_command"] + " :"
            )
        elif "executable" in config[model]:
            mpirun_options += (
                " -np " + no_cpus + " ./" + config[model]["executable"] + " :"
            )

    mpirun_options = mpirun_options[:-1]  # remove trailing ":"

    with open(hostfile, "w") as hostfile:
        hostfile.write(mpirun_options)
    return config
