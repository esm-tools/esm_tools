def prepare_environment(config):
    default_input_grid = config["general"]["experiment_couple_dir"] +"/ice.griddes"
    environment_dict = {
            "NYEAR": config["general"]["nyear"],
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            }
    print (environment_dict)
    return environment_dict
