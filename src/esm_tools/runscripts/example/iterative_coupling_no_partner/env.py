def prepare_environment(config):
    environment_dict = {
            "VAR_IN_SHELL": "same_value",
            "SETUP_NAME": config["general"]["setup_name"],
            }
    return environment_dict
