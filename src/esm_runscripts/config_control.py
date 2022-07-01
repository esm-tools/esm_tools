import yaml
import esm_parser


def check_configuration_consistency(config):
    """ TODO: fill docstring
    """
    runscript_abspath = config["general"]["command_line_config"]["runscript_abspath"]
    # TODO: can this be None or KeyError?
    models = config['general']['valid_model_names']

    with open(runscript_abspath, "rt") as f:
        runscript = yaml.load(f, Loader=yaml.FullLoader)

    for model in models:
        model_config_path = config[model]["debug_info"]["loaded_from_file"]
        # some models have multiple files. Take the one with <model>.yaml
        if isinstance(model_config_path, list):
            model_config_path = model_config_path[0]
            # deniz: remaining lines was a good idea but don't always work
            # base_name = f"{model}.yaml"
            # model_config_path = next((item for item in model_config_path if base_name in item), None)
        with open(model_config_path, "rt") as f:
            model_config = yaml.load(f, Loader=yaml.FullLoader)

        check_version(config, model_config, runscript, model)
        check_scenario(config, model_config, runscript, model)


def check_version(config, model_config, runscript, model):
    """TODO: fill docstring
    """
    # model is not specified in the runscript. Using the default
    if model not in runscript:
        return True
    #TODO: what about in coupled setups?
    version_from_runscript = runscript[model].get("version", None)

    # populate list of available versions
    available_versions = []
    items = model_config.get("available_versions", None)
    if items:
        available_versions += items
    items = model_config.get("choose_version", None)
    if items:
        available_versions += list(items)
    if available_versions:
        # TODO: I have a better way of handling int/str in versions but this is
        # for the clean-up phase
        available_versions = [str(item) for item in available_versions]
        available_versions = sorted(list(set(available_versions)))

    if version_from_runscript is None:
        return True
    # elif "available_versions" not in model_config:
    elif available_versions is None:
        import ipdb; ipdb.set_trace()
        msg = f"Version: '{version_from_runscript}' is requested but the model {model} does not have any versions"
        esm_parser.user_note("Warning", msg)
        return True

    # exit if requested version is not found
    if version_from_runscript not in available_versions: 
        error_type = "Version not available"
        error_msg = f"Version '{version_from_runscript}' is not available for model {model}"
        esm_parser.user_error(error_type, error_msg)

    # version requested in the runscript is found in the model configuration
    return True


def check_scenario(config, model_config, runscript, model):
    """TODO: fill docstring
    """
    # model is not specified in the runscript. Using the default
    if model not in runscript:
        return True
    scenario_from_runscript = runscript[model].get("scenario", None)

    # populate list of available scenarios
    available_scenarios = []
    items = model_config.get("available_scenarios", None)
    if items:
        available_scenarios += items
    items = model_config.get("choose_scenario", None)
    if items:
        available_scenarios += list(items)
    if available_scenarios:
        # TODO: I have a better way of handling int/str in versions but this is
        # for the clean-up phase
        available_scenarios = [str(item) for item in available_scenarios]
        available_scenarios = sorted(list(set(available_scenarios)))

    if scenario_from_runscript is None:
        return True
    elif available_scenarios is None:
        msg = f"Scenario: '{scenario_from_runscript}' is requested but the model {model} does not have any scenarios"
        esm_parser.user_note("Warning", msg)
        return True

    # exit if requested scenario is not found
    if scenario_from_runscript not in available_scenarios:
        error_type = "Scenario not available"
        error_msg = f"Scenario '{scenario_from_runscript}' is not available for model {model}"
        esm_parser.user_error(error_type, error_msg)

    # scenario requested in the runscript is found in the model configuration
    return True


if __name__ == "__main__":
    pass
