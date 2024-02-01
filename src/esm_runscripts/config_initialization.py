import os
import copy
import sys

import esm_parser
import esm_tools

from . import chunky_parts


def init_iterative_coupling(command_line_config, user_config):

    # maybe switch to another runscript, if iterative coupling
    user_config["general"]["iterative_coupled_model"] = ""
    if user_config["general"].get("iterative_coupling", False):
        user_config = chunky_parts.setup_correct_chunk_config(user_config)

        if len(user_config["general"]["original_config"]["general"]["model_queue"]) > 1:
            next_model = user_config["general"]["original_config"]["general"][
                "model_queue"
            ][1]
        else:
            next_model = user_config["general"]["original_config"]["general"][
                "model_queue"
            ][0]

        scriptname = user_config["general"]["original_config"][next_model]["runscript"]
        # command_line_config["scriptname"] = os.path.join(user_config["general"]["started_from"], scriptname)
        new_command_line_config = copy.deepcopy(command_line_config)
        new_command_line_config["scriptname"] = scriptname
        new_command_line_config["runscript_abspath"] = os.path.join(
            os.getcwd(), scriptname
        )

        model_config = get_user_config_from_command_line(new_command_line_config)
        user_config = esm_parser.new_deep_update(user_config, model_config)

        # Set the ``iterative_coupled_model`` string, to add the model name to the
        # run_ folder, finished_config.yaml, etc., to avoid overwritting with the
        # files of other offline coupled models
        user_config["general"]["iterative_coupled_model"] = (
            f"{user_config['general']['setup_name']}_"
        )
        # Extract information about the models run in the previous chunk
        chunky_parts.prev_chunk_info(user_config)

    if user_config["general"].get("debug_obj_init", False):
        pdb.set_trace()

    return user_config


def complete_config_from_user_config(user_config):
    config = get_total_config_from_user_config(user_config)

    if "verbose" not in config["general"]:
        config["general"]["verbose"] = False

    config["general"]["reset_calendar_to_last"] = False

    if config["general"].get("inspect"):
        config["general"]["jobtype"] = "inspect"

        if config["general"].get("inspect") not in [
            "workflow",
            "overview",
            "config",
        ]:
            config["general"]["reset_calendar_to_last"] = True

    return config


def save_command_line_config(config, command_line_config):
    if command_line_config:
        config["general"]["command_line_config"] = command_line_config
    else:
        config["general"]["command_line_config"] = {}

    return config


def get_user_config_from_command_line(command_line_config):
    """
    Reads the runscript provided in ``command_line_config`` and overwirtes the
    information of the runscript with that of the command line (command line wins
    over the runscript.

    Input
    -----
    command_line_config : dict
        Dictionary containing the information coming from the command line

    Returns
    -------
    user_config : dict, DictWithProvenance
        Dictionary containing the information from the command line on top of the
        runscript's

    Raises
    ------
    Syntaxerror : esm_parser.user_error
        If there is a problem with the parsing of the runscript
    """

    # Default user_config
    user_config = {
        "general": {
            "additional_files": [],
        },
    }

    # Read the content of the runscrip
    try:
        user_config.update(
            esm_parser.initialize_from_yaml(command_line_config["runscript_abspath"])
        )
    # If sys.exit is triggered through esm_parser.user_error (i.e. from
    # ``check_for_empty_components`` in ``yaml_to_dict.py``) catch the sys.exit.
    except SystemExit as sysexit:
        sys.exit(sysexit)
    except:
        esm_parser.user_error(
            "Syntax error",
            f"An error occurred while reading the config file "
            f"``{command_line_config['runscript_abspath']}`` from the command line.")

    user_config["general"].update(command_line_config)

    return user_config


def init_interactive_info(command_line_config, user_config):
    """
    Initialize key-values to evaluate at any point whether interactive functions are to
    be run (e.g. questionaries, warnings, etc.). The following key-values are set within
    ``user_config["general"]``:
    - ``isinteractive``: ``True`` if this function is trigger by a command line
            execution
    - ``isresubmitted``: ``True`` if the ``last_jobtype`` is the same as the current
            ``jobtype`` (after the user triggers ``esm_runscripts`` there is a first
            step of preparing the experiment folder and then it resubmit it itself from
            the experiment folder; most questionaries need to be run in this second step
            ``isresubmitted`` because only then the updated information via the
            questionaries plays a role in the simulation).

    Input
    -----
    command_line_config : dict
        Dictionary containing the information coming from the command line
    user_config : dict
        Dictionary containing the information from the command line on top of the
        runscript's

    Returns
    -------
    user_config : Dict
        Same as the input ``user_config`` but with the interactive variables
    """
    last_jobtype = command_line_config.get("last_jobtype", "")
    isinteractive = last_jobtype == "command_line"
    isresubmitted = last_jobtype == user_config["general"]["jobtype"]

    user_config["general"]["isinteractive"] = isinteractive
    user_config["general"]["isresubmitted"] = isresubmitted

    return user_config


def get_total_config_from_user_config(user_config):

    if "version" in user_config["general"]:
        version = str(user_config["general"]["version"])
    else:
        setup_name = user_config["general"]["setup_name"]

        if "version" in user_config[setup_name.replace("_standalone", "")]:
            version = str(user_config[setup_name.replace("_standalone", "")]["version"])
        else:
            version = "DEFAULT"

    config = esm_parser.ConfigSetup(
        user_config["general"]["setup_name"].replace("_standalone", ""),
        version,
        user_config,
    )

    config = add_esm_runscripts_defaults_to_config(config)

    config["computer"]["jobtype"] = config["general"]["jobtype"]
    config["general"]["experiment_dir"] = (
        config["general"]["base_dir"] + "/" + config["general"]["expid"]
    )

    # Check if the 'account' variable is needed and missing
    if config["computer"].get("accounting", False):
        if "account" not in config["general"]:
            esm_parser.user_error(
                "Missing account info",
                f"You cannot run simulations in '{config['computer']['name']}' "
                "without providing an 'account' variable in the 'general' section, whose "
                "value refers to the project where the computing resources are to be "
                "taken from. Please, add the following to your runscript:\n\n"
                "general:\n\taccount: <the_account_to_be_used>",
            )

    return config


def add_esm_runscripts_defaults_to_config(config):
    path_to_file = esm_tools.get_config_filepath() + "/esm_software/esm_runscripts/defaults.yaml"
    default_config = esm_parser.yaml_file_to_dict(path_to_file)
    config["general"]["defaults.yaml"] = default_config
    config = distribute_per_model_defaults(config)
    return config


def distribute_per_model_defaults(config):
    default_config = config["general"]["defaults.yaml"]
    if "general" in default_config:
        config["general"] = esm_parser.new_deep_update(
            config["general"], default_config["general"]
        )

    if "per_model_defaults" in default_config:
        per_model_defaults = copy.deepcopy(default_config["per_model_defaults"])
        # Remove ``file_movements`` from per_model_defaults as this is resolved in
        # ``filelists.py`` and otherwise, it is not possible to understand there what
        # comes from the defaults, from general or from the model config itself.
        if "file_movements" in per_model_defaults:
            del per_model_defaults["file_movements"]
        # Set defaults per model
        for model in config["general"]["valid_model_names"]:
            config[model] = esm_parser.new_deep_update(
                config[model], per_model_defaults
            )
    return config
