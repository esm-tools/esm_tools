import copy
import logging
import os
import questionary
import sys

import esm_parser
import yaml
import esm_utilities
from esm_calendar import Calendar, Date
from loguru import logger

from . import batch_system, helpers


def run_job(config):
    helpers.evaluate(config, "prepare", "prepare_recipe")
    return config


def mini_resolve_variable_date_file(date_file, config):
    while "${" in date_file:
        pre, post = date_file.split("${", 1)
        variable, post = post.split("}", 1)
        if "." in variable:
            variable_section, variable = variable.split(".")
            answer = config[variable_section].get(variable)
        else:
            answer = config["general"].get(variable)
            if not answer:
                answer = config.get("env", {}).get(variable)
                if not answer:
                    try:
                        assert variable.startswith("env.") or variable.startswith(
                            "general."
                        )
                    except AssertionError:
                        print(
                            "The date file contains a variable which is not in the >>env<< or >>general<< section. This is not allowed!"
                        )
                        print(f"date_file = {date_file}")
                        sys.exit(1)
        date_file = pre + answer + post
    return date_file


def _read_date_file(config):
    import logging
    import os

    date_file = (
        f"{config['general']['experiment_dir']}/scripts"
        f"/{config['general']['expid']}_{config['general']['setup_name']}.date"
    )

    date_file = mini_resolve_variable_date_file(date_file, config)

    if os.path.isfile(date_file):
        logging.info("Date file read from %s", date_file)
        with open(date_file) as date_file:
            date, run_number = date_file.readline().strip().split()
            run_number = int(run_number)
        write_file = False
    else:
        logging.info("No date file found %s", date_file)
        logging.info("Initializing run_number=1 and date=18500101")
        date = config["general"].get("initial_date", "18500101")
        run_number = 1
        write_file = True
    config["general"]["run_number"] = run_number
    config["general"]["current_date"] = date
    logging.info("current_date = %s", date)
    logging.info("run_number = %s", run_number)
    return config


def check_model_lresume(config):
    if config["general"]["run_number"] != 1:
        for model in config["general"]["valid_model_names"]:
            config[model]["lresume"] = True
    else:
        # Did the user give a value? If yes, keep it, if not, first run:
        for model in config["general"]["valid_model_names"]:
            if "lresume" in config[model]:
                user_lresume = config[model]["lresume"]
            else:
                user_lresume = False

            if isinstance(user_lresume, str) and "${" in user_lresume:
                user_lresume = esm_parser.find_variable(
                    model, user_lresume, config, [], []
                )
            if type(user_lresume) == str:

                if user_lresume == "0" or user_lresume.upper() == "FALSE":
                    user_lresume = False
                elif user_lresume == "1" or user_lresume.upper() == "TRUE":
                    user_lresume = True
            elif isinstance(user_lresume, int):
                if user_lresume == 0:
                    user_lresume = False
                elif user_lresume == 1:
                    user_lresume = True
            config[model]["lresume"] = user_lresume
    for model in config["general"]["valid_model_names"]:
        # Check if lresume contains a variable which might be set in a different model, and resolve this case
        if (
            "lresume" in config[model]
            and isinstance(config[model]["lresume"], str)
            and "${" in config[model]["lresume"]
        ):
            lr = esm_parser.find_variable(
                model, config[model]["lresume"], config, [], []
            )
            config[model]["lresume"] = eval(lr)
    return config


def resolve_some_choose_blocks(config):
    # from esm_parser import choose_blocks

    # Component-specific environment variables into ``computer``
    # before ``computer`` ``choose_`` blocks are resolved
    model_env_into_computer(config)

    esm_parser.choose_blocks(config, blackdict=config._blackdict)
    return config


def model_env_into_computer(config):
    """
    This function allows to store in the ``computer`` dictionary, variables that were
    defined inside ``environment_changes`` or ``compile/runtime_environment_changes``
    in the components.

    It excludes ``module_actions`` and ``export_vars`` dictionaries as those are
    resolved later.

    This function is necessary for controlling ``choose_`` blocks in the computer file
    from the component configuration file (i.e. add ``useMPI`` case to the component to
    control which ``useMPI`` case is selected in the computer file).

    This function works both for compilation time and run time, and the result is that
    all components work under the same environment. The only exception is for
    the compilation of components, where ``add_export_vars`` and ``add_module_actions``
    are excluded from the merging into ``computer``, and are included individually in
    respective compilation scripts.

    Later on, it might be desirable to always split the environments both for compiling
    (done by Paul Gierz, but this function would need to be adapted) and running (not
    done yet).

    If this script gives you problems contact Miguel Andres-Martinez
    (miguel.andres-martinez@awi.de).

    Parameters
    ----------
    config : dict
        Dictionary containing the simulation/compilation information

    Raises
    ------
    User Note/Error
        If the same variable is found in two or more different component environments.
        Asks the user how to proceed.
    """

    # import logging
    # from esm_parser import basic_choose_blocks, dict_merge, user_note, user_error, pprint_config

    # Get which type of changes are to be applied to the environment
    run_or_compile = config.get("general", {}).get("run_or_compile", "runtime")
    config["general"]["run_or_compile"] = run_or_compile
    thesechanges = run_or_compile + "_environment_changes"
    # List the component names
    models = config.get("general", {}).get("models", [])
    # ``env_vars`` stores information about the environment changes to inform
    # about conflicts
    env_vars = {}
    # Loop through the models
    for model in models:
        # Update the models ``environment_changes`` with the ``compiletime/runtime_
        # environment_changes
        modelconfig = copy.deepcopy(config[model])
        modelconfig["environment_changes"] = modelconfig.get("environment_changes", {})
        if thesechanges in modelconfig:
            if "environment_changes" in modelconfig:
                modelconfig["environment_changes"].update(modelconfig[thesechanges])
            else:
                modelconfig["environment_changes"] = modelconfig[thesechanges]
        # Resolve ``choose_`` blocks, ``add_`` and ``remove_`` inside ``environment_
        # changes``
        esm_parser.basic_choose_blocks(modelconfig["environment_changes"], config)
        # Set to true when specified by the user in ``env_overwrite`` or when this
        # method has been already called once in this run
        overwrite = config[model].get("env_overwrite", False)
        # Loop through every variable in ``environment_changes`` except ``export_vars``
        # and ``module_actions`` as those are solved by ``esm_environment`` later and
        # need of the solving of later ``choose_`` blocks.
        for key, value in modelconfig["environment_changes"].items():
            if (
                key
                not in [
                    "export_vars",
                    "module_actions",
                    "add_export_vars",
                    "add_module_actions",
                    "unset_vars",
                    "add_unset_vars",
                ]
                and "computer" in config
                and not overwrite
                # and run_or_compile=="runtime"
            ):
                # If the key is already included in ``env_vars``, the key variable has
                # been already modified by a previous model and a warning needs to be
                # raised. Do this check only on the first step (user interaction step).
                if key in env_vars and config["general"]["run_number"] == 1:
                    # Previous model with the same key
                    model0 = env_vars[key][1]
                    while True:
                        # Warn the user about the overwriting of the variable
                        esm_parser.user_note("Environment conflict", f"In '{model0}':")
                        esm_parser.pprint_config({key: env_vars[key][0]})
                        logging.info("\nIn '" + model + "':")
                        esm_parser.pprint_config({key: value})
                        # Ask the user how to proceed if it is not a `tidy job
                        if not config["general"]["jobtype"] == "tidy":
                            user_answer = input(
                                f"Environment variable '{key}' defined in '{model0}' is "
                                + "going to be overwritten by the one defined in "
                                + f"'{model}'. Are you okay with that? (Y/n): "
                            )
                        # If it is a ``tidy`` job, the user has already
                        # interacted and accepted the overwriting.
                        else:
                            user_answer = "Y"
                            logging.info(
                                f"Environment variable '{key}' defined in '{model0}' is "
                                + f"overwritten by the one defined in '{model}'."
                            )
                        # If the user selects ``Y``, overwrite the environment variable
                        if user_answer == "Y":
                            config[model]["env_overwrite"] = True
                            break
                        # If the user selects ``n`` raise a user error with recommendations
                        elif user_answer == "n":
                            config[model]["env_overwrite"] = False
                            esm_parser.user_error(
                                "Environment conflict",
                                "You were not happy with the environment variable "
                                + f"'{key}' in '{model0}' being overwritten by the same "
                                + f"variable in '{model}'. If you are running a "
                                + "coupled setup, we recommend that you resolve this "
                                + "conflict inside the coupled setup file, by "
                                + "specifying unconflicting environments for each "
                                + "model, or contact the setup developers.",
                            )
                        else:
                            logging.info("Wrong answer, please choose Y/n.")
                # Merge variable into the ``computer`` dictionary so that it becomes
                # part of the general environment.
                esm_parser.dict_merge(config["computer"], {key: value})
                # Add the variable to ``env_vars`` so it can be checked for conflicts
                # with other models.
                env_vars[key] = [value, model]


def _initialize_calendar(config):
    config = set_restart_chunk(config)
    config = set_leapyear(config)
    config = set_overall_calendar(config)
    if config["general"]["reset_calendar_to_last"]:
        config = find_last_prepared_run(config)
    config = set_most_dates(config)
    if not "iterative_coupling" in config["general"]:
        config["general"]["chunk_number"] = 1

        if config["general"]["run_number"] == 1:
            config["general"]["first_run_in_chunk"] = True
        else:
            config["general"]["first_run_in_chunk"] = False
        if config["general"]["next_date"] >= config["general"]["final_date"]:
            config["general"]["last_run_in_chunk"] = True
        else:
            config["general"]["last_run_in_chunk"] = False
    return config


def set_restart_chunk(config):
    nyear, nmonth, nday, nhour, nminute, nsecond = 0, 0, 0, 0, 0, 0
    nyear = int(config["general"].get("nyear", nyear))
    if not nyear:
        nmonth = int(config["general"].get("nmonth", nmonth))
    if not nyear and not nmonth:
        nday = int(config["general"].get("nday", nday))
    if not nyear and not nmonth and not nday:
        nhour = int(config["general"].get("nhour", nhour))
    if not nyear and not nmonth and not nday and not nhour:
        nminute = int(config["general"].get("nminute", nminute))
    if not nyear and not nmonth and not nday and not nhour and not nminute:
        nsecond = int(config["general"].get("nsecond", nsecond))
    if (
        not nyear
        and not nmonth
        and not nday
        and not nhour
        and not nminute
        and not nsecond
    ):
        nyear = 1
    config["general"]["nyear"] = nyear
    config["general"]["nmonth"] = nmonth
    config["general"]["nday"] = nday
    config["general"]["nhour"] = nhour
    config["general"]["nminute"] = nminute
    config["general"]["nsecond"] = nsecond
    return config


def set_leapyear(config):
    # make sure all models agree on leapyear
    if "leapyear" in config["general"]:
        for model in config["general"]["valid_model_names"]:
            config[model]["leapyear"] = config["general"]["leapyear"]
    else:
        for model in config["general"]["valid_model_names"]:
            if "leapyear" in config[model]:
                for other_model in config["general"]["valid_model_names"]:
                    if "leapyear" in config[other_model]:
                        if (
                            not config[other_model]["leapyear"]
                            == config[model]["leapyear"]
                        ):
                            print(
                                "Models "
                                + model
                                + " and "
                                + other_model
                                + " do not agree on leapyear. Stopping."
                            )
                            sys.exit(43)
                    else:
                        config[other_model]["leapyear"] = config[model]["leapyear"]
                config["general"]["leapyear"] = config[model]["leapyear"]
                break

    if not "leapyear" in config["general"]:
        for model in config["general"]["valid_model_names"]:
            config[model]["leapyear"] = True
        config["general"]["leapyear"] = True
    return config


def set_overall_calendar(config):

    # set the overall calendar
    if config["general"]["leapyear"]:
        config["general"]["calendar"] = Calendar(1)
    else:
        config["general"]["calendar"] = Calendar(0)
    return config


def find_last_prepared_run(config):

    calendar = config["general"]["calendar"]
    current_date = Date(config["general"]["current_date"], calendar)
    initial_date = Date(config["general"]["initial_date"], calendar)
    delta_date = (
        config["general"]["nyear"],
        config["general"]["nmonth"],
        config["general"]["nday"],
        config["general"]["nhour"],
        config["general"]["nminute"],
        config["general"]["nsecond"],
    )

    while True:
        if current_date < initial_date:
            break

        next_date = current_date.add(delta_date)
        end_date = next_date - (0, 0, 1, 0, 0, 0)

        datestamp = (
            current_date.format(form=9, givenph=False, givenpm=False, givenps=False)
            + "-"
            + end_date.format(form=9, givenph=False, givenpm=False, givenps=False)
        )

        # Solve base_dir with variables
        base_dir = esm_parser.find_variable(
            ["general", "base_dir"], config["general"]["base_dir"], config, [], True
        )
        expid = config["general"]["expid"]
        it_coupled_model_name = config["general"]["iterative_coupled_model"]

        if os.path.isdir(
            f"{base_dir}/{expid}/run_{it_coupled_model_name}{datestamp}"
        ):
            config["general"]["current_date"] = current_date
            return config

        current_date = current_date - delta_date

    print("ERROR: Could not find a prepared run.")
    sys.exit(42)


def set_most_dates(config):

    calendar = config["general"]["calendar"]
    if isinstance(config["general"]["current_date"], Date):
        current_date = config["general"]["current_date"]
    else:
        current_date = Date(config["general"]["current_date"], calendar)
    delta_date = (
        config["general"]["nyear"],
        config["general"]["nmonth"],
        config["general"]["nday"],
        config["general"]["nhour"],
        config["general"]["nminute"],
        config["general"]["nsecond"],
    )

    config["general"]["delta_date"] = delta_date
    config["general"]["current_date"] = current_date
    config["general"]["start_date"] = current_date
    config["general"]["initial_date"] = Date(
        config["general"]["initial_date"], calendar
    )
    config["general"]["final_date"] = Date(config["general"]["final_date"], calendar)
    config["general"]["prev_date"] = current_date - (0, 0, 1, 0, 0, 0)

    config["general"]["next_date"] = current_date.add(delta_date)
    config["general"]["last_start_date"] = current_date - delta_date
    config["general"]["end_date"] = config["general"]["next_date"] - (0, 0, 1, 0, 0, 0)

    config["general"]["runtime"] = (
        config["general"]["next_date"] - config["general"]["current_date"]
    )

    config["general"]["total_runtime"] = (
        config["general"]["next_date"] - config["general"]["initial_date"]
    )

    config["general"]["run_datestamp"] = (
        config["general"]["current_date"].format(
            form=9, givenph=False, givenpm=False, givenps=False
        )
        + "-"
        + config["general"]["end_date"].format(
            form=9, givenph=False, givenpm=False, givenps=False
        )
    )

    config["general"]["last_run_datestamp"] = (
        config["general"]["last_start_date"].format(
            form=9, givenph=False, givenpm=False, givenps=False
        )
        + "-"
        + config["general"]["prev_date"].format(
            form=9, givenph=False, givenpm=False, givenps=False
        )
    )
    return config


def _add_all_folders(config):
    all_filetypes = [
        "analysis",
        "config",
        "log",
        "mon",
        "couple",
        "scripts",
        "ignore",
        "unknown",
        "src",
    ]
    config["general"]["out_filetypes"] = [
        "analysis",
        "log",
        "mon",
        "scripts",
        "ignore",
        "unknown",
        "outdata",
        "restart_out",
    ]
    config["general"]["in_filetypes"] = [
        "scripts",
        "input",
        "forcing",
        "bin",
        "config",
        "restart_in",
    ]
    config["general"]["reusable_filetypes"] = config["general"].get(
        "reusable_filetypes", ["bin", "src"]
    )
    # Define the files that could be reusable accross runs (external files)
    config["general"]["potentially_reusable_filetypes"] = (
        all_filetypes + config["general"]["in_filetypes"]
    )
    # Apply changes from ``--update-files`` flag
    config = helpers.update_reusable_filetypes(config)

    experiment_dir = config["general"]["experiment_dir"]
    it_coupled_model_name = config["general"]["iterative_coupled_model"]
    datestamp = config["general"]["run_datestamp"]
    config["general"]["thisrun_dir"] = (
        f"{experiment_dir}/run_{it_coupled_model_name}{datestamp}"
    )

    for filetype in all_filetypes:
        config["general"]["experiment_" + filetype + "_dir"] = (
            config["general"]["experiment_dir"] + "/" + filetype + "/"
        )

    all_filetypes.append("work")
    config["general"]["all_filetypes"] = all_filetypes

    for filetype in all_filetypes:
        config["general"]["thisrun_" + filetype + "_dir"] = (
            config["general"]["thisrun_dir"] + "/" + filetype + "/"
        )

    config["general"]["work_dir"] = config["general"]["thisrun_work_dir"]

    all_model_filetypes = [
        "analysis",
        "bin",
        "config",
        "forcing",
        "input",
        "couple",
        "log",
        "mon",
        "outdata",
        "restart_in",
        "restart_out",
        "viz",
        "ignore",
    ]

    config["general"]["all_model_filetypes"] = all_model_filetypes

    for model in config["general"]["valid_model_names"]:
        for filetype in all_model_filetypes:
            if "restart" in filetype:
                filedir = "restart"
            else:
                filedir = filetype
            config[model]["experiment_" + filetype + "_dir"] = (
                config["general"]["experiment_dir"] + "/" + filedir + "/" + model + "/"
            )
            config[model]["thisrun_" + filetype + "_dir"] = (
                config["general"]["thisrun_dir"] + "/" + filedir + "/" + model + "/"
            )
            config[model]["all_filetypes"] = all_model_filetypes

    return config


def set_prev_date(config):
    import esm_parser

    """Sets several variables relevant for the previous date. Loops over all models in ``valid_model_names``, and sets model variables for:
    * ``prev_date``
    """
    for model in config["general"]["valid_model_names"]:
        if "time_step" in config[model] and not (
            isinstance(config[model]["time_step"], str)
            and "${" in config[model]["time_step"]
        ):
            config[model]["prev_date"] = config["general"]["current_date"] - (
                0,
                0,
                0,
                0,
                0,
                int(config[model]["time_step"]),
            )
            config[model]["last_parent_date"] = config[model]["prev_date"] - (
                0,
                0,
                0,
                0,
                0,
                int(config[model]["time_step"]),
            )

        # NOTE(PG, MAM): Here we check if the time step still has a variable which might be set in a different model, and resolve this case
        elif "time_step" in config[model] and (
            isinstance(config[model]["time_step"], str)
            and "${" in config[model]["time_step"]
        ):
            dt = esm_parser.find_variable(
                model, config[model]["time_step"], config, [], []
            )
            config[model]["prev_date"] = config["general"]["current_date"] - (
                0,
                0,
                0,
                0,
                0,
                int(dt),
            )
            config[model]["last_parent_date"] = config[model]["prev_date"] - (
                0,
                0,
                0,
                0,
                0,
                int(dt),
            )

        else:
            config[model]["prev_date"] = config["general"]["current_date"]
    return config


def set_parent_info(config):

    """Sets several variables relevant for the previous date. Loops over all models in ``valid_model_names``, and sets model variables for:
    * ``parent_expid``
    * ``parent_date``
    * ``parent_restart_dir``
    """

    # Make sure "ini_parent_dir" and "ini_restart_dir" both work:
    for model in config["general"]["valid_model_names"]:
        if not "ini_parent_dir" in config[model]:
            if "ini_restart_dir" in config[model]:
                config[model]["ini_parent_dir"] = config[model]["ini_restart_dir"]
        if not "ini_parent_exp_id" in config[model]:
            if "ini_restart_exp_id" in config[model]:
                config[model]["ini_parent_exp_id"] = config[model]["ini_restart_exp_id"]
        if not "ini_parent_date" in config[model]:
            if "ini_restart_date" in config[model]:
                config[model]["ini_parent_date"] = config[model]["ini_restart_date"]

    # check if parent is defined in esm_tools style
    # (only given for setup)
    setup = config["general"]["setup_name"]
    if not setup in config:
        setup = "general"
    if "ini_parent_exp_id" in config[setup]:
        for model in config["general"]["valid_model_names"]:
            if not "ini_parent_exp_id" in config[model]:
                config[model]["ini_parent_exp_id"] = config[setup]["ini_parent_exp_id"]
    if "ini_parent_date" in config[setup]:
        for model in config["general"]["valid_model_names"]:
            if not "ini_parent_date" in config[model]:
                config[model]["ini_parent_date"] = config[setup]["ini_parent_date"]
    if "ini_parent_dir" in config[setup]:
        for model in config["general"]["valid_model_names"]:
            if not "ini_parent_dir" in config[model]:
                config[model]["ini_parent_dir"] = (
                    config[setup]["ini_parent_dir"] + "/" + model
                )

    # Get correct parent info
    for model in config["general"]["valid_model_names"]:
        if config[model]["lresume"] == True and config["general"]["run_number"] == 1:
            config[model]["parent_expid"] = config[model]["ini_parent_exp_id"]
            if "parent_date" not in config[model]:
                config[model]["parent_date"] = config[model]["ini_parent_date"]
            config[model]["parent_restart_dir"] = config[model]["ini_parent_dir"]
        else:
            config[model]["parent_expid"] = config["general"]["expid"]
            if "parent_date" not in config[model]:
                config[model]["parent_date"] = config[model]["prev_date"]
            config[model]["parent_restart_dir"] = config[model][
                "experiment_restart_in_dir"
            ]
    return config


def add_vcs_info(config):
    """
    Adds version control system information in a plain text yaml file under the
    experiment log directory for both the models and the esm-tools.

    Parameters
    ----------
    config : dict
        The experiment configuration

    Returns
    -------
    config : dict
        The experiment configuration
    """
    exp_vcs_info_file = f"{config['general']['thisrun_log_dir']}/{config['general']['expid']}_vcs_info.yaml"
    logger.debug("Experiment information is being stored for usage under:")
    logger.debug(f">>> {exp_vcs_info_file}")
    vcs_versions = {}
    all_models = config.get("general", {}).get("models", [])
    for model in all_models:
        logger.debug(f"Locating {model}")
        try:
            model_dir = config[model]["model_dir"]
        except KeyError:
            # XIOS does not seem to define model_dir? Jan? What?
            vcs_versions[model] = f"Unable to locate model_dir for {model}."
            continue
        if helpers.is_git_repo(model_dir):
            vcs_versions[model] = helpers.get_all_git_info(model_dir)
        else:
            vcs_versions[model] = "Not a git-controlled model!"

    # NOTE(PG): There is no good way to get the repo directory from the config,
    # this may at least be a good start:
    esm_tools_repo = config.get("general", {}).get("esm_function_dir")
    if esm_tools_repo is not None:
        vcs_versions["esm_tools"] = helpers.get_all_git_info(f"{esm_tools_repo}/../")
    else:
        # FIXME(PG): This should absolutely never happen. The error message could use a better wording though...
        esm_parser.user_error("esm_tools doesn't know where it's own install location is. Something is very seriously wrong.")
    with open(exp_vcs_info_file, "w") as f:
        yaml.dump(vcs_versions, f)
    return config

def check_vcs_info_against_last_run(config):
    """
    Ensures that the version control info for two runs is identical between the
    current run and the previous run.


    This is checked by ensuring that the dictionary representation of the VCS
    files is identical. Differences will result in a ``user_error``, thus
    terminating the run. The check can be circumvented by setting::

        general:
            allow_vcs_differences: True

    Parameters
    ----------
    config : dict
        The experiment configuration

    Returns
    -------
    config : dict
        The experiment configuration
    """
    # FIXME(PG): Sometimes general.run_number is None (shows up as null in the
    # config), so this check is absolutely the worst way of doing it, but
    # whatever...
    if config['general']['run_number'] == 1 or config["general"]["run_number"] is None:
        return config  # No check needed on the very first run
    exp_vcs_info_file = f"{config['general']['thisrun_log_dir']}/{config['general']['expid']}_vcs_info.yaml"
    # FIXME(PG): This file might not exist if people erase every run folder...
    last_exp_vcs_info_file = f"{config['prev_run']['general']['thisrun_log_dir']}/{config['general']['expid']}_vcs_info.yaml"

    try:
        with open(exp_vcs_info_file, "r") as f:
            current_vcs_info = yaml.safe_load(f)
    except IOError:
        logger.warning(f"Unable to open {exp_vcs_info_file}, skipping check...")
        return config
    try:
        with open(last_exp_vcs_info_file, "r") as f:
            last_vcs_info = yaml.safe_load(f)
    except IOError:
        logger.warning(f"Unable to open {last_exp_vcs_info_file}, skipping_check...")
        return config
    if not config["general"].get("allow_vcs_differences", False) and current_vcs_info != last_vcs_info:
        esm_parser.user_error("VCS Differences", """
            You have differences in either the model code or in the esm-tools between two runs! 

            If you are **sure** that this is OK, you can set 'general.allow_vcs_differences' to True to avoid this check.
            """)


    return config


def finalize_config(config):
    config.finalize()
    return config


def add_submission_info(config):

    bs = batch_system.batch_system(config, config["computer"]["batch_system"])

    submitted = bs.check_if_submitted()
    if submitted:
        jobid = bs.get_jobid()
    else:
        jobid = os.getpid()

    config["general"]["submitted"] = submitted
    config["general"]["jobid"] = jobid
    return config


def initialize_batch_system(config):

    config["general"]["batch"] = batch_system.batch_system(
        config, config["computer"]["batch_system"]
    )

    return config


def initialize_coupler(config):
    if config["general"]["standalone"] == False:
        from . import coupler

        base_dir = config["general"]["base_dir"]
        expid = config["general"]["expid"]
        it_coupled_model_name = config["general"]["iterative_coupled_model"]
        datestamp = config["general"]["run_datestamp"]
        for model in list(config):
            if model in coupler.known_couplers:
                config["general"]["coupler_config_dir"] = (
                    f"{base_dir}/{expid}/"
                    f"run_{it_coupled_model_name}{datestamp}/config/{model}/"
                )
                config["general"]["coupler"] = coupler.coupler_class(config, model)
                break
        config["general"]["coupler"].add_files(config)
    return config


def set_logfile(config):
    log_file_path = (
        f"{config['general']['experiment_log_dir']}"
        f"/{config['general']['expid']}"
        f"_{config['general']['setup_name']}.log"
    )
    config["general"]["experiment_log_file"] = config["general"].get(
        "experiment_log_file", log_file_path
    )
    return config


def check_config_for_warnings_errors(config):
    """
    Wrapper method to call ``warn_error`` from the `ESM-Tools` recipes. Loops through
    the trigger variables to call ``warn_error``, only including ``warning`` in the
    ``triggers`` if it is an interactive session (i.e. user just submitted a job), or
    if it's not a resubmission (to avoid displaying the warning twice).

    .. note:: use this in the recipe after the ``choose_`` blocks are resolved,
       otherwise, warnings and errors nested in the ``choose_`` blocks won't be
       reached.

    Parameters
    ----------
    config : dict, esm_parser.ConfigSetup
            ConfigSetup object containing the information of the current simulation

    Returns
    -------
    config : dict, esm_parser.ConfigSetup
            ConfigSetup object containing the information of the current simulation
    """

    # Initialize the trigger variables (i.e. ``error`` and ``warning``))
    triggers = {"error": {"note_function": esm_parser.user_error}}

    # Find conditions to warn (avoid warning more than once)
    last_jobtype = config["general"].get("last_jobtype", "")
    isresubmitted = last_jobtype == config["general"]["jobtype"]
    isinteractive = config["general"].get("isinteractive", "")

    # Only warn if it is an interactive session or while submitted
    if not isresubmitted or isinteractive:
        triggers["warning"] = {"note_function": esm_parser.user_note}

    # Loop through the triggers
    for trigger, trigger_info in triggers.items():
        warn_error(config, trigger, trigger_info["note_function"])

    return config

def warn_error(config, trigger, note_function):
    """
    Checks the ``sections`` of the ``config`` for a given ``trigger`` (``"error"`` or
    ``"warning"``), and if found, returns the ``error`` or ``warning`` using
    ``note_functon`` (``user_error`` or ``user_note`` respectively). Errors always halt
    the core with ``sys.exit(1)``. Warnings only halt the code to ask the user for
    continuation if the job is interactive, it is specified inside the warning as
    ``ask_user_to_continue: True`` and the user has not called ``esm_runscripts`` with
    the ``--ignore-config-warnings`` or defined ``general.ignore_config_warnings: True``
    in their runscript.

    The syntax in the yaml files for triggering warnings or errors is as follows:
    .. code-block:: yaml

       warning/error:
           <name>: # Name for the specific warning or error
               message: "the message of the warning/error"
               esm_tools_version: ">/</=/!=/version_number" # trigger it under certain ESM-Tools version conditions
               ask_user_to_continue: True/False # Ask user about continuing or stopping the process, only for warnings, errors always kill the process

    Example
    -------
    .. code-block:: yaml

       recom:
           choose_scenario:
               HIST:
                   [ ... ]
               PI-CTRL:
                   [ ... ]
               "*":
                   add_warning:
                       "wrong scenario type":
                           message: "The scenario you specified (``${recom.scenario}``) is not supported!"
                           ask_user_to_continue: True

    If you then define ``recom.scenario: hist`` instead of ``HIST`` then you'll get the
    following:
    .. code-block::

       wrong scenario type WARNING
       ---------------------------
       Section: recom

       Wrong scenario, scenario hist does not exist

       ? Do you want to continue (set general.ignore_config_warnings: False to avoid quesitoning)?

    Parameters
    ----------
    config : dict, esm_parser.ConfigSetup
        ConfigSetup object containing the information of the current simulation
    trigger : string
        ``error`` or ``warning``
    note_function : esm_parser.user_error, esm_parser.user_note
        Method to report the note
    """
    # Sufixes for the warning special case
    if trigger=="warning":
        sufix_name = f" WARNING"
    else:
        sufix_name = f""

    # Loop through the sections (e.g. ``general``, ``<model>``, ``computer``, ...)
    for section, value in config.items():
        # If the trigger is found output the warnings or errors
        if trigger in value:
            actions = value[trigger]
            # Loop through the warnings or errors
            for action_name, action_info in actions.items():
                # Check if the version condition for the error/warning is met
                version_condition = action_info.get("esm_tools_version", ">0.0.0")
                if esm_utilities.check_valid_version(version_condition):
                    # Call the ``note_function`` (``user_error`` for errors,
                    # ``user_note`` for warnings))
                    note_function(
                        f"{action_name}{sufix_name}",
                        f'Section: ``{section}``\n\n{action_info.get("message", "")}'
                    )

                    # Check if the warning should halt execution and ask the user if
                    # if is an interactive session, the warning info specifies that i
                    # needs to halt, and the user has not defined the
                    # ``--ignore-config-warnings`` flag in the ``esm_runscripts`` call
                    if (
                        trigger=="warning"
                        and config["general"].get("isinteractive")
                        and action_info.get("ask_user_to_continue", False)
                        and not config["general"].get("ignore_config_warnings", False)
                    ):
                        proceed = questionary.confirm(
                            "Do you want to continue (set "
                            "general.ignore_config_warnings: False to avoid "
                            "quesitoning)?"
                        ).ask()
                        if not proceed:
                            sys.exit(1)
