import copy
import glob
import os
import sys
import yaml
import esm_parser

from loguru import logger

from esm_calendar import Date
from esm_runscripts import prev_run


def setup_correct_chunk_config(config):
    # to be called from the top of prepare

    if not config["general"].get("iterative_coupling", False):
        return config

    print("Initializing iterative coupling")

    chunk_config = _restore_original_config(config)
    chunk_config = _initialize_chunk_date_file(
        chunk_config
    )  # make sure file exists and points to NEXT run
    chunk_config = _read_chunk_date_file_if_exists(chunk_config)

    #   if _called_from_tidy_job(chunk_config):
    #        chunk_config = _update_chunk_date_file(chunk_config)

    chunk_config = _set_model_queue(chunk_config)
    # chunk_config = set_runs_per_chunk(chunk_config)
    config = _store_original_config(chunk_config)

    return config


def _update_run_in_chunk(config):
    if not config["general"].get("iterative_coupling", False):
        return config

    config = _is_first_run_in_chunk(config)
    config = _is_last_run_in_chunk(config)
    config = _find_next_model_to_run(config)
    config = _find_next_chunk_number(config)
    return config


def set_chunk_calendar(config):
    if not config["general"].get("iterative_coupling", False):
        return config

    delta_date = (
        config["general"]["nyear"],
        config["general"]["nmonth"],
        config["general"]["nday"],
        config["general"]["nhour"],
        config["general"]["nminute"],
        config["general"]["nsecond"],
    )

    nyear, nmonth, nday, nhour, nminute, nsecond = 0, 0, 0, 0, 0, 0
    chunk_unit = config["general"].get("this_chunk_unit", "years")

    if chunk_unit in ["years", "yrs", "a"]:
        nyear = int(config["general"].get("this_chunk_size"))
    elif chunk_unit in ["kiloyears", "kyrs", "ka"]:
        nyear = int(config["general"].get("this_chunk_size") * 1000)
    elif chunk_unit in ["months"]:
        nmonth = int(config["general"].get("this_chunk_size"))
    elif chunk_unit in ["days"]:
        nday = int(config["general"].get("this_chunk_size"))
    if (
        not nyear
        and not nmonth
        and not nday
        and not nhour
        and not nminute
        and not nsecond
    ):
        nyear = 1

    chunk_delta_date = (nyear, nmonth, nday, nhour, nminute, nsecond)

    oneday = (0, 0, 1, 0, 0, 0)

    number_of_models = config["general"]["number_of_ic_models"]
    this_chunk_number = int(config["general"]["next_chunk_number"]) - 1
    initial_date = config["general"]["initial_date"]
    next_date = config["general"]["next_date"]

    number_of_chunks_done = this_chunk_number // number_of_models

    # LA: get right number_of_chunks_done for last year in chunk
    if "model_queue" in config["general"]:
        if (
            config["general"]["model_queue"][0] == "model1"
            and config["general"].get("last_run_in_chunk")
        ):
            number_of_chunks_done = number_of_chunks_done - 1

    passed_time = (
        number_of_chunks_done * chunk_delta_date[0],
        number_of_chunks_done * chunk_delta_date[1],
        number_of_chunks_done * chunk_delta_date[2],
        number_of_chunks_done * chunk_delta_date[3],
        number_of_chunks_done * chunk_delta_date[4],
        number_of_chunks_done * chunk_delta_date[5],
    )

    chunk_start_date = initial_date + passed_time
    chunk_end_date = chunk_start_date + chunk_delta_date

    config["general"]["chunk_start_date"] = chunk_start_date
    config["general"]["chunk_end_date"] = chunk_end_date - oneday

    runs_per_chunk = 0
    start_date = chunk_start_date
    while start_date < chunk_end_date:
        start_date = start_date + delta_date
        runs_per_chunk += 1

    if not start_date == chunk_end_date:
        setup_name = config["general"]["setup_name"]
        print(f"Chunk_size is not a multiple of run size for model {setup_name}.")
        sys.exit(-1)

    if runs_per_chunk == 1:
        config["general"]["run_in_chunk"] = "first_and_last"

    config = _update_run_in_chunk(config)
    if config["general"]["last_run_in_chunk"]:
        config["general"]["next_run_in_chunk"] = "first"
    elif next_date + delta_date >= chunk_end_date:
        config["general"]["next_run_in_chunk"] = "last"
    else:
        config["general"]["next_run_in_chunk"] = "middle"

    return config


def _update_chunk_date_file(config):
    if not config["general"].get("iterative_coupling", False):
        return config

    # to be called at the end of tidy
    with open(config["general"]["chunk_date_file"], "w+") as chunk_dates:
        chunk_dates.write(
            str(config["general"]["next_chunk_number"])
            + " "
            + config["general"]["next_setup_name"]
            + " "
            + config["general"]["next_run_in_chunk"]
        )
    # config["general"]["setup_name"] = config["general"]["next_setup_name"]
    # config["general"]["chunk_number"] = config["general"]["next_chunk_number"]
    return config


def update_command_line_config(config):
    if not config["general"].get("iterative_coupling", False):
        return config

    next_log_file = (
        config["general"]["experiment_scripts_dir"]
        + "/"
        + config["general"]["expid"]
        + "_"
        + config["general"]["next_setup_name"]
        + ".date"
    )

    if config["general"]["next_run_in_chunk"] == "first":
        if os.path.isfile(next_log_file):
            with open(next_log_file, "r") as date_file:
                next_start_date, next_run_number = date_file.read().split()

                config["general"]["command_line_config"][
                    "current_date"
                ] = next_start_date
                config["general"]["command_line_config"]["run_number"] = int(
                    next_run_number
                )
        else:
            config["general"]["command_line_config"]["current_date"] = None
            config["general"]["command_line_config"]["run_number"] = 1

    config = _update_chunk_date_file(config)

    return config


def prev_chunk_info(config):
    """
    Loads the ``finished_config`` yaml of the previous model chunks into
    ``config["prev_run_{model}"]`` so that they can be called from other chunks.

    **Example**

    Let's say we are running fesom-recom-medusa. In this offline coupling,
    ``fesom-recom`` are run in separate chunks from ``medusa`` (``fesom-recom`` is
    offline-coupled to ``medusa``). Let's assume we are currently running a
    ``medusa`` chunk but we still want to access from our ``medusa.yaml`` a variable
    defined in a file from fesom-recom files. This function allows you to do so by using
    the following syntax:

    .. code-block:: yaml

       medusa:
           fesom_start_date: "${prev_chunk_fesom.general.start_date}"

    Parameters
    ----------
    config : dict, esm_parser.ConfigSetup
        ConfigSetup object containing the information of the current simulation.
    """
    expid = config["general"]["expid"]
    base_dir = config["general"]["base_dir"]
    chunk_number = config["general"]["chunk_number"]
    model_named_queue = config["general"]["model_named_queue"]
    setup_name = config["general"]["setup_name"]

    if chunk_number <= 1:
        return

    prev_chunk_models = []
    for model in model_named_queue:
        if model == setup_name:
            continue

        # prev_chunk finished_config
        model_date, run_number = _read_model_date_file(config, model)
        finished_config = _find_model_finished_config(config, model, model_date)

        # Load prev_chunk info
        # TODO: this needs to be an instantiation ofr prev_run, once prev_run is
        # generalized
        if finished_config:
            with open(finished_config, "r") as fc:
                config[f"prev_chunk_{model}"] = yaml.load(fc, Loader=yaml.FullLoader)


########################################   END OF API ###############################################


def _called_from_tidy_job(config):  # not called from anywhere
    """
    At the beginning of a prepare job, the date file isn't read yet,
    so run_number doesn't exist. At the end of a tidy job it does...
    Don't know if that is the best criterium to use. DB
    """
    if "general" in config:
        if "run_number" in config["general"]:
            return True
    return False


def _restore_original_config(config):
    if "general" in config:
        if "original_config" in config["general"]:
            resubmit = True
            return copy.deepcopy(config["general"]["original_config"])  # , resubmit
    resubmit = False
    # return config, resubmit
    return config


def _store_original_config(config):
    new_config = {}
    new_config = {"original_config": copy.deepcopy(config)}
    config["general"].update(new_config)
    return config


def _read_chunk_date_file_if_exists(config):
    config["general"]["chunk_date_file"] = (
        config["general"]["base_dir"]
        + "/"
        + config["general"]["expid"]
        + "/scripts/"
        + config["general"]["expid"]
        + "_chunk_date"
    )

    if os.path.isfile(config["general"]["chunk_date_file"]):
        with open(config["general"]["chunk_date_file"], "r") as chunk_dates:
            chunk_number, setup_name, run_in_chunk = chunk_dates.read().split()

        config["general"]["setup_name"] = setup_name
        config["general"]["chunk_number"] = int(chunk_number)
        config["general"]["run_in_chunk"] = run_in_chunk

        index = 1

        while "model" + str(index) in config:
            if config["model" + str(index)]["setup_name"] == setup_name:
                config["general"]["this_chunk_size"] = int(
                    config["model" + str(index)]["chunk_size"]
                )
                config["general"]["this_chunk_unit"] = config["model" + str(index)][
                    "chunk_unit"
                ]
                break
            index += 1

    return config


def _read_model_date_file(config, model):
    """
    Extracts the current date and run number of a ``model`` from the ``.date`` file
    (i.e. the ESM-Tools clock) in ``<base_dir>/<expid>/scripts`` folder.

    Parameters
    ----------
    config : dict, esm_parser.ConfigSetup
        ConfigSetup object containing the information of the current simulation.
    model : str
        Name of the model.

    Returns
    -------
    model_date : str
        Date read from the ``.date`` file, only the file is found. Otherwise returns
        nothing.
    run_number : int
        Run number read from the ``.date`` file, only the file is found. Otherwise returns
        nothing.
    """
    expid = config["general"]["expid"]
    base_dir = config["general"]["base_dir"]
    model_date_file = f"{base_dir}/{expid}/scripts/{expid}_{model}.date"

    if os.path.isfile(model_date_file):
        with open(model_date_file, "r") as model_dates:
            model_date, run_number = model_dates.read().split()

        return model_date, run_number


def _find_model_finished_config(config, model, model_date):
    """
    Finds the ``finished_config`` yaml path of a ``model`` matching the ``model_date``
    provided.

    Parameters
    ----------
    config : dict, esm_parser.ConfigSetup
        ConfigSetup object containing the information of the current simulation.
    model : str
        Name of the model.
    model_date : str
        Date of the current model's time in the format ``<YYYYMMDD>`` or
        ``<YYYYMMDD>T<HH:MM:SS>``

    Returns
    -------
    full_file_path : str
        Path of the ``finished_config`` for the model, matching the date. Returns
        ``None`` if no file is found.

    Raises
    ------
    loguru.logger.error :
        If a more than one file matches the date.
    """
    expid = config["general"]["expid"]
    base_dir = config["general"]["base_dir"]
    file_path = f"{base_dir}/{expid}/config/{expid}_{model}_finished_config.yaml"
    time_stamps = [f"_*-{model_date[:8]}", f"_*-{model_date}", ""]

    for time_stamp in time_stamps:
        full_file_path = glob.glob(f"{file_path}{time_stamp}")
        if len(full_file_path) == 1:
            return full_file_path[0]

        elif len(full_file_path) > 1:
            logger.error(
                "There is more than one finished_config file matching the criteria: "
                "{full_file_path}"
            )
            raise


def _initialize_chunk_date_file(config):
    config["general"]["setup_name"] = config["model1"]["setup_name"]
    config["general"]["chunk_number"] = 1
    config["general"]["run_in_chunk"] = "first"
    config["general"]["this_chunk_size"] = int(config["model1"]["chunk_size"])
    config["general"]["this_chunk_unit"] = config["model1"]["chunk_unit"]
    return config


def _set_model_queue(config):
    index = 1
    model_queue = []
    model_named_queue = []

    while "model" + str(index) in config:
        model_queue += ["model" + str(index)]
        model_named_queue += [config["model" + str(index)]["setup_name"]]
        index += 1

    config["general"]["number_of_ic_models"] = index - 1

    index = model_named_queue.index(config["general"]["setup_name"]) + 1
    index = index % len(model_queue)

    config["general"]["model_queue"] = model_queue[index:] + model_queue[:index]
    config["general"]["model_named_queue"] = (
        model_named_queue[index:] + model_named_queue[:index]
    )

    return config


def _is_first_run_in_chunk(config):
    if config["general"]["run_in_chunk"] in ["first", "first_and_last"]:
        config["general"]["first_run_in_chunk"] = True
    else:
        config["general"]["first_run_in_chunk"] = False
    return config


def _is_last_run_in_chunk(config):
    if config["general"]["run_in_chunk"] in ["last", "first_and_last"]:
        config["general"]["last_run_in_chunk"] = True
    else:
        config["general"]["last_run_in_chunk"] = False
    return config


def _find_next_model_to_run(config):
    if config["general"]["last_run_in_chunk"]:
        config["general"]["next_setup_name"] = config["general"]["model_named_queue"][0]
    else:
        config["general"]["next_setup_name"] = config["general"]["setup_name"]
    return config


def _find_next_chunk_number(config):
    if config["general"]["last_run_in_chunk"]:
        config["general"]["next_chunk_number"] = config["general"]["chunk_number"] + 1
    else:
        config["general"]["next_chunk_number"] = config["general"]["chunk_number"]
    return config
