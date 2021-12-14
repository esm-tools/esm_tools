import os
import shutil
import yaml

from loguru import logger

from .tests import *
from .read_shipped_data import *

# Bold strings
bs = "\033[1m"
be = "\033[0m"


def user_config(info):
    """
    Reads the ``user_config.yaml`` file that contains both the account information and
    the testing directory for the tests. If the file does not exists it asks the user
    for the information.

    Parameters
    ----------
    info : dict
        Dictionary that contains the testing info, including the user defined options.

    Returns
    -------
    info : dict
        Same as input but with a nested ``user`` dictionary.
    """
    # If user info is not needed return None
    if info["ignore_user_info"]:
        info["user"] = None
        return info

    # Check for user configuration file
    user_config = f"{info['script_dir']}/user_config.yaml"
    logger.info("")
    if not os.path.isfile(user_config):
        # Make the user configuration file
        answers = {}
        logger.info(
            f"{bs}Welcome to ESM-Tests! Automatic testing for ESM-Tools devs\n"
            + f"**********************************************************{be}\n"
            + "Please answer the following questions. If you ever need to change the "
            + "configuration, you can do that in the the esm_tests/user_config.yaml\n"
        )
        try:
            answers["account"] = input(
                "What account will you be using for testing? (default: None) "
            )
        except EOFError:
            if os.environ.get("CI"):
                logger.info(
                    "This is probably running on the CI System. We will default to None"
                )
                answers["account"] = None
            else:
                raise
        if not answers["account"] or answers["account"] == "None":
            answers["account"] = None
        try:
            answers["test_dir"] = input(
                "In which directory would you like to run the tests? "
            )
        except EOFError:
            if os.environ.get("CI"):
                logger.info(
                    f"This is probably running on the CI System. We will default to {os.getcwd()}"
                )
                answers["test_dir"] = os.getcwd()
            else:
                raise
        with open(user_config, "w") as uc:
            logger.debug("Writing file")
            out = yaml.dump(answers)
            uc.write(out)
            logger.debug(f"Done: {uc}")

    # Load the user info
    with open(user_config, "r") as uc:
        user_info = yaml.load(uc, Loader=yaml.FullLoader)
    logger.info(f"{bs}Running tests with the following configuration:{be}")
    logger.info(f"{bs}-----------------------------------------------{be}")
    yprint(user_info)

    info["user"] = user_info

    return info


def get_scripts(info):
    """
    Get path and names of the runscripts to be tested and store it in the ``info``
    dictionary. If the file ``esm_tools/src/esm_tests/test_config.yaml`` exists, it
    only loads the information from runscripts specified there. Otherwise, it loads all
    the runscripts available in ``esm_tools/src/resources/runscripts``.

    The structur of the ``test_config.yaml`` should be::

    .. code-block:: yaml

        <model1>:       # A list to specify runscript by runscript
            - <runscript1>.yaml
            - <runscript2>.yaml
        <model2>: all   # To run all runscripts of the model
        [ ... ]

    Parameters
    ----------
    info : dict
        Dictionary that contains the testing info.

    Returns
    -------
    info : dict
        Same as input but with a nested ``scripts`` dictionary.
    """
    for key, value in info.items():
        logger.debug(f"key {key}: value {value}")
    # Get the path for the testing runscripts
    runscripts_dir = get_runscripts_dir()
    # Initialize the scripts_info dictionary
    scripts_info = {}
    # Counter for calculating percentages
    ns = 0

    # Load test info
    test_config = f"{info.get('script_dir', os.getcwd())}/test_config.yaml"
    # Check if there is a user-defined dictionary, where the user has specified which
    # runscripts to run. If the file exists load it into the ``test_info`` dictionary,
    # if not, set ``test_info`` as an empty dictionary.
    if os.path.isfile(test_config):
        with open(test_config, "r") as t:
            test_info = yaml.load(t, Loader=yaml.FullLoader)
    else:
        test_info = {}
    logger.debug(test_info)

    # Check if all runscripts need to be tested
    if len(test_info) > 0:
        test_all = False
    else:
        test_all = True

    # Loop through the models in the runscript folder
    for model in os.listdir(runscripts_dir):
        # Check if the users wants to run this model
        if test_all or test_info.get(model, False):
            # Check computer
            model_config = f"{runscripts_dir}/{model}/config.yaml"
            if not os.path.isfile(model_config):
                logger.error(f"'{model_config}' not found!")
            with open(model_config, "r") as c:
                config_test = yaml.load(c, Loader=yaml.FullLoader)
            computers = config_test.get("computers", False)
            if computers:
                # If this runscript is not to be evaluated in this computer, move on
                if info["this_computer"] not in computers:
                    continue
            # Load files to ignore
            ignore_scripts = config_test.get("ignore_scripts", [])
            ignore_scripts.append("config.yaml")

            scripts_info[model] = {}
            # Loop through the testing runscripts of this model
            for script in os.listdir(f"{runscripts_dir}/{model}"):
                # Check if the runscript needs to be tested
                if (
                    test_all
                    or isinstance(test_info.get(model), str)
                    or script in test_info.get(model, [])
                ) and os.path.isfile(f"{runscripts_dir}/{model}/{script}"):
                    # Check that it is actually a runscript
                    if (
                        not any(script in s for s in ignore_scripts)
                        and ".swp" not in script
                    ):
                        # Store information about the runscript
                        scripts_info[model][script.replace(".yaml", "")] = {}
                        scripts_info[model][script.replace(".yaml", "")][
                            "path"
                        ] = f"{runscripts_dir}/{model}/{script}"
                        scripts_info[model][script.replace(".yaml", "")]["state"] = {}
                        ns += 1
            # Delete models that do not contain any script
            if not scripts_info[model]:
                del scripts_info[model]

    scripts_info["general"] = {"num_scripts": ns}

    info["scripts"] = scripts_info

    return info


def read_info_from_rs(info):
    """
    Loads the following information from the runscripts to be tested:
        * ``general.version`` (or ``general.comp_version``)
        * ``general.comp_command`` (if exists)

    Parameters
    ----------
    info : dict
        Dictionary that contains the testing info.

    Returns
    -------
    info : dict
        Same as input but but including the new information from the runscripts.
    """
    scripts_info = info["scripts"]
    new_loader = create_env_loader()
    # Loop through the models
    for model, scripts in scripts_info.items():
        if model == "general":
            continue
        # Loop through the model's runscripts
        for script, v in scripts.items():
            # Open the script and load variables
            with open(v["path"], "r") as rs:
                runscript = yaml.load(rs, Loader=yaml.SafeLoader)
            v["version"] = runscript["general"].get(
                "comp_version", runscript[model]["version"]
            )
            v["comp_command"] = runscript["general"].get("comp_command", None)
            # Data for iterative coupling
            v["iterative_coupling"] = runscript["general"].get(
                "iterative_coupling", False
            )
            if v["iterative_coupling"]:
                v["nmodels_iterative_coupling"] = 0
                v["iterative_models"] = {}
                for model in runscript:
                    if "model" in model:
                        v["nmodels_iterative_coupling"] += 1
                        # Store the name of the subscripts so that they can be run in
                        # checks
                        v["iterative_models"][model] = {
                            "script": runscript[model]["runscript"]
                        }
            else:
                v["nmodels_iterative_coupling"] = 1

    info["scripts"] = scripts_info

    return info


def del_prev_tests(info):
    """
    Deletes the previews tests that coincide with the ones that are to be run this
    time.

    Parameters
    ----------
    info : dict
        Dictionary that contains the testing info.
    """
    scripts_info = info["scripts"]
    user_info = info["user"]
    logger.debug("Deleting previous tests")
    # Loop through the model's runscripts
    for model, scripts in scripts_info.items():
        if model == "general":
            continue
        # Loop through the model's runscripts
        for script, v in scripts.items():
            # If the compilation directory for this runscript exists, remove it
            if os.path.isdir(
                f"{user_info['test_dir']}/comp/{model}/{model}-{v['version']}"
            ):
                shutil.rmtree(
                    f"{user_info['test_dir']}/comp/{model}/{model}-{v['version']}"
                )
                if len(os.listdir(f"{user_info['test_dir']}/comp/{model}")) == 0:
                    shutil.rmtree(f"{user_info['test_dir']}/comp/{model}")
            # If the run directory for this runscript exists, remove it
            if os.path.isdir(f"{user_info['test_dir']}/run/{model}/{script}"):
                shutil.rmtree(f"{user_info['test_dir']}/run/{model}/{script}")
                if len(os.listdir(f"{user_info['test_dir']}/run/{model}")) == 0:
                    shutil.rmtree(f"{user_info['test_dir']}/run/{model}")
