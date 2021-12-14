import esm_parser

import os
import logging

FUNCTION_PATH = str(os.path.normpath(os.path.dirname(__file__) + "/../"))


# Specific to Runscripts
def runscripts_attach_single_config(config, attach_value):
    model, model_part = (
        attach_value.split(".")[0],
        ".".join(attach_value.split(".")[1:]),
    )
    logging.debug("Attaching: %s to %s", model, model_part)
    attach_single_config(config, model, attach_value)


def runscripts_attach_to_config_and_remove(config, attach_key):
    """
    Attaches extra dict to this one and removes the chapter

    Updates the dictionary on ``config`` with values from any file found under
    a specified by ``attach_key``.

    Parameters
    ----------
    config : dict
        The configuration to update
    attach_key : str
        A key who's value points to a list of various yaml files to update
        ``config`` with.

    Warning
    -------
    The ``config`` is modified **in place**!
    """
    if attach_key in config:
        attach_value = config[attach_key]
        if isinstance(attach_value, list):
            for attach_value in attach_value:
                runscripts_attach_single_config(config, attach_value)
        elif isinstance(attach_value, str):
            runscripts_attach_single_config(config, attach_value)
        else:
            raise TypeError("%s needs to have values of type list or str!" % attach_key)
        del config[attach_key]


def runscripts_check_conflicting_model_and_setup_names(config):
    all_names = list(config)
    all_model_names = config["general"]["valid_model_names"]
    for source_model in all_model_names:
        for target_model in all_names:
            if target_model in config[source_model]:
                raise KeyError(
                    "Please don't define %s in %s" % (target_model, source_model)
                )


def runscripts_update_models_from_setup(config):
    all_model_names = config["general"]["valid_model_names"]
    for model_name in all_model_names:
        model_config_in_setup = config["general"].pop(model_name, {})
        esm_parser.dict_merge(config[model_name], model_config_in_setup)
