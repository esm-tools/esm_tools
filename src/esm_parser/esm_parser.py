#!/usr/bin/env python
"""
=======================================
``YAML`` Parser for Earth System Models
=======================================

One core element of the ``esm-tools`` is the description of model
configurations and experiments with the aid of ``YAML`` files. Beyond the
standard features of ``YAML``, several specific conventions have been
implemented to ease the description of your simulations. These conventions are
described below, and the functions which implement them are documented with
minimal examples. Internally, after parsing the ``YAML`` files are converted
into a single Python dictionary.

Parsing takes place by initializing objects which represent either an entire
setup, ``ConfigSetup``, or a specific component, ``ConfigComponent``. Both of
these objects base off of ``GeneralConfig``, which is a dictionary subclass
performing specific parsing steps during the object's creation. The parsing
steps are presented in the order that they are resolved:

When initializing a ``ConfigSetup`` or ``ConfigComponent``, a name of the
desired setup or component must be given, e.g. ``"awicm"`` or ``"echam"``. This
configuration is immediately loaded along with any further configs listed in
the section "further_reading". Note that this means that **any configuration
listed in "further_reading" must not contain any variables!!**

Following this step, a method called ``_config_init`` is run for all classes
based off of ``GeneralConfig``. For components, any entries listed under
``"include_submodels"`` are attached and registed under a new keyword
``"submodels"``.

For setups, the next step is to determine the computing host and load the
appropriate configuration files. Setups divide their configuration into 3
specific parts:

#. Setup information, contained under ``config['setup']``. This includes, e.g.
   information regarding a standalone setup, possible coupling, etc.
#. Model Information, under ``config['model']``. This contains specific
   information for all models and submodels, such as resolution, input file
   names, namelists, etc.
#. User information, under ``config['model']``. The user can specify to
   override any of the defaults with their own choices.

In the next step, all keys starting with ``"choose_"`` are determined, along
with any information they set. This is done first for the setup, and then for
the models. These are filtered to determine an independent choice, and if
cyclic dependencies occur, an error is raised. All choices are then resolved
until nothing is left.


-------

Specific documentation for classes and functions are given below:
"""
# Python 2 and 3 version agnostic compatiability:
from __future__ import print_function
from __future__ import unicode_literals
from __future__ import division
from __future__ import absolute_import

import pdb

# Python Standard Library imports
import collections
import copy
import logging
import os
import re
import shutil
import socket
import subprocess
import sys
import warnings
import numpy

# Always import externals before any non standard library imports

# Third-Party Imports
import coloredlogs
import colorama
import yaml
import six

# functions reading in dict from file
from .shell_to_dict import *
from .yaml_to_dict import *

# Date class
from esm_calendar import Date

# Loader for package yamls
import esm_tools

# Logger and related constants
logger = logging.getLogger("root")
DEBUG_MODE = logger.level == logging.DEBUG
FORMAT = (
    "[%(asctime)s,%(msecs)03d:%(filename)s:%(lineno)s - %(funcName)20s() ] %(message)s"
)
# f_handler = logging.FileHandler("file.log")
# f_handler.setFormatter(FORMAT)
# logger.addHandler(f_handler)


# Module Constants:
CONFIGS_TO_ALWAYS_ATTACH_AND_REMOVE = ["further_reading"]
# NOTE: For very strange reasons, DATE_MARKER ends up being unicode in py2, not a string...
DATE_MARKER = str(">>>THIS_IS_A_DATE<<<")


import esm_rcfile


FUNCTION_PATH = esm_rcfile.EsmToolsDir("FUNCTION_PATH")
SETUP_PATH = FUNCTION_PATH + "/setups"
DEFAULTS_DIR = FUNCTION_PATH + "/defaults"
COMPONENT_PATH = FUNCTION_PATH + "/components"


esm_function_dir = FUNCTION_PATH
esm_namelist_dir = esm_rcfile.EsmToolsDir("NAMELIST_PATH")
esm_runscript_dir = esm_rcfile.EsmToolsDir("RUNSCRIPT_PATH")

gray_list = [
    r"choose_lresume",
    r"choose_.*lresume",
    r"lresume",
    r".*date$",
    r".*date!(year|month|day|hour|minute|second)",
    r".*date!(syear|smonth|sday|shour|sminute|ssecond|sdoy)",
    r"parent_date",
    r"parent_expid",
    r"parent_restart_dir",
]

gray_list = [re.compile(entry) for entry in gray_list]
gray_list = []
constant_blacklist = [r"PATH", r"LD_LIBRARY_PATH", r"NETCDFF_ROOT", r"I_MPI_ROOT"]

constant_blacklist = [re.compile(entry) for entry in constant_blacklist]

protected_adds = ["add_module_actions", "add_export_vars", "add_unset_vars"]
keep_as_str = ["branch"]
early_choose_vars = ["include_models", "version", "omp_num_threads"]

# Ensure FileNotFoundError exists:
if six.PY2:  # pragma: no cover
    FileNotFoundError = IOError


def look_for_file(model, item, all_config=None):
    """
    Finds the file containing the configuration of a component, model, coupled setup
    or software included in `ESM-Tools`. The ``model`` input provides a general name,
    normally the folder where all the versioned files of that component are contained.
    The ``item`` input can contain information about the version. If the configuration
    file is not found, ``item`` will be reduced one ``-`` back and ``look_for_file``
    will be called recursively.

    Parameters
    ----------
    model : str
        General name of the model, component, coupled setup or software.
    item : str
        Name of the component and the version needed.
    all_config : dict
        main configuration

    Returns
    -------
    possible_path : str
        Path to the configuration file.
    needs_loading : bool
        Boolean to indicate the need of loading the file.
    """
    # look at the directory where yaml runscript is found. This is an absolute
    # path
    runscript_path = ""
    if all_config and all_config["general"].get("runscript_abspath"):
        runscript_path = os.path.dirname(all_config["general"]["runscript_abspath"])

    # Loop through all possible path combinations
    possible_paths = [
        f"{SETUP_PATH}/{model}/{item}",
        f"{COMPONENT_PATH}/{model}/{item}",
        f"{FUNCTION_PATH}/esm_software/{model}/{item}",
        f"{FUNCTION_PATH}/other_software/{model}/{item}",
        f"{FUNCTION_PATH}/{model}/{item}",
        f"{runscript_path}/{item}",
        f"{os.getcwd()}/{item}",  # last resort: look at the CWD if others fail
    ]

    endings = ["", ".yaml", ".yml", ".YAML", ".YML"]

    for possible_path in possible_paths:

        for ending in endings:
            file_path = possible_path + ending

            # Check if the file exists and if it does return its path
            if os.path.isfile(file_path):
                needs_loading = True
                return file_path, needs_loading

    # If the item is a subversion of a model version with its own file (e.g.
    # item = fesom-2.0-jio and model = fesom), the previous lines won't be able
    # to find the versioned file (e.g. fesom-2.0.yaml) cause it is looking for
    # a file which name contains the whole item string (e.g. fesom-2.0-jio.yaml).
    # To solve that kind of problem the item's name is reduced to the last "-"
    # (e.g. to fesom-2.0) and then ``look_for_file`` is called recursively
    new_item = "-".join(item.split("-")[:-1])
    if len(new_item) > 0:
        possible_path, needs_loading = look_for_file(model, new_item)
        if possible_path:
            return possible_path, needs_loading

    # The file was not found
    warnings.warn(f'File for "{item}" not found in "{model}"')
    return None, False


def shell_file_to_dict(filepath):
    """
    Generates a ~`ConfigSetup` from an old shell script.

    See also ~`ShellscriptToUserConfig`

    Parameters
    ----------
    filepath : str
        The file to load

    Returns
    -------
    ConfigSetup :
        The parsed config.
    """
    config = ShellscriptToUserConfig(filepath)
    config = complete_config(config)
    return config


def initialize_from_shell_script(filepath):
    config = ShellscriptToUserConfig(filepath)
    config = complete_config(config)
    return config


def initialize_from_yaml(filepath):
    for file_ending in YAML_AUTO_EXTENSIONS:
        if filepath.endswith(file_ending) and not file_ending == "":
            user_config = yaml_file_to_dict(filepath)

            if not "general" in user_config:
                user_config["general"] = {}
            # full absolute path of the user runscript including the yaml file
            user_config["general"]["runscript_abspath"] = filepath

            user_config = complete_config(user_config)
    return user_config


def complete_config(user_config):
    if not "general" in user_config:
        user_config["general"] = {}
    user_config["general"]["additional_files"] = []

    while True:
        for model in list(user_config):
            if "further_reading" in user_config[model]:
                if type(user_config[model]["further_reading"]) == list:
                    for additional_file in user_config[model]["further_reading"]:
                        if (
                            not additional_file
                            in user_config["general"]["additional_files"]
                        ):
                            user_config["general"]["additional_files"].append(
                                additional_file
                            )
                elif type(user_config[model]["further_reading"]) == str:
                    additional_file = user_config[model]["further_reading"]
                    if (
                        not additional_file
                        in user_config["general"]["additional_files"]
                    ):
                        user_config["general"]["additional_files"].append(
                            additional_file
                        )

                if model == "general":
                    user_config["further_reading"] = user_config["general"][
                        "further_reading"
                    ]
                    del user_config["general"]["further_reading"]
                    attach_to_config_and_remove(
                        user_config, "further_reading", all_config=user_config
                    )
                else:
                    attach_to_config_and_remove(
                        user_config[model], "further_reading", all_config=user_config
                    )

        found = False
        for model in user_config:
            if "further_reading" in user_config[model]:
                found = True
                break
        if not found:
            break

    return user_config


def pprint_config(config):  # pragma: no cover
    """
    Prints the dictionary given to the stdout in a nicely formatted YAML style.

    Parameters
    ----------
    config : dict
        The configuration to print

    Returns
    -------
    None
    """
    # Delete the ``prev_run`` chapter: we don't need to print the info from the
    # previous run.
    config_to_print = copy.deepcopy(config)  # PrevRunInfo
    if "prev_run" in config_to_print:  # PrevRunInfo
        del config_to_print["prev_run"]  # PrevRunInfo
    yaml.Dumper.ignore_aliases = lambda *args: True
    print(yaml.dump(config_to_print, default_flow_style=False))


def attach_to_config_and_reduce_keyword(
    config_to_read_from,
    config_to_write_to,
    full_keyword,
    reduced_keyword="included_files",
    level_to_write_to=None,
):
    """
    Attaches a new dictionary to the config, and registers it as the value of
    ``reduced_keyword``.

    Parameters
    ----------
    config_to_read_from : dict
        The configuration dictionary from which information is read from. The
        keyword from which additional YAML files are read from should be on the
        top level of this dictionary.
    config_to_write_to : dict
        The dictionary where the contents of
        ``config_to_read_from[full_keyword]`` is written in.
    full_keyword :
        The keyword where contents are extracted from
    reduced_keyword :
        The keyword where the contents of ``config_to_read_from[full_keyword]``
        are written to
    level_to_write_to :
        If this is specified, the attached entries are written here instead of
        in the top level of ``config_to_write_to``. Note that only one level
        down is currently supported.


    The purpose behind this is to have a chapter in config "include_submodels"
    = ["echam", "fesom"], which would then find the "echam.yaml" and
    "fesom.yaml" configs, and attach them to "config" under config[submodels],
    and the entire config for e.g. echam would show up in config[echam

    Since ``config_to_read_from`` and ``config_to_write_to`` are ``dict``
    objects, they are modified **in place**. Note also that the entry
    ``config_to_read_from[full_keyword]`` is deleted at the end of the routine.

    If the entry in ``config_to_read_from[full_keyword]`` is a list, each item
    in that list is split into two parts: ``model`` and ``model_part``. For example:

    >>> # Assuming: config_to_read_from[full_keyword] = ['echam.datasets', 'echam.restart.streams']
    >>> model, model_part = 'echam', 'datasets' # first part
    >>> model, model_part = 'echam', 'restart.streams' # second part

    The first part, in the example ``echam`` is used to determine where to look
    for new YAML files. Then, a yaml file corresponding to a file called
    ``echam.datasets.yaml`` is loaded, and attached to the config.

    Warning
    -------
    Both ``config_to_read_from`` and ``config_to_write_to`` are modified **in place**!
    """
    if full_keyword in config_to_read_from:

        if level_to_write_to:
            if reduced_keyword in config_to_read_from[level_to_write_to]:
                config_to_read_from[level_to_write_to][
                    reduced_keyword
                ] += config_to_read_from[full_keyword]
            else:
                config_to_read_from[level_to_write_to][
                    reduced_keyword
                ] = config_to_read_from[full_keyword]
        else:
            if reduced_keyword in config_to_read_from:
                config_to_read_from[reduced_keyword] += config_to_read_from[
                    full_keyword
                ]
            else:
                config_to_read_from[reduced_keyword] = config_to_read_from[full_keyword]
        # FIXME: Does this only need to work for lists?
        if isinstance(config_to_read_from[full_keyword], list):
            for item in config_to_read_from[full_keyword]:
                model = item
                if "-" in item:
                    model, rest = (item.split("-")[0], "-".join(item.split("-")[1:]))
                else:
                    if "." in item:
                        model, model_part = (
                            item.split(".")[0],
                            ".".join(item.split(".")[1:]),
                        )
                        logger.debug("Attaching: %s for %s", model_part, model)
                    else:
                        if item in config_to_read_from:
                            if "version" in config_to_read_from[item]:
                                item = (
                                    model + "-" + config_to_read_from[item]["version"]
                                )

                include_path, needs_load = look_for_file(model, item)
                if not include_path:
                    include_path, needs_load = look_for_file(model, model)
                if not include_path:
                    print(
                        f"attach_to_config_and_reduce: File {item} of model {model} could not be found. Sorry."
                    )
                    sys.exit(-1)
                logger.debug("Reading %s", include_path)
                if needs_load:
                    tmp_config = yaml_file_to_dict(include_path)
                else:
                    tmp_config = include_path

                config_to_write_to[tmp_config["model"]] = tmp_config

                for attachment in CONFIGS_TO_ALWAYS_ATTACH_AND_REMOVE:
                    logger.debug("Attaching: %s", attachment)
                    attach_to_config_and_remove(
                        config_to_write_to[tmp_config["model"]],
                        attachment,
                        all_config=None,
                    )

        else:
            raise TypeError("The entries in %s must be a list!!" % full_keyword)
        del config_to_read_from[full_keyword]


def attach_single_config(config, path, attach_value, all_config=None, **kwargs):
    """
    Parameters
    ----------
    config : dict
        subset of the main configuration, eg. config[model]

    all_config : dict
        main configuration
    """
    include_path, needs_load = look_for_file(path, attach_value, all_config=all_config)
    if include_path:
        if needs_load:
            attachable_config = yaml_file_to_dict(include_path)
        else:
            attachable_config = include_path
    elif os.path.isfile(path + "/" + attach_value):
        attachable_config = yaml_file_to_dict(path + "/" + attach_value)
    else:
        print("Could not find ", path + "/" + attach_value)
        sys.exit(1)
    # DB this is a try:
    dict_merge(config, attachable_config, **kwargs)
    # config.update(attachable_config)


def attach_to_config_and_remove(config, attach_key, all_config=None, **kwargs):
    """
    Attaches extra dict to this one and removes the chapter

    Updates the dictionary on ``config`` with values from any file found under
    a listing specified by ``attach_key``.

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
        del config[attach_key]
        if type(attach_value) == str:
            attach_value = [attach_value]
        for attach_value in attach_value:
            try:
                attach_path, attach_value = attach_value.rsplit("/", 1)
            except ValueError:
                attach_path = "."
            attach_single_config(
                config, attach_path, attach_value, all_config=all_config, **kwargs
            )


priority_marker = ">>THIS_ONE<<"


def priority_merge_dicts(first_config, second_config, priority="first"):
    """Given two dictionaries, merge them together preserving either first or last entries.

    Parameters
    ----------
    first_config : dict
    second_config : dict
    priority : str
        One of "first" or "second". Specifies which dictionary should be given
        priority when merging.

    Returns
    -------
    merged : dict
        A dictionary containing all keys, with duplicate entries reverting to
        the dictionary given in "priority". The merge occurs across all levels.
    """
    if priority == "second":
        merged_dictionary = first_config
        to_merge = second_config
    elif priority == "first":
        merged_dictionary = second_config
        to_merge = first_config
    else:
        raise TypeError("Please use 'first' or 'second' for the priority!")
    # for key in to_merge:
    #    for inner_key in list(to_merge[key]):
    #        if not priority_marker in inner_key:
    #            to_merge[key][inner_key+priority_marker] = to_merge[key][inner_key]
    #            del to_merge[key][inner_key]
    dict_merge(merged_dictionary, to_merge)
    return merged_dictionary


# NEW STUFF ALREADY, SHALL REPLACE OLD STUFF:


def resolve_remove_and_add(workdict):
    for chapter in workdict:
        if "remove_" in chapter:
            remove_chapter = chapter.replace("remove_", "")
            remove_entries_from_chapter(workdict, remove_chapter, entries)
            # del config[chapter]

        elif "add_" in chapter:
            add_chapter = chapter.replace("add_", "")
            add_entries_from_chapter(workdict, add_chapter, entries)
            # del config[chapter]
    return workdict


def new_dict_merge(dct, merge_dct, winner="to_be_included"):
    """Recursive dict merge. Inspired by :meth:``dict.update()``, instead of
    updating only top-level keys, dict_merge recurses down into dicts nested
    to an arbitrary depth, updating keys. The ``merge_dct`` is merged into
    ``dct``.
    :param dct: dict onto which the merge is executed
    :param merge_dct: dct merged into dct
    :param winner: should be either receiving (default) or to_be_included
    :return: None
    """
    for k, v in six.iteritems(merge_dct):
        if (
            k in dct
            and isinstance(dct[k], dict)
            and isinstance(merge_dct[k], collections.Mapping)
        ):
            new_dict_merge(dct[k], merge_dct[k], winner)
        else:
            if not (winner == "receiving" and k in dct):
                dct[k] = merge_dct[k]


def new_deep_update(
    receiving_dict, dict_to_be_included, winner="receiving", blackdict={}
):
    dict_to_be_included = resolve_remove_and_add(dict_to_be_included)
    for chapter in dict_to_be_included:
        if chapter not in blackdict:
            new_dict_merge(
                receiving_dict, {chapter: dict_to_be_included[chapter]}, winner=winner
            )
    return receiving_dict


# END NEW STUFF


def dict_merge(dct, merge_dct, resolve_nested_adds=False, **kwargs):
    """Recursive dict merge. Inspired by :meth:``dict.update()``, instead of
    updating only top-level keys, dict_merge recurses down into dicts nested
    to an arbitrary depth, updating keys. The ``merge_dct`` is merged into
    ``dct``.
    :param dct: dict onto which the merge is executed
    :param merge_dct: dct merged into dct
    :return: None
    """
    # option to overwrite a dict value if merge_dict contains empty value. Default
    # is False
    dont_overwrite_with_empty_value = kwargs.get(
        "dont_overwrite_with_empty_value", False
    )

    for k, v in six.iteritems(merge_dct):
        if (
            k in dct
            and isinstance(dct[k], dict)
            and isinstance(merge_dct[k], collections.Mapping)
        ):
            # NOTE(PG): this is a very bad hack and doesn't belong here at all.
            # Maybe instead the yaml_file_to_dict needs to say something like
            # "add_debug_info", so everything gets put together, but for right
            # now you are given ifnromation where your config originally came
            # from...
            #
            # IDEA: It would be great if somehow we knew which key came from
            # which config file: some are in the user, some are in the setup,
            # some are in the component. However, I have no idea how to do that
            # correctly...Turn the keys in the config into named tuples with
            # the original file? Make a custom "class" for config keys?? That
            # would then be:
            #
            # config['echam'].keys()
            # * (key_name: namelist_changes, came_from: user, overrides: [setup, component])
            # Above, the overrides list always gets longer depending on where the value actually came from.
            # * (another key-tuple)
            # * and so on...
            #
            # An idea...but I have absolutely no clue how to cleanly implement that...
            if k != "debug_info":
                dict_merge(dct[k], merge_dct[k], resolve_nested_adds)
            else:
                if "debug_info" in dct:
                    if isinstance(dct["debug_info"]["loaded_from_file"], str):
                        dct["debug_info"]["loaded_from_file"] = [
                            dct["debug_info"]["loaded_from_file"]
                        ]
                    else:
                        dct["debug_info"]["loaded_from_file"].append(
                            merge_dct["debug_info"]["loaded_from_file"]
                        )
        # MA: I'm not super happy about the resolve_nested_adds implementation. Nested
        # adds should probably resolved in a different place, after the first level
        # ones are resolved.
        # If the key exists and starts by ``add_``, it is a nested ``add_`` and is
        # solved as such
        elif (
            resolve_nested_adds
            and isinstance(k, str)
            and k.startswith("add_")
            and k not in protected_adds
            and isinstance(v, (list, dict))
        ):
            add_entries_from_chapter(dct, "".join(k.split("add_")), v)
        else:
            # keep the value of dct[k] if dct[k] is already set but merge_dct[k]
            # is empty and protection is requested
            if k in dct:
                if dct[k] and not merge_dct[k] and dont_overwrite_with_empty_value:
                    merge_dct[k] = dct[k]

            dct[k] = merge_dct[k]


def deep_update(chapter, entries, config, blackdict={}):
    if "add_" in chapter:
        add_chapter = chapter.replace("add_", "")
        add_entries_from_chapter(config, add_chapter, entries)
        # del config[chapter]
    else:
        if chapter not in blackdict:
            dict_merge(config, {chapter: entries})

        # deniz: bugfix: choose_ blocks with "*" keys don't get updated. Eg.
        # mail1 and mail2 don't get updated since they are initialized as empty
        # strings and are already inside. Current bug fix only correct scalar
        # types such as strings.
        else:
            empty_values = ["", None]  # some possible empty values to override

            # strip all whitespace. Eg. "  " -> "", if it is only space
            if isinstance(blackdict[chapter], str) and blackdict[chapter].isspace():
                blackdict[chapter] = re.sub(r"\s+", "", blackdict[chapter])

            # The update_key (chapter) is already inside the blackdict however,
            # its value (entries) it not empty. So, update them
            if blackdict[chapter] in empty_values and entries not in empty_values:
                dict_merge(config, {chapter: entries})

            # Trying to update a dictionary from the same file (in blackdict)
            # an ``add_`` returns an error
            if isinstance(blackdict[chapter], dict):
                user_error(
                    "Missing 'add_'",
                    (
                        f"Not possible to update the '{config['model']}.{chapter}' "
                        + f"dictionary. Please, use 'add_{chapter}' inside the "
                        + f"'choose_' block, instead of just '{chapter}'."
                    ),
                )


def dict_overwrite(sender, receiver, key_path=[], recursion_level=0, verbose=False):
    """Recursively search through the dictionary and replace the keys with the
    ``overwrite`` tag

    Parameters
    ----------
    sender : dict
        contains the information with the key 'overwrite' to replace the
        information in the receiver dictionary.
    receiver : dict
        contains the information that will be replaced by the sender dictionary.
    key_path : list
        used only in the recursive call. Contains the nested dictionary structure.
    recursion_level : int
        used only in the recursive call. Stores the value of the recursion depth.
    verbose : bool
        if True then the function will print the before/after information

    Returns
    -------
    receiver : dict
        The input argument is overwritten
    """
    # iterate over the sender dictionary and enter recursively search for the
    # boolean key "overwrite". It is assumed that 'key' is also present in the
    # receiver
    for key in sender:
        # clear the key_path upon first entry or new entry after recursion
        if recursion_level == 0:
            key_path.clear()
        else:
            # len(key_path) is recursion_level. Only take up to the current
            # recursion level to prevent unnecessary appends
            key_path = key_path[:recursion_level]

        key_path.append(key)

        if isinstance(receiver.get(key, None), dict):
            # check if there is "overwrite" key whose value is True
            if sender[key].get("overwrite", False):
                del sender[key]["overwrite"]  # clean up
                value_before = copy.deepcopy(receiver[key])

                # update (overwrite) the value in the receiving dictionary
                receiver[key] = copy.deepcopy(sender[key])

                # print the changed section
                if verbose:
                    space = "    "
                    path_str = " -> ".join(key_path)
                    print(f"::: Overwriting the path:    [{path_str}]")
                    print("::: value before:")
                    before_str = f"{space}{yaml.dump(value_before, indent=4)}"
                    before_str = before_str.replace("\n", f"\n{space}")
                    before_str = re.sub(r"\n[ \t]*$", "", before_str)
                    print(before_str)
                    print("---")

                    print("::: value after:")
                    after_str = f"{space}{yaml.dump(receiver[key], indent=4)}"
                    after_str = after_str.replace("\n", f"\n{space}")
                    after_str = re.sub(r"\n[ \t]*$", "", after_str)
                    print(after_str)
                    print()
            else:
                # enter into recursion using the current dictionary values. Keep
                # track of the recursion path and depth
                dict_overwrite(
                    sender[key],
                    receiver[key],
                    key_path=key_path,
                    recursion_level=recursion_level + 1,
                    verbose=verbose,
                )

    return receiver


def find_remove_entries_in_config(mapping, model_name, models=[]):
    all_removes = []
    mappings = [mapping]
    while mappings:
        mapping = mappings.pop()
        try:
            items = six.iteritems(mapping)
        except AttributeError:
            continue
        for key, value in items:
            if isinstance(key, str) and key.startswith("remove_"):
                # If the model is not present in the path add it
                if key.split("remove_")[-1].split(".")[0] not in models:
                    key = "remove_" + model_name + "." + key.split("remove_")[-1]
                all_removes.append((key, value))
            if isinstance(value, dict):
                mappings.append(value)
    # all_removes = [(remove_echam.forcing_files, [sst, sic,])]
    # NOTE: Not Allowed: all_removes = [(remove_echam.forcing_files, sst)]
    return all_removes


def remove_entries_from_chapter(config, remove_chapter, remove_entries):
    for entry in remove_entries:
        try:
            del config[remove_chapter][entry]
        except KeyError:
            pass


def add_entries_from_chapter(config, add_chapter, add_entries):
    my_entries = copy.deepcopy(add_entries)
    if add_chapter in config:
        if type(config[add_chapter]) == list:
            for entry in my_entries:
                config[add_chapter].append(entry)
        elif type(config[add_chapter]) == dict:
            # MA: I'm not supper happy about the resolve_nested_adds implementation
            dict_merge(
                config[add_chapter],
                add_entries,
                resolve_nested_adds=True,
            )
    else:
        config[add_chapter] = add_entries


def remove_entry_from_chapter(
    remove_chapter,
    remove_entries,
    model_to_remove_from,
    model_with_remove_statement,
    model_config,
    setup_config,
):
    """
    Deletes the entries specified by the user using the ``remove_<chapter>`` command
    contained in the chapter, that can be either a list or a dictionary. After the
    removals the ``remove_<chapter>`` command is cleaned up from the config.

    Parameters
    ----------
    remove_chapter : str
        A string specifying the path inside the config to reach the chapter where
        the entries to be removed are. The string is composed by ``remove_`` followed
        by the path where each nested chapter is separated by a ``.``.
    remove_entries : list
        The list of entries to be remove from the chapter.
    model_to_remove_from : str
        Indicates the main chapter inside config where removes need to take place (i.e.
        ``computer``, ``general``, ``<model>``, ...).
    model_with_remove_statement : str
        Indicates the main chapter where the remove command is defined.
    model_config : dict
        Component-specific general configuration.
    setup_config : dict
        Setup-specific general configuration.
    """

    logging.debug("%s, %s", remove_entries, remove_chapter)
    # Check that the the user entry is a list, if not rise an exception
    if not isinstance(remove_entries, list):
        raise TypeError("Please put all entries to remove as a list")
    # Check for ``namelist_changes`` and change the extension dot ``.`` for a ``,``
    remove_chapter_nml = remove_chapter
    if "namelist_changes" in remove_chapter:
        ind_nc = remove_chapter.find("namelist_changes") + len("namelist_changes") + 1
        ind_ext_dot = remove_chapter.find(".", ind_nc)
        if ind_ext_dot > 0:
            remove_chapter_split = list(remove_chapter)
            remove_chapter_split[ind_ext_dot] = ","
            remove_chapter_nml = "".join(remove_chapter_split)
    # Find the required config
    if model_to_remove_from in model_config:
        config = model_config[model_to_remove_from]
    elif model_to_remove_from in setup_config:
        config = setup_config[model_to_remove_from]

    # Delete the variables specified in the remove_<chapter> in config
    for entry in remove_entries:
        try:
            # Find the nested subchapter and substitute "," with "." for namelist extensions
            path2chapter = [
                subchapter.replace(",", ".")
                for subchapter in remove_chapter_nml.split(".")[1:]
            ]
            # Reach the subchapter that to be removed
            remove_from_config = recursive_get(config, path2chapter)
            # If remove_from_config is a list use remove method, if it is a dictionary use del
            if isinstance(remove_from_config, list):
                remove_from_config.remove(entry)
            else:
                del remove_from_config[entry]
        except:
            pass
    # Cleanup the remove_<chapter> command defined by the user either in model_config or in setup_config
    if model_with_remove_statement in model_config:
        try:
            del model_config[model_with_remove_statement][
                remove_chapter.replace(model_with_remove_statement + ".", "")
            ]
        except:
            pass
    elif model_with_remove_statement in setup_config:
        try:
            del setup_config[model_with_remove_statement][
                remove_chapter.replace(model_with_remove_statement + ".", "")
            ]
        except:
            pass


def remove_entries_from_chapter_in_config(
    model_config, valid_model_names, setup_config, valid_setup_names
):
    config = model_config
    for model in valid_model_names:
        logging.debug(model)
        all_removes_for_model = find_remove_entries_in_config(
            config[model], model, config.keys()
        )
        for remove_chapter, remove_entries in all_removes_for_model:
            model_to_remove_from = remove_chapter.split(".")[0].replace("remove_", "")
            remove_entry_from_chapter(
                remove_chapter,
                remove_entries,
                model_to_remove_from,
                model,
                model_config,
                setup_config,
            )
            try:
                del config[model][remove_chapter]
            except:
                pass


def basic_find_remove_entries_in_config(mapping):
    all_removes = []
    mappings = [mapping]
    while mappings:
        mapping = mappings.pop()
        try:
            items = six.iteritems(mapping)
        except AttributeError:
            continue
        for key, value in items:
            if isinstance(key, str) and key.startswith("remove_"):
                all_removes.append((key, value))
            if isinstance(value, dict):
                mappings.append(value)
    # all_removes = [(remove_echam.forcing_files, [sst, sic,])]
    # NOTE: Not Allowed: all_removes = [(remove_echam.forcing_files, sst)]
    return all_removes


def basic_find_add_entries_in_config(mapping):
    all_adds = []
    mappings = [mapping]
    while mappings:
        mapping = mappings.pop()
        # try:
        #    items = six.iteritems(mapping)
        # except AttributeError:
        #    continue
        for key in list(mapping):
            value = mapping[key]
            # for key, value in items:
            if isinstance(key, str) and key.startswith("add_"):
                all_adds.append((key, value))
            if isinstance(value, dict):
                mappings.append(value)
    # all_adds = [(add_echam.forcing_files, [sst, sic,])]
    # NOTE: Not Allowed: all_adds = [(add_echam.forcing_files, sst)]
    return all_adds


def find_add_entries_in_config(mapping, model_name):
    all_adds = []
    mappings = [mapping]
    while mappings:
        mapping = mappings.pop()
        try:
            items = six.iteritems(mapping)
        except AttributeError:
            continue
        for key, value in items:
            if isinstance(key, str) and key.startswith("add_"):
                if not "." in key:
                    key = "add_" + model_name + "." + key.split("add_")[-1]
                all_adds.append((key, value))
    #       if isinstance(value, dict):
    #           mappings.append(value)
    # all_adds = [(add_echam.forcing_files, [sst, sic,])]
    # NOTE: Not Allowed: all_adds = [(add_echam.forcing_files, sst)]
    return all_adds


list_counter = 0


def add_entry_to_chapter(
    add_chapter,
    add_entries,
    model_to_add_to,
    model_with_add_statement,
    model_config,
    setup_config,
):

    if model_to_add_to in model_config:
        target_config = model_config
    else:
        target_config = setup_config
    if model_with_add_statement in model_config:
        source_config = model_config
    else:
        cource_config = setup_config

    logging.debug(model_to_add_to)
    logging.debug(add_chapter)
    if add_chapter in source_config[model_with_add_statement]:
        source_chapter = add_chapter
    else:
        source_chapter = add_chapter.replace(model_to_add_to + ".", "")

    # If the desired chapter doesn't exist yet, just put it there
    logging.debug(model_to_add_to)
    logging.debug(add_chapter)
    if (
        not add_chapter.split(".")[-1].replace("add_", "")
        in target_config[model_to_add_to]
    ):
        target_config[model_to_add_to][
            add_chapter.split(".")[-1].replace("add_", "")
        ] = add_entries
    else:
        if not type(
            target_config[model_to_add_to][
                add_chapter.split(".")[-1].replace("add_", "")
            ]
        ) == type(add_entries):
            raise TypeError("Something is wrong")
        else:
            if isinstance(
                target_config[model_to_add_to][
                    add_chapter.split(".")[-1].replace("add_", "")
                ],
                list,
            ):
                # Define the list to be modified
                mod_list = target_config[model_to_add_to][
                    add_chapter.split(".")[-1].replace("add_", "")
                ]
                # Add the entries
                mod_list += add_entries
                # Remove duplicates
                mod_list_no_dupl = []
                for el in mod_list:
                    if not isinstance(el, (dict, tuple, list)):
                        if not el in mod_list_no_dupl:
                            mod_list_no_dupl.append(el)
                    else:
                        mod_list_no_dupl.append(el)
                target_config[model_to_add_to][
                    add_chapter.split(".")[-1].replace("add_", "")
                ] = mod_list_no_dupl
                global list_counter
                list_counter += 1
            elif isinstance(
                target_config[model_to_add_to][
                    add_chapter.split(".")[-1].replace("add_", "")
                ],
                dict,
            ):
                # If the chapter is a dictionary use dict_merge where the new entries
                # have priority over the preexisting ones (user choices win over
                # anything else)
                dict_merge(
                    target_config[model_to_add_to][
                        add_chapter.split(".")[-1].replace("add_", "")
                    ],
                    add_entries,
                )
    if list_counter > 1:
        pass
        # pdb.set_trace()
    logging.debug(model_with_add_statement)
    logging.debug(source_chapter)
    # del source_config[model_with_add_statement][source_chapter.replace("add_", "")]


def basic_add_entries_to_chapter_in_config(config):
    all_adds_for_model = basic_find_add_entries_in_config(config)
    for add_chapter, add_entries in all_adds_for_model:
        add_entries_from_chapter(config, add_chapter.replace("add_", ""), add_entries)


def basic_remove_entries_from_chapter_in_config(config):
    all_removes_for_model = basic_find_remove_entries_in_config(config)
    for remove_chapter, remove_entries in all_removes_for_model:
        remove_entries_from_chapter(
            config, remove_chapter.replace("remove_", ""), remove_entries
        )


def add_entries_to_chapter_in_config(
    model_config, valid_model_names, setup_config, valid_setup_names
):
    config = model_config
    for model in list(config):
        logging.debug(model)
        all_adds_for_model = find_add_entries_in_config(config[model], model)
        for add_chapter, add_entries in all_adds_for_model:
            model_to_add_to = add_chapter.split(".")[0].replace("add_", "")
            add_entry_to_chapter(
                add_chapter,
                add_entries,
                model_to_add_to,
                model,
                model_config,
                setup_config,
            )


def merge_dicts(*dict_args):
    """
    Given any number of dicts, shallow copy and merge into a new dict,
    precedence goes to key value pairs in latter dicts.

    Note that this function only merges the first level. For deeper merging,
    use ``priority_merge_dicts``.

    Parameters
    ----------
    *dict_args
        Any number of dictionaries to merge together

    Returns
    -------
    A merged dictionary (shallow).
    """
    result = {}
    for dictionary in dict_args:
        result.update(dictionary)
    return result


def del_value_for_nested_key(config, key):
    """
    In a dict of dicts, delete a key/value pair.

    Parameters
    ----------
    config : dict
        The dict to delete in.
    key : str
        The key to delete.

    Warning
    -------
    The ``config`` is modified **in place**!
    """
    if key in config:
        del config[key]
    for v in config.values():
        if isinstance(v, dict):
            del_value_for_nested_key(v, key)


def find_value_for_nested_key(mapping, key_of_interest, tree=[]):
    """
    In a dict of dicts, find a value for a given key

    Parameters
    ----------
    mapping : dict
        The nested dictionary to search through
    key_of_interest : str
        The key to search for.
    tree : list
        Where to start searching

    Returns
    -------
    value :
        The value of key anywhere in the nested dict.

    Note
    ----
    Behaviour of what happens when a key appears twice anywhere on different
    levels of the nested dict is unclear. The uppermost one is taken, but if
    the key appears in more than one item, I'd guess something ambigous
    occus...
    """
    original_mapping = mapping
    logger.debug("Looking for key %s", key_of_interest)
    logging.debug("Looking in %s", mapping)
    logger.debug("Using tree %s", tree)
    if tree:
        for leaf in tree:
            mapping = mapping[leaf]
        else:
            tree = [None]
    for leaf in reversed(tree):
        logging.debug("Looking in bottommost leaf %s", leaf)
        for key, value in six.iteritems(mapping):
            if key == key_of_interest:
                return value
        if leaf:
            find_value_in_nested_key(original_mapping, key_of_interest, tree[:-1])
    warnings.warn("Couldn't find value for key %s" % key_of_interest)
    # raise KeyError("Couldn't find value for key %s", key_of_interest)


def basic_choose_blocks(config_to_resolve, config_to_search, isblacklist=True):
    all_set_variables = {}
    while True:
        name_chooses = basic_list_all_keys_starting_with_choose(
            config_to_resolve, gray_list, isblacklist
        )
        if name_chooses == []:
            break
        for key, block in name_chooses:
            all_set_variables[key] = basic_determine_set_variables_in_choose_block(
                block
            )

        task_list = choose_key = basic_find_one_independent_choose(all_set_variables)
        logging.debug("The task list is: %s", task_list)
        logging.debug("all_set_variables: %s", all_set_variables)
        resolve_basic_choose(config_to_search, config_to_resolve, choose_key)
        del all_set_variables[choose_key]
        for key in list(all_set_variables):
            if not all_set_variables[key]:
                del all_set_variables[key]
        logging.debug("Remaining all_set_variables=%s", all_set_variables)

    basic_add_entries_to_chapter_in_config(config_to_resolve)
    basic_remove_entries_from_chapter_in_config(config_to_resolve)


def basic_list_all_keys_starting_with_choose(mapping, ignore_list, isblacklist):
    logging.debug("Top of list_all_keys_starting_with_choose")
    all_chooses = []
    for key, value in six.iteritems(mapping):
        if (
            isinstance(key, str)
            and key.startswith("choose_")
            and (
                (not isblacklist)
                or (isblacklist and not determine_regex_list_match(key, ignore_list))
            )
            and not determine_regex_list_match(key, constant_blacklist)
        ):
            all_chooses.append((key, value))
    logging.debug("Will return %s", all_chooses)
    return all_chooses


def list_all_keys_starting_with_choose(mapping, model_name, ignore_list, isblacklist):
    """
    Given a ``mapping`` (e.g. a ``dict``-type object), list all keys that start
    with ``"choose_"`` on any level of the nested dictionary.

    Parameters
    ----------
    mapping : dict
        The dictionary to search through for keys starting with ``"choose_"``

    model_name : str
    ignore_list : list
    Returns
    -------
    all_chooses : list
        A list of tuples for ....
        A dictionary containing all key, value pairs starting with
        ``"choose_"``.
    """
    logging.debug("Top of list_all_keys_starting_with_choose")
    all_chooses = []
    if not isinstance(mapping, dict):
        print(">>>>>>>>>>>>>>>>>>>> PG")
        print(locals())
        import pdb

        pdb.set_trace()
    keys = list(mapping)
    for key in keys:
        value = mapping[key]
        if (
            isinstance(key, str)
            and key.startswith("choose_")
            and (
                (not isblacklist)
                or (isblacklist and not determine_regex_list_match(key, ignore_list))
            )
            and not determine_regex_list_match(key, constant_blacklist)
        ):
            if not "." in key:
                old_key = key
                key = "choose_" + model_name + "." + key.split("choose_")[-1]
                del mapping[old_key]
                mapping[key] = value
            all_chooses.append((key, value))
    logging.debug("Will return %s", all_chooses)
    return all_chooses


def basic_determine_set_variables_in_choose_block(config):
    set_variables = []
    for k, v in six.iteritems(config):
        if isinstance(v, dict):  # and isinstance(k, str) and k.startswith("choose_"):
            # Go in further
            set_variables += basic_determine_set_variables_in_choose_block(v)
        else:
            var_name = k
            set_variables.append(var_name)
    return set_variables


def determine_set_variables_in_choose_block(config, valid_model_names, model_name=[]):
    """
    Given a config, figures out which variables are resolved in a choose block.

    In order to avoid cyclic dependencies, it is necessary to figure out which
    variables are set in which choose block. This function recurses over all
    key/value pairs of a configuration, and for any key which is a model name,
    it determines which variables are set in it's ``choose_`` blocks. Tuples of
    ``(model_name, var_name)`` are appended to a list, which is returned with
    all it's duplicates removed.

    Parameters
    ----------
    config : dict
    valid_model_names : list
    model_name : list

    Returns
    -------
    set_variables : list
        A list of tuples of model_name and corresponding variable that are
        determined in ``config``
    """
    set_variables = []
    for k, v in six.iteritems(config):
        if isinstance(k, str) and k in valid_model_names:
            logging.debug(k)
            model_name = k
        if isinstance(v, dict):  # and isinstance(k, str) and k.startswith("choose_"):
            # Go in further
            set_variables += determine_set_variables_in_choose_block(
                v, valid_model_names, model_name
            )
        else:
            var_name = k
            if not model_name:
                model_name = "general"
            set_variables.append((model_name, var_name))
    return set_variables


def basic_find_one_independent_choose(all_set_variables):
    """
    Given a dictionary of ``all_set_variables``, which comes out of the
    function ``determine_set_variables_in_choose_block``, gives a list of
    task/variable dependencies to resolve in order to figure out the variable.

    Parameters
    ----------
    all_set_variables : dict

    Returns
    -------
    task_list : list
        A list of tuples comprising ``(model_name, var_name)`` in order to
        resolve one ``choose_`` block. This list is built in such a way that
        the beginning of the list provides dependencies for later on in the
        list.
    """
    task_list = []
    for choose_keyword in list(all_set_variables):
        # for choose_keyword, set_vars in six.iteritems(value):
        task_list.append(choose_keyword)
        task_list = basic_add_more_important_tasks(
            choose_keyword, all_set_variables, task_list
        )
        logging.debug(task_list)
        return task_list[0]


def find_one_independent_choose(all_set_variables):
    """
    Given a dictionary of ``all_set_variables``, which comes out of the
    function ``determine_set_variables_in_choose_block``, gives a list of
    task/variable dependencies to resolve in order to figure out the variable.

    Parameters
    ----------
    all_set_variables : dict

    Returns
    -------
    task_list : list
        A list of tuples comprising ``(model_name, var_name)`` in order to
        resolve one ``choose_`` block. This list is built in such a way that
        the beginning of the list provides dependencies for later on in the
        list.
    """
    task_list = []
    for key in all_set_variables:
        value = all_set_variables[key]
        choose_keywords = list(value)
        for choose_keyword in choose_keywords:
            set_vars = value[choose_keyword]
            task_list.append((key, choose_keyword))
            task_list = add_more_important_tasks(
                choose_keyword, all_set_variables, task_list
            )
            logging.debug(task_list)
            return task_list[0]


def resolve_basic_choose(config, config_to_replace_in, choose_key, blackdict={}):
    path_to_key = choose_key.replace("choose_", "").split(".")
    try:
        choice = recursive_get(config, path_to_key)
    except ValueError:
        if "*" not in config_to_replace_in[choose_key]:
            raise KeyError("Key %s was not defined" % ".".join(path_to_key))
        else:
            del config_to_replace_in[choose_key]
            return
    if isinstance(choice, str) and "${" in choice:
        try:
            choice = find_variable(
                [config_to_replace_in["model"]], choice, config, [], True
            )
            # print("BEEEEE CALM, resolved: " + choice)
        except:
            # print("BEEEEE CAREFUL, did not resolve: " + choose_key)
            # logging.warning("Variable %s as a choice, skipping...", choice)
            # del config_to_replace_in[choose_key]
            gray_list.append(re.compile(choose_key))
            return
    # Evaluates the mathematical expressions in the choose_ blocks
    if isinstance(choice, str) and "$((" in choice:
        choice = do_math_in_entry([False], choice, config)
    logging.debug(choice)

    if choice in config_to_replace_in.get(choose_key):
        for update_key, update_value in six.iteritems(
            config_to_replace_in[choose_key][choice]
        ):
            deep_update(update_key, update_value, config_to_replace_in, blackdict)

    elif "*" in config_to_replace_in.get(choose_key):
        logging.debug("Found a * case!")
        for update_key, update_value in six.iteritems(
            config_to_replace_in[choose_key]["*"]
        ):
            deep_update(update_key, update_value, config_to_replace_in, blackdict)
    else:
        # Those two are too noisy
        # logging.warning("Choice %s could not be resolved", choice)
        # logging.warning("Key was key=%s", choose_key)
        pass

    del config_to_replace_in[choose_key]


def resolve_choose(model_with_choose, choose_key, config):
    if model_with_choose in config:
        config_to_replace_in = config
    else:
        raise KeyError("Something is horribly wrong")
    model_name, key = choose_key.replace("choose_", "").split(".")
    choice = config.get(model_with_choose).get(key)

    config_to_search_in = {}

    if model_name in config:
        config_to_search_in = config

    if not config_to_search_in:
        raise KeyError("Something else is horribly wrong")

    if key in config_to_search_in[model_name]:
        choice = config_to_search_in[model_name][key]
        logging.debug(model_with_choose)
        logging.debug(choice)

        logging.debug("key=%s", key)


def resolve_choose_with_var(
    var, config, current_model=None, user_config={}, model_config={}, setup_config={}
):
    """
    Searches for a ``choose_`` block inside a model configuration ``config``, in which
    ``var`` is defined, and then resolves ONLY the ``var`` and ``add_<var>`` (the other
    variables in the ``choose_`` remain untouched). Needed, for example, for being able
    to use ``include_models`` from a ``choose_`` before the general choose-resolve takes
    place (i.e. include ``xios`` component from ``oifs.yaml`` using a ``choose_``).

    Parameters
    ----------
    var : str
        Name of the variable to be searched inside ``choose_`` blocks.
    config : dict
        Model configuration to be changed if the ``var`` is resolved by the ``choose_``.
    user_config : dict
        User configuration, used to search for the selected case of the ``choose_``.
    model_config : dict
        Component configuration, used to search for the selected case of the
        ``choose_``.
    setup_config : dict
        Setup configuration, used to search for the selected case of the ``choose_``.
    """

    sep = ","
    # Find the path to the variable ``var`` in the given ``config``, inside a
    # ``choose_``
    choose_with_var = find_key(config, var, exc_strings="add_", paths2finds=[], sep=sep)
    choose_with_var = [
        x for x in choose_with_var if "choose_" in x and f"choose_{var}" not in x
    ]
    # Find the path to the variable ``add_var`` in the given ``config``, inside a
    # ``choose_``
    choose_with_add_var = find_key(config, f"add_{var}", paths2finds=[], sep=sep)
    choose_with_add_var = [x for x in choose_with_add_var if "choose_" in x]

    # Resolve first for ``<var>`` and then for ``add_<var>``
    for choose_with_var, lvar in [
        (choose_with_var, var),
        (choose_with_add_var, f"add_{var}"),
    ]:
        # If the path is found
        if choose_with_var:
            # If ``lvar`` is in multiple ``choose_`` blocks return an error
            chooses = [sep.join(choose_with_var[0].split(sep)[:-2])]
            for case in choose_with_var:
                choose = sep.join(case.split(sep)[:-2])
                if choose not in chooses:
                    user_error(
                        f'"{lvar}" in more than one choose_ block', choose_with_var
                    )
                chooses.append(choose)
            # Get the first part of the path to the ``lvar``
            choose_with_var = choose_with_var[0].split(sep)[0]
            # Get the key for the ``choose_``
            choose_key = choose_with_var.replace("choose_", "")
            # Deep copy here avoids the other variables in the case to be updated
            # now. We want to update now ONLY the ``lvar``.
            config_copy = copy.deepcopy(config)
            # If the key includes the absolute path
            if "." in choose_key:
                # Get the component to search for the key
                component_with_key = choose_key.split(".")[0]
                # Remove the chapter from the key
                choose_key = choose_key.split(".")[1]
            else:
                # Name of the evaluated model
                if not current_model:
                    current_model = config.get("model")
                # Get the component to search for the key
                component_with_key = current_model
                # Rename the whole choose_ block in the copied configuration to include
                # the chapter
                choose_with_var_new = choose_with_var.replace(
                    "choose_", f"choose_{component_with_key}."
                )
                config_copy[choose_with_var_new] = config_copy[choose_with_var]
                del config_copy[choose_with_var]
                choose_with_var = choose_with_var_new
            # Find where the case for the ``choose_`` is defined, with priority: user ->
            # setup -> model
            config_to_search_into = None
            if choose_key in user_config.get(component_with_key, []):
                config_to_search_into = user_config
            elif choose_key in setup_config.get(component_with_key, []):
                config_to_search_into = setup_config
            elif choose_key in model_config.get(component_with_key, []):
                config_to_search_into = model_config
            # If the case was found
            if config_to_search_into:
                # Resolve the case
                resolve_basic_choose(
                    config_to_search_into, config_copy, choose_with_var
                )
                # If ``var`` was defined through the resolution of the ``choose_``, add
                # the ``var`` value to the ``config``.
                if config_copy.get(var):
                    config[var] = config_copy.get(var)


def basic_add_more_important_tasks(choose_keyword, all_set_variables, task_list):
    """
    Determines dependencies of a choose keyword.

    Parameters
    ----------
    choose_keyword : str
        The keyword, starting with choose, which is looked through to check if
        there are any dependencies that must be resolved first to correctly
        resolve this one.
    all_set_variables : dict
        All variables that can be set
    task_list : list
        A list in the order in which tasks must be resolved for
        ``choose_keyword`` to make sense.

    Returns
    -------
    task_list
        A list of choices which must be made in order for choose_keyword to
        make sense.
    """
    keyword = choose_keyword.replace("choose_", "")
    for choose_thing in all_set_variables:
        logging.debug("Choose_thing = %s", choose_thing)
        for keyword_that_is_set in all_set_variables[choose_thing]:
            if keyword_that_is_set == keyword:
                if choose_thing not in task_list:
                    task_list.insert(0, choose_thing)
                    basic_add_more_important_tasks(
                        choose_thing, all_set_variables, task_list
                    )
                    return task_list
                else:
                    raise KeyError("Opps cyclic dependency: %s" % task_list)
    return task_list


def add_more_important_tasks(choose_keyword, all_set_variables, task_list):
    """
    Determines dependencies of a choose keyword.

    Parameters
    ----------
    choose_keyword : str
        The keyword, starting with choose, which is looked through to check if
        there are any dependencies that must be resolved first to correctly
        resolve this one.
    all_set_variables : dict
        All variables that can be set
    task_list : list
        A list in the order in which tasks must be resolved for
        ``choose_keyword`` to make sense.

    Returns
    -------
    task_list
        A list of choices which must be made in order for choose_keyword to
        make sense.
    """
    keyword = choose_keyword.replace("choose_", "")
    if "cores_per_node" in keyword:
        pass  # pdb.set_trace()
    for model in all_set_variables:
        for choose_thing in all_set_variables[model]:
            # logging.debug("Choose_thing = %s", choose_thing)
            for (host, keyword_that_is_set) in all_set_variables[model][choose_thing]:
                if (
                    keyword_that_is_set == keyword
                    or keyword_that_is_set == keyword.replace(model + ".", "")
                ):
                    if (model, choose_thing) not in task_list:
                        task_list.insert(0, (model, choose_thing))
                        add_more_important_tasks(
                            choose_thing, all_set_variables, task_list
                        )
                        return task_list
                    else:
                        raise KeyError("Opps cyclic dependency: %s" % task_list)
    return task_list


def recursive_run_function(tree, right, level, func, *args, **kwargs):
    """Recursively runs func on all nested dicts.

    Tree is a list starting at the top of the config dictionary, where it will
    be labeled "top"

    Parameters
    ----------
    tree : list
        Where in the dictionary you are
    right :
        The value of the last key in `tree`
    level : str, one of "mappings", "atomic", "always"
        When to perform func
    func : callable
        An function to perform on all levels where the type of ``right`` is in
        ``level``. See the Notes for how this function's call signature should
        look.
    *args :
        Passed to func
    **kwargs :
        Passed to func

    Returns
    -------
    right

    Note
    ----
    The ``func`` argument must be a callable (i.e. a function) and **must**
    have a call signature of the following form:

    .. code::

        def func(tree, right, *args, **kwargs)

    """

    if len(tree) > 100:
        print("Maximum recursion depth exceeded.")
        print(tree)
        print(func)
        print(right)
        sys.exit(-1)

    # logging.debug("Top of function")
    # logging.debug("tree=%s", tree)
    if level == "mappings":
        do_func_for = [dict, list]
    elif level == "atomic":
        do_func_for = [str, int, float, Date]
        if six.PY2:
            do_func_for.append(unicode)
    elif level == "always":
        do_func_for = [str, dict, list, int, float, bool]
    elif level == "keys":
        do_func_for = []
    else:
        do_func_for = []

    # Python 2/3 error in YAML parser, bad workaround:
    if six.PY2:
        if isinstance(right, unicode):
            logging.warning("Unicode type detected, converting to a regular string!")
            right = right.encode("utf-8")
            assert isinstance(right, str)
            logging.warning(right)

    logging.debug("Type right: %s", type(right))
    logging.debug("Do func for: %s", do_func_for)

    if level == "keys" and isinstance(right, dict):
        keys = list(right)
        for key in keys:
            old_value = right[key]
            returned_key = func(tree + [key], key, *args, **kwargs)
            del right[key]
            right.update({returned_key: old_value})

    # logger.debug("right is a %s!", type(right))
    if type(right) in do_func_for:
        if isinstance(right, dict):
            keys = list(right)
            for key in keys:
                value = right[key]
                logging.debug("Deleting key %s", key)
                logging.debug(
                    "Start func %s with %s, %s sent from us",
                    func.__name__,
                    tree + [key],
                    value,
                    "type_of_sender=dict",
                )
                returned_dict = func(tree + [key], value, *args, **kwargs)
                del right[key]
                # logger.debug("Back out of func %s", func.__name__)
                # logger.debug("Got as returned_dict: %s", returned_dict)
                right.update(returned_dict)
        # elif isinstance(right, list):
        #    for index, item in enumerate(right):
        #        del right[0]
        #        right.append(func(tree + [None], item, *args, **kwargs))
        else:
            right = func(tree + [None], right, *args, **kwargs)

    # logger.debug("finished with do_func_for")

    if isinstance(right, list):
        newright = []
        for index, item in enumerate(right):
            new_item = recursive_run_function(
                tree + [None], item, level, func, *args, **kwargs
            )
            """
                We are not supposed to understand this from Sept 01 2020 on
                takes care of list_to_multikey returning a list, that needs to be
                merged into the existing list rather than become a list within lists
                extremely undesirable way of solving this
                Miguels fault
            """
            if type(item) == str and "[[" in item and func == list_to_multikey:
                newright += new_item
            else:
                newright.append(new_item)
        right = newright
    elif isinstance(right, dict):
        keys = list(right)
        for key in keys:
            # Avoid doing this for ``prev_run`` chapters, this is not needed as the
            # previous config is already resolved
            if key == "prev_run":  # PrevRunInfo
                continue  # PrevRunInfo
            value = right[key]
            right[key] = recursive_run_function(
                tree + [key], value, level, func, *args, **kwargs
            )
    return right


def recursive_get(config_to_search, config_elements):
    """
    Recusively gets entries in a nested dictionary in the form ``outer_key.middle_key.inner_key = value``

    Given a list of config elements in the form above (e.g. the result of
    splitting the string ``"outer_key.middle_key.inner_key".split(".")``` on
    the dot), the value "value" of the innermost nest is returned.

    Parameters
    ----------
    config_to_search : dict
        The dictionary to search through
    config_elements : list
        Each part of the next level of the dictionary to search, as a list.

    Returns
    -------
        The value associated with the nested dictionary specified by ``config_elements``.

    Note
    ----
        This is actually just a wrapper around the function
        ``actually_recursive_get``, which is needed to pop off standalone model
        configurations.
    """
    logging.debug("Incoming config elements: %s", config_elements)
    my_config_elements = copy.deepcopy(config_elements)
    this_config = my_config_elements.pop(0)

    logger.debug("this_config=%s", this_config)
    logger.debug("config_to_search=%s", config_to_search)
    try:
        result = config_to_search[this_config]
    except:
        raise ValueError(
            "Exactly None! Couldn't find an answer for:", my_config_elements
        )
    # This looks dangerous too...
    if my_config_elements:
        return recursive_get(result, my_config_elements)

    # Unicode vs Str again
    if six.PY2:
        if isinstance(result, list):
            for index, entry in enumerate(result):
                if isinstance(entry, unicode):
                    logging.critical("Changing unicode to str!")
                    result[index] = str(index)
        elif isinstance(result, unicode):
            logging.critical("Changing unicode to str!")
            entries_of_key = str(entries_of_key)

    return result


def determine_regex_list_match(test_str, regex_list):
    result = []
    for regex in regex_list:
        logging.debug("Checking %s against %s", test_str, regex)
        result.append(regex.match(test_str))
    logging.debug("Will return %s" % any(result))
    return any(result)


def find_variable(tree, rhs, full_config, white_or_black_list, isblacklist):
    raw_str = rhs
    if not tree[-1]:
        tree = tree[:-1]
    if isinstance(raw_str, str) and "${" in raw_str:
        ok_part, rest = raw_str.split("${", 1)
        var, new_raw = rest.split("}", 1)
        if ((determine_regex_list_match(var, white_or_black_list)) != isblacklist) and (
            not determine_regex_list_match(var, constant_blacklist)
        ):
            var_result, var_attrs = actually_find_variable(tree, var, full_config)

            if type(var_result) == str:
                if "${" in var_result:
                    var_result = find_variable(
                        tree,
                        var_result,
                        full_config,
                        white_or_black_list,
                        isblacklist,
                    )

                if "$((" in var_result:
                    var_result = do_math_in_entry(tree, var_result, full_config)

            if var_attrs:
                rentry = []
                if not isinstance(var_result, Date):
                    var_result = var_result.replace(DATE_MARKER, "")
                    entry = Date(var_result, full_config["general"]["calendar"])
                else:
                    entry = var_result
                for attr in var_attrs.split("!"):
                    rentry.append(str(getattr(entry, attr)))
                var_result = "".join(rentry)

            # if var_result:
            # BUG/FIXME: Note that this means that we **always** will get
            # back a string if a variable is replaced!
            if type(var_result) not in [list]:
                ok_part, var_result, more_rest = (
                    str(ok_part),
                    str(var_result),
                    str(new_raw),
                )

                if "${" in ok_part + var_result + more_rest:
                    raw_str = find_variable(
                        tree,
                        ok_part + var_result + more_rest,
                        full_config,
                        white_or_black_list,
                        isblacklist,
                    )
                else:
                    raw_str = ok_part + var_result + more_rest

            else:
                return var_result
    return raw_str


class EsmParserError(Exception):
    """Raise this error when the parser has problems"""


def actually_find_variable(tree, rhs, full_config):
    config_elements = rhs.split(".")
    valid_names = list(full_config)
    logging.debug(valid_names)
    if config_elements[0] not in valid_names:
        config_elements.insert(0, tree[0])

    full_varname = config_elements[-1]
    if "!" in full_varname:
        var_name, var_attr = full_varname.split("!", 1)
    else:
        var_name, var_attr = full_varname, None

    config_elements[-1] = var_name
    original_config_elements = copy.deepcopy(config_elements)
    try:
        var_result = recursive_get(full_config, config_elements)
        # return var_result
    except ValueError:
        # Maybe it is in the general:
        try:
            config_elements = original_config_elements
            logging.debug(config_elements)
            config_elements[0] = "general"
            var_result = recursive_get(full_config, config_elements)
            # return var_result
        except:
            raise EsmParserError(
                "Sorry, a variable was not resolved: %s not found" % (rhs)
            )

    return var_result, var_attr


def list_to_multikey(tree, rhs, config_to_search, ignore_list, isblacklist):
    """
    A recursive_run_function conforming func which puts any list based key to a
    multikey elsewhere. Sorry, that sounds confusing even to me, and I wrote
    the function.

    Parameters
    ----------
    tree : list

    rhs : str

    config_to_search : dict


    Notes
    -----
    Internal variable definitions in this function; based upon the example:
    prefix_[[streams-->STREAM]]_postfix

    + ``ok_part``: ``prefix_``
    + ``actual_list``: ``streams-->STREAM``
    + ``key_in_list``: ``streams``
    + ``value_in_list``: ``STREAM``
    + ``entries_of_key``: list of actual chapter ``streams``, e.g. ``[accw, echam6, e6hrsp, ...]``
    """
    list_fence = "[["
    list_end = "]]"
    if tree:
        lhs = tree[-1]
        if isinstance(lhs, str) and lhs:
            if list_fence in lhs:
                return_dict = {}
                ok_part, rest = lhs.split(list_fence, 1)
                actual_list, new_raw = rest.split(list_end, 1)
                key_in_list, value_in_list = actual_list.split("-->", 1)
                # PG: THIS NEEDS TO BE OFF!!!
                # if isblacklist and not determine_regex_list_match(
                #    key_in_list, ignore_list
                # ):
                #    return {lhs: rhs}
                key_elements = key_in_list.split(".")
                entries_of_key, _ = actually_find_variable(
                    tree, key_in_list, config_to_search
                )

                if isinstance(entries_of_key, str):
                    entries_of_key = [entries_of_key]

                if isinstance(rhs, str):
                    return_dict2 = {}
                    for key in entries_of_key:
                        return_dict2[
                            lhs.replace("[[" + actual_list + "]]", str(key)).replace(
                                value_in_list, str(key)
                            )
                        ] = rhs.replace(value_in_list, str(key))

                if isinstance(rhs, list):
                    replaced_list = []
                    for item in rhs:
                        if isinstance(item, str):
                            for key in entries_of_key:
                                replaced_list.append(
                                    item.replace(value_in_list, str(key))
                                )
                        else:
                            replaced_list.append(item)
                    return_dict2 = {}
                    for key in entries_of_key:
                        return_dict2[
                            lhs.replace("[[" + actual_list + "]]", str(key)).replace(
                                value_in_list, str(key)
                            )
                        ] = replaced_list

                def helper_dict_replacer(entry, value_in_list, replacement_key):
                    if isinstance(entry, str):
                        return entry.replace(value_in_list, replacement_key)
                    if isinstance(entry, list):
                        new_list = []
                        for i in entry:
                            if isinstance(i, str):
                                i = i.replace(value_in_list, replacement_key)
                            # Handle lists containing dicts
                            elif isinstance(i, dict):
                                # Go through each key/value and replace STREAM with value
                                keys = list(i)
                                for k in keys:
                                    v = i[k]
                                    del i[k]
                                    if value_in_list in k:
                                        new_k = (
                                            k.replace(value_in_list, replacement_key)
                                            if isinstance(k, str)
                                            else k
                                        )
                                    if value_in_list in v:
                                        new_v = (
                                            v.replace(value_in_list, replacement_key)
                                            if isinstance(v, str)
                                            else v
                                        )
                                    i[new_k] = new_v
                            new_list.append(i)
                        return new_list
                    if isinstance(entry, dict):
                        new_entry = {}
                        # pdb.set_trace()
                        for k in list(entry):
                            v = entry[k]
                            # del entry[k]
                            if value_in_list in k:
                                new_k = (
                                    k.replace(value_in_list, replacement_key)
                                    if isinstance(k, str)
                                    else k
                                )
                            else:
                                new_k = k
                            # pdb.set_trace()
                            new_v = helper_dict_replacer(
                                v, value_in_list, replacement_key
                            )
                            # pdb.set_trace()
                            new_entry[new_k] = new_v
                        return new_entry

                if isinstance(rhs, dict):
                    replacement_dict = {}
                    keys_of_rhs_dict = list(rhs)
                    for replacement_key in entries_of_key:
                        inner_replacement_dict = replacement_dict[
                            ok_part + replacement_key + new_raw
                        ] = {}
                        for rhs_key in keys_of_rhs_dict:
                            entry = rhs[rhs_key]
                            # del rhs[rhs_key]
                            if isinstance(rhs_key, str):
                                new_rhs_key = (
                                    rhs_key.replace(value_in_list, replacement_key)
                                    if isinstance(rhs_key, str)
                                    else rhs_key
                                )
                                new_entry = helper_dict_replacer(
                                    entry, value_in_list, replacement_key
                                )
                                inner_replacement_dict[new_rhs_key] = new_entry
                            else:
                                raise NotImplementedError(
                                    "Nested multikey replacement for dicts is only implement for str keys right now!"
                                )
                    return_dict2 = replacement_dict

                if list_fence in new_raw:
                    for key, value in six.iteritems(return_dict2):
                        return_dict.update(
                            list_to_multikey(
                                tree + [key],
                                value,
                                config_to_search,
                                ignore_list,
                                isblacklist,
                            )
                        )
                else:
                    return_dict = return_dict2
                return return_dict
            return {lhs: rhs}

        if isinstance(rhs, str) and list_fence in rhs:
            rhs_list = []
            ok_part, rest = rhs.split(list_fence, 1)
            actual_list, new_raw = rest.split(list_end, 1)
            # seb-wahl: check if a [[ ...]] entry in the string parsed contains
            # '-->' to avoid a crash if a shell command such as 'if [[ ...]]; then' is parsed
            if "-->" in actual_list:
                key_in_list, value_in_list = actual_list.split("-->", 1)
                key_elements = key_in_list.split(".")
                entries_of_key, _ = actually_find_variable(
                    tree, key_in_list, config_to_search
                )
                if isinstance(entries_of_key, str):
                    entries_of_key = [entries_of_key]
                for entry in entries_of_key:
                    rhs_list.append(
                        rhs.replace("[[" + actual_list + "]]", str(entry)).replace(
                            value_in_list, str(entry)
                        )
                    )
                # if isinstance(entries_of_key, str):
                #    entries_of_key = [entries_of_key]
                # for entry in entries_of_key:
                #    rhs_list.append(
                #        rhs.replace("[[" + actual_list + "]]", str(entry)).replace(
                #            value_in_list, str(entry)
                #        )
                #    )
            if list_fence in new_raw:
                out_list = []
                for rhs_listitem in rhs_list:
                    out_list += list_to_multikey(
                        tree + [None],
                        rhs_listitem,
                        config_to_search,
                        ignore_list,
                        isblacklist,
                    )
                rhs_list = out_list
            return rhs_list
        elif isinstance(lhs, bool):
            return {lhs: rhs}
    return rhs


def determine_computer_from_hostname():
    """
    Determines which yaml config file is needed for this computer

    Notes
    -----
    The supercomputer must be registered in the ``all_machines.yaml`` file in
    order to be found.

    Returns
    -------
    str
        A string for the path of the computer specific yaml file.
    """
    all_computers = yaml_file_to_dict(FUNCTION_PATH + "/machines/all_machines.yaml")
    for this_computer in all_computers:
        for computer_pattern in all_computers[this_computer].values():
            if isinstance(computer_pattern, str):
                if re.match(computer_pattern, socket.gethostname()) or re.match(
                    computer_pattern, socket.getfqdn()
                ):
                    return FUNCTION_PATH + "/machines/" + this_computer + ".yaml"
            elif isinstance(computer_pattern, (list, tuple)):
                # Pluralize to avoid confusion:
                computer_patterns = computer_pattern
                for pattern in computer_patterns:
                    if re.match(pattern, socket.gethostname()):
                        return FUNCTION_PATH + "/machines/" + this_computer + ".yaml"
    logging.warning(
        "The yaml file for this computer (%s) could not be determined!"
        % socket.gethostname()
    )
    logging.warning("Continuing with generic settings...")
    return FUNCTION_PATH + "/machines/generic.yaml"

    # raise FileNotFoundError(
    #    "The yaml file for this computer (%s) could not be determined!"
    #    % socket.gethostname()
    # )


def do_math_in_entry(tree, rhs, config):
    if not tree[-1]:
        tree = tree[:-1]
    entry = rhs
    if isinstance(entry, Date):
        return entry
    if "${" in str(entry):
        return entry
    entry = " " + str(entry) + " "
    while "$((" in entry:
        math, after_math = entry.split("))", 1)
        math, before_math = math[::-1].split("(($", 1)
        math = math[::-1]
        before_math = before_math[::-1]
        if DATE_MARKER in math:
            all_dates = []
            steps = math.split(" ")
            steps = [step for step in steps if step]
            math = ""
            index = 0
            for step in steps:
                if step in ["+", "-"]:
                    math = math + step
                elif "seconds" in step:
                    tupel = (
                        "(0, 0, 0, 0, 0,"
                        + step.replace("seconds", "")
                        .replace('"', "")
                        .replace("'", "")
                        .strip()
                        + ")"
                    )
                    math = math + tupel
                elif "minutes" in step:
                    tupel = (
                        "(0, 0, 0, 0,"
                        + step.replace("minutes", "")
                        .replace('"', "")
                        .replace("'", "")
                        .strip()
                        + ", 0)"
                    )
                    math = math + tupel
                elif "hours" in step:
                    tupel = (
                        "(0, 0, 0,"
                        + step.replace("hours", "")
                        .replace('"', "")
                        .replace("'", "")
                        .strip()
                        + ", 0, 0)"
                    )
                    math = math + tupel
                elif "days" in step:
                    tupel = (
                        "(0, 0,"
                        + step.replace("days", "")
                        .replace('"', "")
                        .replace("'", "")
                        .strip()
                        + ", 0, 0, 0)"
                    )
                    math = math + tupel
                elif "months" in step:
                    tupel = (
                        "(0,"
                        + step.replace("months", "")
                        .replace('"', "")
                        .replace("'", "")
                        .strip()
                        + ", 0, 0, 0, 0)"
                    )
                    math = math + tupel
                elif "years" in step:
                    tupel = (
                        "("
                        + step.replace("years", "")
                        .replace('"', "")
                        .replace("'", "")
                        .strip()
                        + ", 0, 0, 0, 0, 0)"
                    )
                    math = math + tupel
                else:
                    all_dates.append(
                        Date(
                            step.replace(DATE_MARKER, ""), config["general"]["calendar"]
                        )
                    )
                    math = math + "all_dates[" + str(index) + "]"
                    index += 1
        result = eval(math)
        if type(result) == list:
            result = result[
                -1
            ]  # should be extended in the future - here: if list (= if diff between dates) than result in seconds
        result = str(result)
        entry = before_math + result + after_math
    # TODO MA: this is a provisional dirty fix for release. Get rid of this once a more
    # general solution is worked out
    # ORIGINAL LINE: return convert(entry.strip())
    return convert(entry.strip(), tree)


def mark_dates(tree, rhs, config):
    """Adds the ``DATE_MARKER`` to any entry who's key ends with ``"date"``"""
    if not tree[-1]:
        tree = tree[:-1]
    lhs = tree[-1]
    entry = rhs
    logging.debug(entry)
    # if "${" in str(entry):
    #    return entry
    if isinstance(lhs, str) and lhs.endswith("date"):
        entry = str(entry) + DATE_MARKER
    return entry


def marked_date_to_date_object(tree, rhs, config):
    """Transforms a marked date string into a Date object"""
    if not tree[-1]:
        tree = tree[:-1]
    lhs = tree[-1]
    entry = rhs
    if isinstance(entry, Date):
        return entry
    if "${" in str(entry):
        return entry
    if isinstance(lhs, str) and lhs.endswith("date"):
        # if isinstance(entry, str) and DATE_MARKER in entry and "<--" not in entry:
        while DATE_MARKER in entry and "${" not in entry:
            entry = entry.replace(DATE_MARKER, "")
            if "!" in entry:
                actual_date, date_attr = entry.split("!", 1)
            else:
                actual_date, date_attr = entry, None
            entry = Date(actual_date, config["general"]["calendar"])
            if date_attr:
                rentry = []
                for attr in date_attr.split("!"):
                    rentry.append(str(getattr(entry, attr)))
                return "".join(rentry)
            else:
                # return entry.output()
                return entry
    return entry


def unmark_dates(tree, rhs, config):
    """Removes the ``DATE_MARKER`` to any entry who's entry contains the ``DATE_MARKER``."""
    if not tree[-1]:
        tree = tree[:-1]
    lhs = tree[-1]
    entry = rhs
    if isinstance(entry, str) and DATE_MARKER in entry:
        entry = entry.replace(DATE_MARKER, "")
    return entry


def perform_actions(tree, rhs, config):
    if not tree[-1]:
        tree = tree[:-1]
    lhs = tree[-1]
    entry = rhs
    if type(entry) == str:
        if "[[" in entry:
            return rhs
        if "<--" in entry:
            left, newrhs = entry.split("<--")
            action, source = newrhs.split("--")
            if "${" in source:
                source = find_variable(
                    tree,
                    source,
                    config,
                    [],
                    True,
                )

            parameter = None
            if "(" in action:
                action = action.replace(")", "")
                action, parameter = action.split("(")
            if "format" in action:
                if "d" in parameter or "f" in parameter:
                    source = int(source)
                if parameter:
                    solved_rhs = parameter % source
                else:
                    solved_rhs = source
                newrhs = solved_rhs
                entry = left + newrhs
            if "fseq" in action:
                rest = source.replace("<--fseq-- ", "").strip()
                try:
                    start, stop, step, precision = rest.split(" ")
                except:
                    precision = 4
                    try:
                        start, stop, step = rest.split(" ")
                    except:
                        start, stop = rest.split(" ")
                        step = 1

                numpyentry = list(numpy.arange(float(start), float(stop), float(step)))
                entry = [round(float(number), precision) for number in numpyentry]
    return entry


def purify_booleans(tree, rhs, config):
    if not tree[-1]:
        tree = tree[:-1]
    lhs = tree[-1]

    entry = rhs
    if isinstance(entry, Date):
        return entry
    if entry in ["True", "true", "False", "false"]:
        entry = eval(entry.capitalize())
    return entry


def to_boolean(value):
    if type(value) == bool:
        return value
    elif value in ["True", "true", ".true."]:
        return True
    elif value in ["False", "false", ".false."]:
        return False


def could_be_bool(value):
    if type(value) == bool:
        return True
    elif type(value) == str:
        if value.strip() in ["True", "true", "False", "false", ".true.", ".false."]:
            return True
    return False


def could_be_int(value):
    try:
        int(value)
        return contains_underscore(value)
    except:
        try:
            intval = int(
                float(value)
            )  # that is actually necessary, because of int("48.0")
            if intval - float(value) == 0.0:
                return contains_underscore(value)
            else:
                return False
        except:
            return False


def could_be_float(value):
    try:
        float(value)
        return contains_underscore(value)
    except:
        return False


def could_be_complex(value):
    try:
        complex(value)
        return contains_underscore(value)
    except:
        return False


def contains_underscore(value):
    if isinstance(value, str):
        if "_" in value:
            return False
    return True


# TODO MA: this is a provisional dirty fix for release. Get rid of this once a more
# general solution is worked out
# ORIGINAL LINE: def convert(value):
def convert(value, tree=["NO_KEY"]):
    if tree:
        key = tree[-1]
        if key in keep_as_str:
            return value
    if could_be_bool(value):
        return to_boolean(value)
    elif could_be_int(value):
        return int(float(value))
    elif could_be_float(value):
        return float(value)
    elif could_be_complex(value):
        return complex(value)
    return value


def list_all_keys_with_priority_marker(config):
    all_keys = []
    for key in list(config):
        if isinstance(key, str):
            if priority_marker in key:
                all_keys.append(key)
            if isinstance(config[key], dict):
                list_all_keys_with_priority_marker(config[key])
    logging.critical(all_keys)
    return all_keys


def finish_priority_merge(config):
    all_keys = list(config)
    all_keys_with_priority_marker = list_all_keys_with_priority_marker(config)
    while all_keys_with_priority_marker:
        for key in all_keys:
            value = config[key]
            if isinstance(value, dict):
                return finish_priority_merge(config)
            if priority_marker in key:
                del config[key]
                config[key.replace(priority_marker, "")] = value
        # Recreate the test list
        all_keys_with_priority_marker = list_all_keys_with_priority_marker(config)


def choose_blocks(config, blackdict={}, isblacklist=True):
    global gray_list
    all_set_variables = {}
    all_names = list(config)
    gray_list_backup = gray_list.copy()
    while True:
        for name in all_names:
            all_set_variables[name] = {}
            name_chooses = list_all_keys_starting_with_choose(
                config[name], name, gray_list, isblacklist
            )
            if name_chooses == []:
                continue
            for key, block in name_chooses:
                all_set_variables[name][key] = determine_set_variables_in_choose_block(
                    block, all_names, name
                )

            task_list = model_with_choose, choose_key = find_one_independent_choose(
                all_set_variables
            )

        for key in list(all_set_variables):
            if not all_set_variables[key]:
                del all_set_variables[key]
        if not all_set_variables:
            break
        logging.debug("The task list is: %s", task_list)
        logging.debug("all_set_variables: %s", all_set_variables)
        if model_with_choose in list(blackdict):
            resolve_basic_choose(
                config,
                config[model_with_choose],
                choose_key,
                blackdict[model_with_choose],
            )
        else:
            resolve_basic_choose(config, config[model_with_choose], choose_key, {})
        del all_set_variables[model_with_choose][choose_key]
        logging.debug("Remaining all_set_variables=%s", all_set_variables)

    add_entries_to_chapter_in_config(config, all_names, config, all_names)
    remove_entries_from_chapter_in_config(config, all_names, config, all_names)
    gray_list = gray_list_backup.copy()


def find_key(d_search, k_search, exc_strings="", level="", paths2finds=[], sep="."):
    """
    Searches for a key inside a nested dictionary. It can search for an
    integer, or a piece of string. A list of strings can be given as an
    input to search for keys containing all of them. An additional list
    of strings can be specified for keys containing them be excluded
    from the findings. This is a recursive function.

    .. Note::
       Always define paths2finds, to avoid expansion of this list with
       consecutive calls.

    Parameters
    ----------
    d_search : dict
        The dictionary to be explored recursively.
    k_search : list, str, int
        String, integer or list of strings to be search for in ``d_search``.
    exc_strings : list, str
        String or list of strings for keys containing them to be excluded
        from the finds. When set to an empty string, nothing is excluded.
    level : string
        String specifying the full path to the currently evaluated
        dictionary. Each dictionary level in these strings is separated
        by a ``.``.
    paths2finds : list
        List of strings specifying the full path to the found keys in
        ``d_search``. Each dictionary level in these strings is separated
        by a the specified string in ``sep`` (default is ``"."``).
    sep : string
        String separator used in between each path component in
        ``paths2finds``.

    Return
    ------
    paths2finds : list
        List of strings specifying the full path to the found keys in
        ``d_search``. Each dictionary level in these strings is separated
        by a ``.``.
    """
    # Transform input to lists
    if isinstance(k_search, (str, int)):
        k_search = [k_search]
    elif not isinstance(k_search, list):
        raise Exception("k_search is not a string, a list or a list of strings")
    if isinstance(exc_strings, str):
        exc_strings = [exc_strings]
    elif not isinstance(exc_strings, list):
        raise Exception("exc_strings is not a string, or a list of strings")

    # Loop over the d_search keys
    for key in d_search.keys():
        strings_in_key = True
        # Check if the key meets the specified requirements
        for istr in k_search:
            # key is a string but it does not contained the searched string
            if isinstance(key, str) and isinstance(istr, str) and istr not in key:
                strings_in_key &= False
            # key is an integer but is not equal to the searched integer
            elif isinstance(key, int) and istr != key:
                strings_in_key &= False
            elif isinstance(key, float) and istr != key:
                strings_in_key &= False
            # key is neither an integer or a string
            elif (
                not isinstance(key, str)
                and not isinstance(key, int)
                and not isinstance(key, float)
            ):
                raise Warning(
                    "find_key only supports searches for keys that are string, integers,"
                    "floats and booleans"
                )
                strings_in_key &= False
        # Check if the key needs to be excluded
        for estr in exc_strings:
            # Nothing to exclude if the key is not a string or estr is empty
            if isinstance(key, str) and estr in key and len(estr) > 0:
                strings_in_key &= False

        # If the key meets the criteria, add the path to the paths2finds
        if strings_in_key:
            paths2finds.append(level + str(key))
        # If the key does not meet the criteria, but its value is a dictionary
        # keep searching inside (recursion).
        elif not strings_in_key and isinstance(d_search[key], dict):
            paths2finds = find_key(
                d_search[key],
                k_search,
                exc_strings,
                level + str(key) + sep,
                paths2finds,
                sep,
            )

    return paths2finds


def user_note(note_heading, note_text, color=colorama.Fore.YELLOW):
    """
    Notify the user about something. In the future this should also write in the log.

    Parameters
    ----------
    note_heading : str
        Note type used for the heading.
    text : str
        Text clarifying the note.
    """
    colorama.init(autoreset=True)
    reset_s = colorama.Style.RESET_ALL
    note_text = re.sub("``([^`]*)``", f"{color}\\1{reset_s}", note_text)
    print(f"\n{color}{note_heading}\n{'-' * len(note_heading)}")
    print(f"{note_text}\n")


def user_error(error_type, error_text, exit_code=1):
    """
    User-friendly error using ``sys.exit()`` instead of an ``Exception``.

    Parameters
    ----------
    error_type : str
        Error type used for the error heading.
    text : str
        Text clarifying the error.
    exit_code : int
        The exit code to send back to the parent process (default to 1)
    """
    error_title = "ERROR: " + error_type
    user_note(error_title, error_text, color=colorama.Fore.RED)
    sys.exit(exit_code)


class GeneralConfig(dict):  # pragma: no cover
    """All configs do this!"""

    def __init__(self, model, version, user_config):
        super(dict, self).__init__()

        if os.path.isfile(model + "-" + version):
            config_path = model + "-" + version
        elif os.path.isfile(model):
            config_path = model
        else:

            include_path, needs_load = look_for_file(model, model + "-" + version)
            if not include_path:
                include_path, needs_load = look_for_file(model, model)
            if not include_path:
                print(f"GeneralConfig: Couldn't find file for model {model}")
                sys.exit(-1)

        if needs_load:
            self.config = yaml_file_to_dict(include_path)
        else:
            self.config = include_path
        for attachment in CONFIGS_TO_ALWAYS_ATTACH_AND_REMOVE:
            attach_to_config_and_remove(self.config, attachment, all_config=None)

        self._config_init(user_config)
        for k, v in six.iteritems(self.config):
            self.__setitem__(k, v)
        del self.config

    def _config_init(self, user_config):
        raise NotImplementedError(
            "Subclasses of GeneralConfig must define a _config_init!"
        )


class ConfigSetup(GeneralConfig):  # pragma: no cover
    """Config Class for Setups"""

    def _config_init(self, user_config):
        # user_config should be ok already
        # self.config contains first yaml file and further_readings

        # setup_config:

        if not "defaults" in user_config:
            user_config["defaults"] = {}

        default_infos = {}

        # get the files in the defaults directory and exclude general.yaml
        yaml_files = os.listdir(DEFAULTS_DIR)
        if "general.yaml" in yaml_files:
            yaml_files.remove("general.yaml")

        for yaml_file in yaml_files:
            file_contents = yaml_file_to_dict(DEFAULTS_DIR + "/" + yaml_file)
            default_infos.update(file_contents)

        # construct the `defaults` section of the configuration
        user_config["defaults"].update(default_infos)

        computer_file = determine_computer_from_hostname()
        computer_config = yaml_file_to_dict(computer_file)

        if "general.yaml" in os.listdir(DEFAULTS_DIR):
            general_config = yaml_file_to_dict(f"{DEFAULTS_DIR}/general.yaml")
        else:
            general_config = {}

        setup_config = {
            "computer": computer_config,
            "general": general_config,
        }
        for attachment in CONFIGS_TO_ALWAYS_ATTACH_AND_REMOVE:
            attach_to_config_and_remove(
                setup_config["computer"],
                attachment,
                all_config=None,
                dont_overwrite_with_empty_value=True,
            )
        # Add the fake "model" name to the computer:
        setup_config["computer"]["model"] = "computer"
        logger.info("setup config is being updated with setup_relevant_configs")

        # distribute self.config into setup_config

        if "general" in self.config and "coupled_setup" in self.config["general"]:
            setup_config["general"].update({"standalone": False})
            # Resolve choose with include_models
            resolve_choose_with_var(
                "include_models",
                self.config["general"],
                current_model="general",
                user_config=user_config,
                setup_config=setup_config,
            )
            setup_config["general"]["include_models"] = self.config["general"][
                "include_models"
            ]

            # that should happen in Shell2Yaml
            if user_config["general"]["setup_name"] in user_config:
                user_config["general"].update(
                    user_config[user_config["general"]["setup_name"]]
                )
                del user_config[user_config["general"]["setup_name"]]
            dict_merge(setup_config, self.config)

            setup_config["general"]["valid_setup_names"] = valid_setup_names = list(
                setup_config
            )
            setup_config["general"]["valid_model_names"] = valid_model_names = []
        else:
            setup_config["general"].update({"standalone": True})
            setup_config["general"].update({"models": [self.config["model"]]})

            # Resolve choose with include_models
            resolve_choose_with_var(
                "include_models",
                self.config,
                user_config=user_config,
                setup_config=setup_config,
            )

            if "include_models" in self.config:
                setup_config["general"]["include_models"] = self.config[
                    "include_models"
                ]
            setup_config[self.config["model"]] = self.config

            setup_config["general"]["valid_setup_names"] = valid_setup_names = list(
                setup_config
            )
            setup_config["general"]["valid_setup_names"].remove(self.config["model"])
            setup_config["general"]["valid_model_names"] = valid_model_names = [
                self.config["model"]
            ]

        del self.config

        setup_config["general"].update(
            {
                "esm_function_dir": esm_function_dir,
                "esm_namelist_dir": esm_namelist_dir,
                "esm_runscript_dir": esm_runscript_dir,
                "expid": "test",
            }
        )

        # setup_config should be ok now
        # model_config:

        old_model_list = None
        if "include_models" in setup_config["general"]:
            new_model_list = []
            old_model_list = setup_config["general"]["include_models"].copy()
            if "models" in setup_config["general"]:
                old_model_list += setup_config["general"]["models"].copy()
            for model in setup_config["general"]["include_models"]:
                if (
                    not "-" in model
                    and model in user_config
                    and "version" in user_config[model]
                ):
                    new_model_list.append(model + "-" + user_config[model]["version"])
                elif (
                    not "-" in model
                    and model in setup_config
                    and "version" in setup_config[model]
                ):
                    new_model_list.append(model + "-" + setup_config[model]["version"])
                else:
                    new_model_list.append(model)
            setup_config["general"]["include_models"] = new_model_list

        model_config = {}
        attach_to_config_and_reduce_keyword(
            setup_config["general"], model_config, "include_models", "models"
        )

        if old_model_list:
            # print (old_model_list)
            setup_config["general"]["models"] = old_model_list

        if "models" in setup_config["general"]:
            # Solve the variables within choose_ blocks that need to be solved early
            # (i.e. include_models, versions...)
            for model in setup_config["general"]["models"]:
                # Solve the variable for each configuration type
                for this_config in [user_config, setup_config, model_config]:
                    if model in this_config:
                        # Resolve the target variable
                        for var in early_choose_vars:
                            resolve_choose_with_var(
                                var,
                                this_config.get(model),
                                user_config=user_config,
                                model_config=model_config,
                                setup_config=setup_config,
                            )
                        # Special treatment for "include_models"
                        attach_to_config_and_reduce_keyword(
                            this_config[model], model_config, "include_models", "models"
                        )
            for model in list(model_config):
                for attachment in CONFIGS_TO_ALWAYS_ATTACH_AND_REMOVE:
                    attach_to_config_and_remove(
                        model_config[model], attachment, all_config=None
                    )

        # Allows the ``general`` section to be able to handle attachable files (e.g.
        # ``further_reading``)
        for attachment in CONFIGS_TO_ALWAYS_ATTACH_AND_REMOVE:
            attach_to_config_and_remove(
                setup_config["general"], attachment, all_config=None
            )

        # if "models" in setup_config["general"]:
        #    new_model_list = []
        #    for model in setup_config["general"]["models"]:
        #        new_model_list.append(model.split("-")[0])
        #    setup_config["general"]["models"] = new_model_list

        for model in list(model_config):
            setup_config["general"]["valid_model_names"].append(model)
            # valid_model_names.append(list(model_config)) happens automatically

        # model_config should be ok now
        # merge everything

        logging.debug("Valid Setup Names = %s", valid_setup_names)
        logging.debug("Valid Model Names = %s", valid_model_names)

        self._blackdict = blackdict = priority_merge_dicts(
            user_config, setup_config, priority="first"
        )
        self.config = priority_merge_dicts(blackdict, model_config, priority="first")

        if not "coupled_setup" in self.config["general"]:
            self._blackdict = blackdict = user_config

        # deniz: if the user-defined forcing_sources (inside the runscript) is
        # a dictionary with multiple levels then the users need to provide
        # 'overwrite' key at the level that they want to change. Otherwise,
        # the parser basically appends that to the forcing_sources and since
        # Python dictionaries are unordered noone can guarantee which source is
        # taken
        self.config = dict_overwrite(
            sender=user_config,
            receiver=self.config,
            key_path=[],
            verbose=self.config["general"].get("verbose", False),
        )

        # pprint_config(self.config)
        # sys.exit(0)

    def finalize(self):
        self.run_recursive_functions(self)
        del self._blackdict

    def run_recursive_functions(self, config, isblacklist=True):
        logging.debug("Top of run recursive functions")
        recursive_run_function([], config, "atomic", mark_dates, config)
        recursive_run_function([], config, "atomic", perform_actions, config)
        recursive_run_function(
            [],
            config,
            "atomic",
            find_variable,
            config,
            gray_list,
            isblacklist=isblacklist,
        )
        recursive_run_function(
            [],
            config,
            "keys",
            find_variable,
            config,
            gray_list,
            isblacklist=isblacklist,
        )
        recursive_run_function([], config, "atomic", do_math_in_entry, config)
        recursive_run_function([], config, "atomic", marked_date_to_date_object, config)
        recursive_run_function([], config, "atomic", unmark_dates, config)
        recursive_run_function(
            [],
            config,
            "always",
            list_to_multikey,
            config,
            gray_list,
            isblacklist=isblacklist,
        )
        recursive_run_function([], config, "atomic", purify_booleans, config)
        recursive_run_function([], config, "atomic", perform_actions, config)
