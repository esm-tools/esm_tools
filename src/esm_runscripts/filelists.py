import copy
import filecmp
import glob
import os
import pathlib
import re
import shutil
import sys

import f90nml
import yaml

import esm_parser
from loguru import logger

from . import helpers


def rename_sources_to_targets(config):
    # Purpose of this routine is to make sure that filetype_sources and
    # filetype_targets are set correctly, and _in_work is unset
    for filetype in config["general"]["all_model_filetypes"]:
        for model in config["general"]["valid_model_names"] + ["general"]:

            sources = filetype + "_sources" in config[model]
            targets = filetype + "_targets" in config[model]
            in_work = filetype + "_in_work" in config[model]

            if (
                filetype in config["general"]["out_filetypes"]
            ):  # stuff to be copied out of work

                if sources and targets and in_work:
                    if (
                        not config[model][filetype + "_sources"]
                        == config[model][filetype + "_in_work"]
                    ):
                        logger.error(
                            "Mismatch between "
                            + filetype
                            + "_sources and "
                            + filetype
                            + "_in_work in model "
                            + model,
                        )
                        helpers.print_datetime(config)
                        sys.exit(-1)

                elif sources and targets and not in_work:
                    # all fine
                    pass

                elif sources and not targets:
                    logger.debug(
                        "Renaming sources to targets for filetype "
                        + filetype
                        + " in model "
                        + model,
                    )
                    helpers.print_datetime(config)
                    config[model][filetype + "_targets"] = copy.deepcopy(
                        config[model][filetype + "_sources"]
                    )
                    if in_work:
                        config[model][filetype + "_sources"] = copy.deepcopy(
                            config[model][filetype + "_in_work"]
                        )

                elif targets and not sources:
                    if in_work:
                        config[model][filetype + "_sources"] = copy.deepcopy(
                            config[model][filetype + "_in_work"]
                        )
                    else:
                        config[model][filetype + "sources"] = copy.deepcopy(
                            config[model][filetype + "_targets"]
                        )

            else:  # stuff to be copied into work

                if sources and targets and in_work:
                    if (
                        not config[model][filetype + "_targets"]
                        == config[model][filetype + "_in_work"]
                    ):
                        logger.error(
                            "Mismatch between "
                            + filetype
                            + "_targets and "
                            + filetype
                            + "_in_work in model "
                            + model,
                        )
                        helpers.print_datetime(config)
                        sys.exit(-1)

                elif sources and targets and not in_work:
                    # all fine
                    pass

                elif (not sources and in_work) or (not sources and targets):
                    logger.error(
                        filetype + "_sources missing in model " + model
                    )
                    helpers.print_datetime(config)
                    sys.exit(-1)

                elif sources and not targets:
                    if in_work:
                        config[model][filetype + "_targets"] = copy.deepcopy(
                            config[model][filetype + "_in_work"]
                        )
                    else:
                        config[model][filetype + "_targets"] = {}
                        for descrip, name in config[model][
                            filetype + "_sources"
                        ].items():
                            config[model][filetype + "_targets"].update(
                                {descrip: os.path.basename(name)}
                            )

            if in_work:
                del config[model][filetype + "_in_work"]

    return config


def complete_targets(config):
    for filetype in config["general"]["all_model_filetypes"]:
        for model in config["general"]["valid_model_names"] + ["general"]:
            if filetype + "_sources" in config[model]:
                for category in config[model][filetype + "_sources"]:
                    if not category in config[model][filetype + "_targets"]:
                        file_source = config[model][filetype + "_sources"][category]

                        # check if the file_source has the correct type. For
                        # unresolved variables they may still be a 'dict'
                        if not isinstance(file_source, (str, os.PathLike)):
                            # model, scenario and version are omitted to make
                            # error message less verbose
                            scenario = config[model].get("scenario", "UNDEFINED")
                            version = config[model].get("version", "UNDEFINED")

                            error_type = "Missing Configuration"
                            error_text = (
                                # comment-out the line below to provide more information
                                # f"Scenario {scenario} for the model {model} (version: {version}) has not been implemented yet. \n" +
                                f"The input file variable {category} of {filetype}_sources can not be fully resolved:\n\n"
                                + yaml.dump(file_source, indent=4)
                            )
                            esm_parser.user_error(error_type, error_text)
                        else:
                            config[model][filetype + "_targets"][category] = (
                                os.path.basename(file_source)
                            )

    return config


def complete_sources(config):
    logger.debug("::: Complete sources")
    helpers.print_datetime(config)
    for filetype in config["general"]["out_filetypes"]:
        for model in config["general"]["valid_model_names"] + ["general"]:
            if filetype + "_sources" in config[model]:
                for category in config[model][filetype + "_sources"]:
                    if not config[model][filetype + "_sources"][category].startswith(
                        "/"
                    ):
                        config[model][filetype + "_sources"][category] = (
                            config["general"]["thisrun_work_dir"]
                            + "/"
                            + config[model][filetype + "_sources"][category]
                        )
    return config


def reuse_sources(config):
    if config["general"]["run_number"] == 1:
        return config

    # MA: the changes below are to be able to specify model specific reusable_filetypes
    # without changing the looping order (a model loop nested inside a file-type loop)

    # Put together all the possible reusable file types
    all_reusable_filetypes = []
    for model in config["general"]["valid_model_names"] + ["general"]:
        all_reusable_filetypes = list(
            set(all_reusable_filetypes + config[model].get("reusable_filetypes", []))
        )
    # Loop through all the reusable file types
    for filetype in all_reusable_filetypes:
        for model in config["general"]["valid_model_names"] + ["general"]:
            # Get the model-specific reusable_filetypes, if not existing, get the
            # general ones
            model_reusable_filetypes = config[model].get(
                "reusable_filetypes", config["general"]["reusable_filetypes"]
            )
            # Apply changes from ``--update-files`` flag
            model_reusable_filetypes = helpers.update_reusable_filetypes(
                config, reusable_filetypes=model_reusable_filetypes
            )
            # If <filetype>_sources dictionary exists and filetype is in the
            # model-specific filetype list then add the sources
            if (
                filetype + "_sources" in config[model]
                and filetype in model_reusable_filetypes
            ):
                for category in config[model][filetype + "_sources"]:
                    config[model][filetype + "_sources"][category] = (
                        config[model]["experiment_" + filetype + "_dir"]
                        + "/"
                        + config[model][filetype + "_targets"][category]
                    )
    return config


def choose_needed_files(config):
    # aim of this function is to only take those files specified in fileytype_files
    # (if exists), and then remove filetype_files

    for filetype in config["general"]["all_model_filetypes"]:
        for model in config["general"]["valid_model_names"] + ["general"]:

            if not filetype + "_files" in config[model]:
                continue

            new_sources = new_targets = {}
            for category, name in config[model][filetype + "_files"].items():
                # TODO: change with user_error()
                if not name in config[model][filetype + "_sources"]:
                    logger.error(
                        "Implementation "
                        + name
                        + " not found for filetype "
                        + filetype
                        + " of model "
                        + model,
                    )
                    logger.error(config[model][filetype + "_files"])
                    logger.error(config[model][filetype + "_sources"])
                    helpers.print_datetime(config)
                    sys.exit(-1)
                new_sources.update(
                    {category: config[model][filetype + "_sources"][name]}
                )

            config[model][filetype + "_sources"] = new_sources

            all_categs = list(config[model][filetype + "_targets"].keys())
            for category in all_categs:
                if not category in config[model][filetype + "_sources"]:
                    del config[model][filetype + "_targets"][category]

            del config[model][filetype + "_files"]

    return config


def globbing(config):
    for filetype in config["general"]["all_model_filetypes"]:
        for model in config["general"]["valid_model_names"] + ["general"]:
            if filetype + "_sources" in config[model]:
                # oldconf = copy.deepcopy(config[model])
                for descr, filename in copy.deepcopy(
                    config[model][filetype + "_sources"]
                ).items():
                    if "*" in filename:
                        del config[model][filetype + "_sources"][descr]
                        # Save the wildcard string for later use when copying to target
                        wild_card = config[model].get(
                            f"{filetype}_sources_wild_card", {}
                        )
                        wild_card[descr] = filename.split("/")[-1]
                        config[model][filetype + "_sources_wild_card"] = wild_card
                        # skip subdirectories in file list, otherwise they
                        # will be listed as missing files later on
                        all_filenames = [
                            f for f in glob.glob(filename) if not os.path.isdir(f)
                        ]
                        running_index = 0

                        for new_filename in all_filenames:
                            newdescr = descr + "_glob_" + str(running_index)
                            config[model][filetype + "_sources"][
                                newdescr
                            ] = new_filename
                            if (
                                config[model][filetype + "_targets"][descr] == filename
                            ):  # source and target are identical if autocompleted
                                config[model][filetype + "_targets"][newdescr] = (
                                    os.path.basename(new_filename)
                                )
                            else:
                                config[model][filetype + "_targets"][newdescr] = config[
                                    model
                                ][filetype + "_targets"][descr]
                            running_index += 1

                        del config[model][filetype + "_targets"][descr]
    return config


def target_subfolders(config):
    for filetype in config["general"]["all_model_filetypes"]:
        for model in config["general"]["valid_model_names"] + ["general"]:
            if filetype + "_targets" in config[model]:
                for descr, filename in config[model][filetype + "_targets"].items():
                    # * only in targets if denotes subfolder
                    # TODO: change with user_error()
                    if not descr in config[model][filetype + "_sources"]:
                        esm_parser.user_error(
                            "Filelists",
                            f"No source found for target ``{name}`` in model "
                            f"``{model}``\n",
                        )
                    if "*" in filename:
                        source_filename = os.path.basename(
                            config[model][filetype + "_sources"][descr]
                        )
                        # directory wildcards are given as /*, wildcards in filenames are handled
                        # seb-wahl: directory wildcards are given as /*, wildcards in filenames are handled
                        # in routine 'globbing' above, if we don't check here, wildcards are handled twice
                        # for files and hence filenames of e.g. restart files are screwed up.
                        if filename.endswith("/*"):
                            config[model][filetype + "_targets"][descr] = (
                                filename.replace("*", source_filename)
                            )
                        elif "/" in filename:
                            # Return the correct target name
                            target_name = get_target_name_from_wildcard(
                                config, model, filename, filetype, descr
                            )
                            config[model][filetype + "_targets"][descr] = (
                                "/".join(filename.split("/")[:-1]) + "/" + target_name
                            )
                        else:
                            # Return the correct target name
                            target_name = get_target_name_from_wildcard(
                                config, model, filename, filetype, descr
                            )
                            config[model][filetype + "_targets"][descr] = target_name
                    elif filename.endswith("/"):
                        source_filename = os.path.basename(
                            config[model][filetype + "_sources"][descr]
                        )
                        config[model][filetype + "_targets"][descr] = (
                            filename + source_filename
                        )

    return config


def get_target_name_from_wildcard(config, model, filename, filetype, descr):
    """
    Given a taget ``filename`` path containing a wildcard (``*``), and the ``descr``
    key to an specific file (i.e. the one that points to the actual specific file),
    returns the ``target_name`` basename for that specific file, taking care of
    changing the name from source to target, if both are compatible (same number of
    ``*``).

    For example::
      filename = "<new_name>.*.nc"
      model = "fesom"
      filetype = "restart_in"
      config["fesom"]["restart_in_sources_wild_card"] = "<old_name>.*.nc"
      config["fesom"]["restart_in_sources"]["<restart_file>_glob_1"] = "<old_name>.temp.nc"

    Then the outcome will be substitutting ``temp`` into the ``*`` of ``filename``::
      target_name = "<new_name>.temp.nc"

    Note:: MA: This is very poorly generalized and too specific to the current use.
    I volunteer myself to clean the ``filelists.py``, and make this method more
    general.

    Parameters
    ----------
    config : dict
        The experiment configuration
    model : str
        Target component/model
    filename : str
        Target path, with or without wildcards
    filetype : str
        Target file type
    descr : str
        Key pointing at the specific file, which target name needs to be constructed

    Returns
    -------
    targer_name : str
        Basename of the specific target file associated to the ``descr`` key

    Raises
    ------
    user_error : esm_parser.user_error
        If source and target wildcard patterns do not match (different number of ``*``
        in the patterns), raises a user friendly error
    """

    source_filename = os.path.basename(config[model][filetype + "_sources"][descr])
    target_filename = os.path.basename(filename)

    # Get the general description of the filename
    gen_descr = descr.split("_glob_")[0]
    # Load the wild cards from source and target and split them
    # at the *
    wild_card_source_all = config[model][f"{filetype}_sources_wild_card"]
    wild_card_source = wild_card_source_all[gen_descr].split("*")
    wild_card_target = target_filename.split("*")
    # Check for syntax mistakes
    if len(wild_card_target) != len(wild_card_source):
        esm_parser.user_error(
            "Wild card",
            (
                "The wild card pattern of the source "
                + f"{wild_card_source} does not match with the "
                + f"target {wild_card_target}. Make sure the "
                + f"that the number of * are the same in both "
                + f"sources and targets."
            ),
        )
    # Loop through the pieces of the wild cards to create
    # the correct target name by substituting in the source
    # name
    target_name = source_filename
    for wcs, wct in zip(wild_card_source, wild_card_target):
        target_name = target_name.replace(wcs, wct)

    return target_name


def complete_restart_in(config):
    for model in config["general"]["valid_model_names"]:
        if (
            not config[model]["lresume"] and config["general"]["run_number"] == 1
        ):  # isn't that redundant? if run_number > 1 then lresume == True?
            if "restart_in_sources" in config[model]:
                del config[model]["restart_in_sources"]
            if "restart_in_targets" in config[model]:
                del config[model]["restart_in_targets"]
            if "restart_in_intermediate" in config[model]:
                del config[model]["restart_in_intermediate"]
        if "restart_in_sources" in config[model]:
            for category in list(config[model]["restart_in_sources"].keys()):
                if not config[model]["restart_in_sources"][category].startswith("/"):
                    config[model]["restart_in_sources"][category] = (
                        config[model]["parent_restart_dir"]
                        + config[model]["restart_in_sources"][category]
                    )
    return config


def assemble_intermediate_files_and_finalize_targets(config):
    for filetype in config["general"]["all_model_filetypes"]:
        for model in config["general"]["valid_model_names"] + ["general"]:
            if filetype + "_targets" in config[model]:
                if not filetype + "_intermediate" in config[model]:
                    config[model][filetype + "_intermediate"] = {}
                for category in config[model][filetype + "_targets"]:
                    target_name = config[model][filetype + "_targets"][category]

                    interm_dir = (
                        config[model]["thisrun_" + filetype + "_dir"] + "/"
                    ).replace("//", "/")
                    if filetype in config["general"]["out_filetypes"]:
                        target_dir = (
                            config[model]["experiment_" + filetype + "_dir"] + "/"
                        ).replace("//", "/")
                        source_dir = (
                            config["general"]["thisrun_work_dir"] + "/"
                        ).replace("//", "/")
                        if not config[model][filetype + "_sources"][
                            category
                        ].startswith("/"):
                            config[model][filetype + "_sources"][category] = (
                                source_dir
                                + config[model][filetype + "_sources"][category]
                            )
                    else:
                        target_dir = (config["general"]["thisrun_work_dir"]).replace(
                            "//", "/"
                        )

                    config[model][filetype + "_intermediate"][category] = (
                        interm_dir + target_name
                    )
                    config[model][filetype + "_targets"][category] = (
                        target_dir + target_name
                    )

    return config


def find_valid_year(config, year):
    for entry in config:
        min_val = -50000000000
        max_val = 500000000000

        from_info = float(config[entry].get("from", min_val))
        to_info = float(config[entry].get("to", max_val))

        if from_info <= year <= to_info:
            return entry

    error_type = "Year Error"
    error_text = f"Sorry, no entry found for year {year} in config {config}"
    esm_parser.user_error(error_type, error_text)


def replace_year_placeholder(config):
    for filetype in config["general"]["all_model_filetypes"]:
        for model in config["general"]["valid_model_names"] + ["general"]:
            if filetype + "_targets" in config[model]:
                if filetype + "_additional_information" in config[model]:
                    for file_category in config[model][
                        filetype + "_additional_information"
                    ]:
                        if file_category in config[model][filetype + "_targets"]:

                            all_years = [config["general"]["current_date"].year]

                            if (
                                "need_timestep_before"
                                in config[model][filetype + "_additional_information"][
                                    file_category
                                ]
                            ):
                                all_years.append(config["general"]["prev_date"].year)
                            if (
                                "need_timestep_after"
                                in config[model][filetype + "_additional_information"][
                                    file_category
                                ]
                            ):
                                all_years.append(config["general"]["next_date"].year)
                            if (
                                "need_year_before"
                                in config[model][filetype + "_additional_information"][
                                    file_category
                                ]
                            ):
                                all_years.append(
                                    config["general"]["current_date"].year - 1
                                )
                            if (
                                "need_year_after"
                                in config[model][filetype + "_additional_information"][
                                    file_category
                                ]
                            ):
                                all_years.append(
                                    config["general"]["current_date"].year + 1
                                )

                            if (
                                "need_2years_before"
                                in config[model][filetype + "_additional_information"][
                                    file_category
                                ]
                            ):
                                all_years.append(
                                    config["general"]["current_date"].year - 2
                                )

                            if (
                                "need_2years_after"
                                in config[model][filetype + "_additional_information"][
                                    file_category
                                ]
                            ):
                                all_years.append(
                                    config["general"]["current_date"].year + 2
                                )

                            all_years = list(
                                dict.fromkeys(all_years)
                            )  # removes duplicates

                            # loop over all years (including year_before & after)
                            # change replace the @YEAR@ variable with the
                            # corresponding year
                            for year in all_years:
                                new_category = file_category + "_year_" + str(year)

                                # if the source contains 'from' or 'to' information
                                # then they have a dict type
                                if (
                                    type(
                                        config[model][filetype + "_sources"][
                                            file_category
                                        ]
                                    )
                                    == dict
                                ):

                                    # process the 'from' and 'to' information in
                                    # file sources and targets
                                    config[model][filetype + "_sources"][
                                        new_category
                                    ] = find_valid_year(
                                        config[model][filetype + "_sources"][
                                            file_category
                                        ],
                                        year,
                                    )

                                    config[model][filetype + "_targets"][
                                        new_category
                                    ] = config[model][filetype + "_targets"][
                                        file_category
                                    ]

                                    # replace @YEAR@ in the targets
                                    if (
                                        "@YEAR@"
                                        in config[model][filetype + "_targets"][
                                            new_category
                                        ]
                                    ):
                                        new_target_name = config[model][
                                            filetype + "_targets"
                                        ][new_category].replace("@YEAR@", str(year))

                                        config[model][filetype + "_targets"][
                                            new_category
                                        ] = new_target_name

                                    # replace @YEAR@ in the sources
                                    if (
                                        "@YEAR@"
                                        in config[model][filetype + "_sources"][
                                            new_category
                                        ]
                                    ):
                                        new_source_name = config[model][
                                            filetype + "_sources"
                                        ][new_category].replace("@YEAR@", str(year))

                                        config[model][filetype + "_sources"][
                                            new_category
                                        ] = new_source_name

                                # value is not a dictionary. Ie. it does not
                                # have `from` or `to` attributes. This else
                                # block preserves these sections in the config.
                                else:
                                    # create `new_category` from `file_category`
                                    config[model][filetype + "_sources"][
                                        new_category
                                    ] = config[model][filetype + "_sources"][
                                        file_category
                                    ]

                                    config[model][filetype + "_targets"][
                                        new_category
                                    ] = config[model][filetype + "_targets"][
                                        file_category
                                    ]

                                    # replace @YEAR@ in the targets
                                    if (
                                        "@YEAR@"
                                        in config[model][filetype + "_targets"][
                                            new_category
                                        ]
                                    ):
                                        new_target_name = config[model][
                                            filetype + "_targets"
                                        ][new_category].replace("@YEAR@", str(year))

                                        config[model][filetype + "_targets"][
                                            new_category
                                        ] = new_target_name

                                    # replace @YEAR@ in the sources
                                    if (
                                        "@YEAR@"
                                        in config[model][filetype + "_sources"][
                                            new_category
                                        ]
                                    ):
                                        new_source_name = config[model][
                                            filetype + "_sources"
                                        ][new_category].replace("@YEAR@", str(year))

                                        config[model][filetype + "_sources"][
                                            new_category
                                        ] = new_source_name

                                # end if
                            # end of the for year loop

                            # deniz: new additions for @YEAR_1850@
                            # these are the Kinne aerosol files for the background
                            # aerosol concentration. They are needed for years
                            # 1849, 1850, and 1851. All these 3 files are the same
                            # and ECHAM needs them
                            if (
                                "@YEAR_1850@"
                                in config[model][filetype + "_targets"][file_category]
                            ):
                                # only target name is changed since source file is for a fixed year (1850)
                                for year in [1849, 1850, 1851]:
                                    new_category = file_category + "_year_" + str(year)

                                    # add the sources and targets to the config
                                    config[model][filetype + "_sources"][
                                        new_category
                                    ] = config[model][filetype + "_sources"][
                                        file_category
                                    ]

                                    config[model][filetype + "_targets"][
                                        new_category
                                    ] = config[model][filetype + "_targets"][
                                        file_category
                                    ]

                                    # construct the file target and add this to the config
                                    new_target_name = config[model][
                                        filetype + "_targets"
                                    ][new_category].replace("@YEAR_1850@", str(year))

                                    config[model][filetype + "_targets"][
                                        new_category
                                    ] = new_target_name

                            del config[model][filetype + "_sources"][file_category]
                            del config[model][filetype + "_targets"][file_category]
                # end of if additonal information

                year = config["general"]["current_date"].year

                for file_category in config[model][filetype + "_targets"]:

                    if (
                        type(config[model][filetype + "_sources"][file_category])
                        == dict
                    ):
                        config[model][filetype + "_sources"][file_category] = (
                            find_valid_year(
                                config[model][filetype + "_sources"][file_category],
                                year,
                            )
                        )
                    if "@YEAR@" in config[model][filetype + "_targets"][file_category]:
                        new_target_name = config[model][filetype + "_targets"][
                            file_category
                        ].replace("@YEAR@", str(year))

                        config[model][filetype + "_targets"][
                            file_category
                        ] = new_target_name

                    if "@YEAR@" in config[model][filetype + "_sources"][file_category]:
                        new_source_name = config[model][filetype + "_sources"][
                            file_category
                        ].replace("@YEAR@", str(year))

                        config[model][filetype + "_sources"][
                            file_category
                        ] = new_source_name

            # end of if filetype in target
        # end of model loop
    # end of filetype loop
    return config


def log_used_files(config):
    logger.debug("\n::: Logging used files")
    filetypes = config["general"]["relevant_filetypes"]
    expid = config["general"]["expid"]
    it_coupled_model_name = config["general"]["iterative_coupled_model"]
    datestamp = config["general"]["run_datestamp"]
    for model in config["general"]["valid_model_names"] + ["general"]:
        thisrun_config_dir = config[model]["thisrun_config_dir"]
        # this file contains the files used in the experiment
        flist_file = (
            f"{thisrun_config_dir}/"
            f"{expid}_{it_coupled_model_name}filelist_{datestamp}"
        )

        with open(flist_file, "w") as flist:
            flist.write(
                f"These files are used for \n"
                f"experiment {config['general']['expid']}\n"
                f"component {model}\n"
                f"date {config['general']['run_datestamp']}"
            )
            flist.write("\n")
            flist.write(80 * "-")
            for filetype in filetypes:
                if filetype + "_sources" in config[model]:
                    flist.write("\n" + filetype.upper() + ":\n")
                    for category in config[model][filetype + "_sources"]:
                        flist.write(
                            "\nSource: "
                            + config[model][filetype + "_sources"][category]
                        )
                        flist.write(
                            "\nExp Tree: "
                            + config[model][filetype + "_intermediate"][category]
                        )
                        flist.write(
                            "\nTarget: "
                            + config[model][filetype + "_targets"][category]
                        )
                        logger.debug(f"::: logging file category: {category}")
                        logger.debug(
                            (
                                f"- source: "
                                f'{config[model][filetype + "_sources"][category]}'
                            ),
                        )
                        logger.debug(
                            (
                                f"- target: "
                                f'{config[model][filetype + "_targets"][category]}'
                            ),
                        )
                        helpers.print_datetime(config)
                        flist.write("\n")
                flist.write("\n")
                flist.write(80 * "-")
    return config


def check_for_unknown_files(config):
    # files = os.listdir(config["general"]["thisrun_work_dir"])
    all_files = glob.iglob(
        config["general"]["thisrun_work_dir"] + "**/*", recursive=True
    )

    known_files = [
        config["general"]["thisrun_work_dir"] + "/" + "hostfile_srun",
        config["general"]["thisrun_work_dir"] + "/" + "namcouple",
        config["general"]["thisrun_work_dir"] + "/" + "coupling.xml",
    ]

    for filetype in config["general"]["all_model_filetypes"]:
        for model in config["general"]["valid_model_names"] + ["general"]:
            if filetype + "_sources" in config[model]:
                known_files += list(config[model][filetype + "_sources"].values())
                known_files += list(config[model][filetype + "_targets"].values())

    known_files = [os.path.realpath(known_file) for known_file in known_files]
    known_files = list(dict.fromkeys(known_files))

    if not "unknown_sources" in config["general"]:
        config["general"]["unknown_sources"] = {}
        config["general"]["unknown_targets"] = {}
        config["general"]["unknown_intermediate"] = {}

    unknown_files = []
    index = 0

    for thisfile in all_files:

        if os.path.realpath(thisfile) in known_files + unknown_files:
            continue
        config["general"]["unknown_sources"][index] = os.path.realpath(thisfile)
        config["general"]["unknown_targets"][index] = os.path.realpath(
            thisfile
        ).replace(
            os.path.realpath(config["general"]["thisrun_work_dir"]),
            os.path.realpath(config["general"]["experiment_unknown_dir"]),
        )
        config["general"]["unknown_intermediate"][index] = os.path.realpath(
            thisfile
        ).replace(
            os.path.realpath(config["general"]["thisrun_work_dir"]),
            os.path.realpath(config["general"]["thisrun_unknown_dir"]),
        )

        unknown_files.append(os.path.realpath(thisfile))

        index += 1
        logger.warning("Unknown file in work: " + os.path.realpath(thisfile))
        helpers.print_datetime(config)

    return config


def resolve_symlinks(config, file_source):
    if os.path.islink(file_source):
        points_to = os.path.realpath(file_source)

        # deniz: check if file links to itself. In UNIX
        # ln -s endless_link endless_link is a valid command
        if os.path.abspath(file_source) == points_to:
            logger.debug(f"file {file_source} links to itself")
            helpers.print_datetime(config)
            return file_source

        # recursively find the file that the link is pointing to
        return resolve_symlinks(config, points_to)
    else:
        return file_source


def copy_files(config, filetypes, source, target):
    """
    This function has a misleading name. It is not only used for copying, but also
    for moving or linking, depending on what was specified for the particular file
    or file type vie the ``file_movements``.

    Note: when the ``target`` is ``thisrun`` (intermediate folders) check whether the
    type of file is included in ``intermediate_movements``. If it's not, instead of
    moving the file to the intermediate folder it moves it to ``work``. This is an
    ugly fix to provide a fast solution to the problem that files are
    copied/moved/linked twice unnecessarily, and this affects inmensely the performance
    of high resolution simulations. A better fix is not made because ``filelists`` are
    being entirely reworked, but the fix cannot wait.

    Parameters
    ----------
    config : dict
        The general configuration
    filetypes : list
        List of file types to be copied/linked/moved
    source : str
        Specifies the source type, to be chosen between ``init``, ``thisrun``,
        ``work``.
    target : str
        Specifies the target type, to be chosen between ``init``, ``thisrun``,
        ``work``.
    """
    logger.debug("\n::: Copying files")
    helpers.print_datetime(config)

    successful_files = []
    missing_files = {}

    intermediate_movements = config["general"].get(
        "intermediate_movements",
        [],
    )

    if source == "init":
        text_source = "sources"
    elif source == "thisrun":
        text_source = "intermediate"
    elif source == "work":
        text_source = "sources"

    if target == "thisrun":
        text_target = "intermediate"
    elif target == "work":
        text_target = "targets"

    # Loop through the different filetypes (input, forcing, restart_in/out, ...)
    files_to_be_moved = []
    for filetype in [filetype for filetype in filetypes if not filetype == "ignore"]:
        # Loop through the components
        for model in config["general"]["valid_model_names"] + ["general"]:
            # If there is a source of this file type in the model
            if filetype + "_" + text_source in config[model]:
                this_text_target = text_target
                this_intermediate_movements = config[model].get(
                    "intermediate_movements", intermediate_movements
                )
                skip_intermediate = False
                if filetype not in intermediate_movements:
                    if text_target == "intermediate":
                        this_text_target = "targets"
                        skip_intermediate = True
                    elif text_source == "intermediate":
                        continue
                sourceblock = config[model][filetype + "_" + text_source]
                targetblock = config[model][filetype + "_" + this_text_target]
                # Loop through categories (file keys)
                for category in sourceblock:
                    movement_method = get_method(
                        get_movement(config, model, category, filetype, source, target)
                    )
                    file_source = os.path.normpath(sourceblock[category])
                    file_target = os.path.normpath(targetblock[category])
                    logger.debug(f"::: copying file category: {category}")
                    logger.debug(f"- source: {file_source}")
                    logger.debug(f"- target: {file_target}")
                    helpers.print_datetime(config)
                    # Skip movement if file exist
                    if file_source == file_target:
                        logger.debug(
                            f"Source and target paths are identical, skipping {file_source}",
                        )
                        helpers.print_datetime(config)
                        continue
                    dest_dir = os.path.dirname(file_target)
                    file_source = resolve_symlinks(config, file_source)
                    if not os.path.isdir(file_source):
                        try:
                            if not os.path.isdir(dest_dir):
                                # MA: ``os.makedirs`` creates the specified directory
                                # and the parent directories if the last don't exist
                                # (same as with ``mkdir -p <directory>>``)
                                os.makedirs(dest_dir)
                            if not os.path.isfile(file_source):
                                logger.warning(
                                    f"File not found: {file_source}",
                                )
                                helpers.print_datetime(config)
                                missing_files.update({file_target: file_source})
                                continue
                            if os.path.isfile(file_target) and filecmp.cmp(
                                file_source, file_target
                            ):
                                logger.debug(
                                    f"Source and target file are identical, skipping {file_source}",
                                )
                                helpers.print_datetime(config)
                                continue
                            files_to_be_moved.append({
                                "movement_method": movement_method,
                                "file_source": file_source,
                                "file_target": file_target,
                            })

                            # To avoid overwriting in general experiment folder
                            if skip_intermediate == True:
                                file_target = avoid_overwriting(
                                    config, file_source, file_target
                                )

                            # Execute movement
                            movement_method(file_source, file_target)

                            successful_files.append(file_source)
                        except IOError:
                            logger.error(
                                f"Could not copy {file_source} to {file_target} for unknown reasons.",
                            )
                            helpers.print_datetime(config)
                            missing_files.update({file_target: file_source})

    if missing_files:
        if not "files_missing_when_preparing_run" in config["general"]:
            config["general"]["files_missing_when_preparing_run"] = {}
        if config["general"]["verbose"]:
            logger.warning("\n\nWARNING: These files were missing:")
            for missing_file in missing_files:
                logger.warning(
                    f"- missing source: {missing_files[missing_file]}"
                )
                logger.warning(f"- missing target: {missing_file}")
                helpers.print_datetime(config)
        config["general"]["files_missing_when_preparing_run"].update(missing_files)
    return config


def avoid_overwriting(config, source, target):
    """
    Function that appends the date stamp to ``target`` if the target already exists.
    Additionally, if the target exists, it renames it with the previous run time stamp,
    and creates a link named ``target`` that points at the target with the current time
    stamp.

    Note: This function does not execute the file movement.

    Parameters
    ----------
    config : dict
        Simulation configuration
    source : str
        Path of the source of the file that will be copied/moved/linked
    target : src
        Path of the target of the file that will be copied/moved/linked
    """
    if os.path.isfile(target):
        if filecmp.cmp(source, target):
            return target

        date_stamped_target = f"{target}_{config['general']['run_datestamp']}"
        if os.path.isfile(date_stamped_target):
            esm_parser.user_note(
                "File movement conflict",
                f"The file ``{date_stamped_target}`` already exists. Skipping movement:\n"
                f"{soucer} -> {date_stamped_target}"
            )
            return target

        if os.path.islink(target):
            os.remove(target)
        else:
            os.rename(target, f"{target}_{config['general']['last_run_datestamp']}")

        os.symlink(date_stamped_target, target)
        target = date_stamped_target

    elif os.path.isdir(target):
        esm_parser.user_error(
            "File operation not supported",
            f"The target ``{target}`` is a folder, and this should not be happening "
            "here. Please, open an issue in www.github.com/esm-tools/esm_tools"
        )

    return target


def filter_allowed_missing_files(config):
    """
    Filters the general.files_missing_when_preparing_run dictionary to move any
    allowed missing files to a seperate dictionary.


    This function can be included to mark specific files as "allowed to be
    missing". A list of allowed missing files should be put into the model
    configuration::

        echam:
            allowed_missing_files:
                - "*restart*jsbid*"
                - "unit.*"

    The allowed missing files may either be globs or regular expressions. The
    filename (without full path) on either the source or the target is used for
    matching. Therefore, if a file is renamed in the process of being moved
    from the pool filesystem to the experiment tree, or to the work folder, an
    exclusion can be made both with the source name or with the target name.

    Parameters
    ----------
    config : dict
        The experiment configuration

    Returns
    -------
    config : dict
    """
    allowed_missing_files = config["general"].setdefault("allowed_missing_files", {})
    missing_files = config["general"].get("files_missing_when_preparing_run", {})
    # TODO(PG): Replace with logger statements
    logger.debug("Currently known missing files:")
    for k, v in missing_files.items():
        logger.debug(f"source: {k} --> target: {v}")
    remove_missing_files = []
    for missing_file_source, missing_file_target in missing_files.items():
        missing_file_source_fname = pathlib.Path(missing_file_source).name
        missing_file_target_fname = pathlib.Path(missing_file_target).name
        for model in config["general"]["valid_model_names"] + ["general"]:
            for allowed_missing_pattern in config[model].get(
                "allowed_missing_files", []
            ):
                if (
                    re.match(allowed_missing_pattern, missing_file_source_fname)
                    or missing_file_source_fname in glob.glob(allowed_missing_pattern)
                    or re.match(allowed_missing_pattern, missing_file_target_fname)
                    or missing_file_target_fname in glob.glob(allowed_missing_pattern)
                ):
                    # TODO(PG): Replace with logger statements
                    logger.debug(
                        f"Detected allowed missing file with {allowed_missing_pattern}"
                    )
                    logger.debug("Adding to allowed missing files:")
                    logger.debug(f"source: {missing_file_source}")
                    logger.debug(f"target: {missing_file_target}")
                    remove_missing_files.append(missing_file_source)
                    allowed_missing_files.update(
                        {missing_file_source: missing_file_target}
                    )
    for remove_missing in remove_missing_files:
        missing_files.pop(remove_missing)
    return config


def report_missing_files(config):
    # this list is populated by the ``copy_files`` function in filelists.py
    config = _check_fesom_missing_files(config)
    if "files_missing_when_preparing_run" in config["general"]:
        if not config["general"]["files_missing_when_preparing_run"] == {}:
            logger.warning("MISSING FILES:")
        for missing_file in config["general"]["files_missing_when_preparing_run"]:
            logger.warning(
                f'- missing source: {config["general"]["files_missing_when_preparing_run"][missing_file]}',
            )
            logger.warning(f"- missing target: {missing_file}")
            helpers.print_datetime(config)
        if not config["general"]["files_missing_when_preparing_run"] == {}:
            logger.info(80 * "=")
    return config


def _check_fesom_missing_files(config):
    """
    Checks for missing files in FESOM namelist.config

    Parameters
    ----------
    config : dict
        The experiment configuration

    Returns
    -------
    config : dict
    """
    if "fesom" in config["general"]["valid_model_names"]:
        namelist_config = f90nml.read(
            os.path.join(config["general"]["thisrun_work_dir"], "namelist.config")
        )
        for path_key, path in namelist_config["paths"].items():
            if path:  # Remove empty strings
                if not os.path.exists(path):
                    if "files_missing_when_preparing_run" not in config["general"]:
                        config["general"]["files_missing_when_preparing_run"] = {}
                    config["general"]["files_missing_when_preparing_run"][
                        path_key + " (from namelist.config in FESOM)"
                    ] = path
    return config


# FILE MOVEMENT METHOD STUFF


def create_missing_file_movement_entries(config):
    for model in config["general"]["valid_model_names"] + ["general"]:
        if not "file_movements" in config[model]:
            config[model]["file_movements"] = {}
        for filetype in config["general"]["all_model_filetypes"] + [
            "scripts",
            "unknown",
        ]:
            if not filetype in config[model]["file_movements"]:
                config[model]["file_movements"][filetype] = {}
    return config


def complete_one_file_movement(config, model, filetype, movement, movetype):
    if not movement in config[model]["file_movements"][filetype]:
        config[model]["file_movements"][filetype][movement] = movetype
    return config


def get_method(movement):
    if movement == "copy":
        return shutil.copy2
    elif movement == "link":
        return os.symlink
    elif movement == "move":
        return os.rename
    logger.info("Unknown file movement type, using copy (safest option).")
    return shutil.copy2


def complete_all_file_movements(config):

    mconfig = config["general"]
    general_file_movements = copy.deepcopy(mconfig.get("file_movements", {}))
    if "defaults.yaml" in mconfig:
        if "per_model_defaults" in mconfig["defaults.yaml"]:
            if "file_movements" in mconfig["defaults.yaml"]["per_model_defaults"]:
                mconfig["file_movements"] = copy.deepcopy(
                    mconfig["defaults.yaml"]["per_model_defaults"]["file_movements"]
                )
                del mconfig["defaults.yaml"]["per_model_defaults"]["file_movements"]
    if not "file_movements" in mconfig:
        mconfig["file_movements"] = {}
    # General ``file_movements`` win over default ones
    esm_parser.dict_merge(mconfig["file_movements"], general_file_movements)

    config = create_missing_file_movement_entries(config)

    for model in config["general"]["valid_model_names"] + ["general"]:
        mconfig = config[model]
        if "file_movements" in mconfig:
            for filetype in config["general"]["all_model_filetypes"] + [
                "scripts",
                "unknown",
            ]:
                if filetype in mconfig["file_movements"]:
                    if "all_directions" in mconfig["file_movements"][filetype]:
                        movement_type = mconfig["file_movements"][filetype][
                            "all_directions"
                        ]
                        for movement in [
                            "init_to_exp",
                            "exp_to_run",
                            "run_to_work",
                            "work_to_run",
                        ]:
                            config = complete_one_file_movement(
                                config, model, filetype, movement, movement_type
                            )
                        del mconfig["file_movements"][filetype]["all_directions"]

    for model in config["general"]["valid_model_names"] + ["general"]:
        mconfig = config[model]
        if "file_movements" in mconfig:
            # Complete movements with general
            esm_parser.new_deep_update(
                mconfig["file_movements"], config["general"].get("file_movements", {})
            )

            # Complete file specific movements with ``all_directions``
            for file_in_fm in mconfig["file_movements"]:
                # If it is a specific file, and not a file type
                if file_in_fm not in (
                    config["general"]["all_model_filetypes"] + ["scripts", "unknown"]
                ):
                    # Check syntax for restart files
                    if file_in_fm in mconfig.get(
                        "restart_in_files", {}
                    ) or file_in_fm in mconfig.get("restart_out_files", {}):
                        esm_parser.user_error(
                            "Movement direction not specified",
                            f"'{model}.file_movements.{file_in_fm}' refers to a "
                            + "restart file which can be moved/copied/link in two "
                            + "directions, into the 'work' folder and out of the "
                            + "'work' folder. Please, add the direction '_in' or "
                            + f"'_out' to '{file_in_fm}':\n\n{model}:\n    "
                            + f"file_movements:\n        {file_in_fm}_<in/out>:\n"
                            + f"            [ ... ]",
                        )
                    # Solve ``all_directions``
                    file_spec_movements = mconfig["file_movements"][file_in_fm]
                    if "all_directions" in file_spec_movements:
                        movement_type = file_spec_movements["all_directions"]
                        for movement in [
                            "init_to_exp",
                            "exp_to_run",
                            "run_to_work",
                            "work_to_run",
                        ]:
                            config = complete_one_file_movement(
                                config, model, file_in_fm, movement, movement_type
                            )
                        del mconfig["file_movements"][file_in_fm]["all_directions"]

            if "default" in mconfig["file_movements"]:
                if "all_directions" in mconfig["file_movements"]["default"]:
                    movement_type = mconfig["file_movements"]["default"][
                        "all_directions"
                    ]
                    for movement in [
                        "init_to_exp",
                        "exp_to_run",
                        "run_to_work",
                        "work_to_run",
                    ]:
                        config = complete_one_file_movement(
                            config, model, "default", movement, movement_type
                        )
                    del mconfig["file_movements"]["default"]["all_directions"]

                for movement in mconfig["file_movements"]["default"]:
                    movement_type = mconfig["file_movements"]["default"][movement]
                    for filetype in config["general"]["all_model_filetypes"] + [
                        "scripts",
                        "unknown",
                    ]:
                        config = complete_one_file_movement(
                            config, model, filetype, movement, movement_type
                        )
                del mconfig["file_movements"]["default"]
    return config


def get_movement(config, model, category, filetype, source, target):
    # Remove globing strings from category
    if isinstance(category, str):
        category = category.split("_glob_")[0]
    # Two type of directions are needed for restarts, therefore, the categories need an
    # "_in" or "_out" at the end.
    if filetype == "restart_in":
        category = f"{category}_in"
    elif filetype == "restart_out":
        category = f"{category}_out"
    # File specific movements
    file_spec_movements = config[model]["file_movements"].get(category, {})
    # Movements associated to ``filetypes``
    file_type_movements = config[model]["file_movements"][filetype]
    if source == "init":
        # Get the model-specific reusable_filetypes, if not existing, get the
        # general ones
        model_reusable_filetypes = config[model].get(
            "reusable_filetypes", config["general"]["reusable_filetypes"]
        )

        if (
            config["general"]["run_number"] == 1
            or filetype not in model_reusable_filetypes
        ):
            return file_spec_movements.get(
                "init_to_exp", file_type_movements["init_to_exp"]
            )
        else:
            return file_spec_movements.get(
                "exp_to_run", file_type_movements["exp_to_run"]
            )
    elif source == "work":
        return file_spec_movements.get(
            "work_to_run", file_type_movements["work_to_run"]
        )
    elif source == "thisrun" and target == "work":
        return file_spec_movements.get(
            "run_to_work", file_type_movements["run_to_work"]
        )
    else:
        # This should NOT happen
        logger.error(f"Error: Unknown file movement from {source} to {target}")
        helpers.print_datetime(config)
        sys.exit(42)


def assemble(config):
    config = complete_all_file_movements(config)
    config = rename_sources_to_targets(config)
    config = choose_needed_files(config)
    config = complete_targets(config)
    config = complete_sources(config)
    config = reuse_sources(config)
    config = replace_year_placeholder(config)

    config = complete_restart_in(config)
    config = globbing(config)
    config = target_subfolders(config)
    config = assemble_intermediate_files_and_finalize_targets(config)
    return config
