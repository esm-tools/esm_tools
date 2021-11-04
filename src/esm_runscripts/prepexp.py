import os
import shutil
import subprocess
import copy
import pathlib

import esm_rcfile
import six
import yaml
from esm_calendar import Date
from colorama import Fore, Back, Style, init

import esm_tools
import esm_parser

from .batch_system import batch_system
from .filelists import copy_files, log_used_files
from .helpers import end_it_all, evaluate, write_to_log
from .namelists import Namelist
from loguru import logger

from colorama import Fore


def run_job(config):
    evaluate(config, "prepexp", "prepexp_recipe")
    return config


def color_diff(diff):
    for line in diff:
        if line.startswith("+"):
            yield Fore.GREEN + line + Fore.RESET
        elif line.startswith("-"):
            yield Fore.RED + line + Fore.RESET
        elif line.startswith("^"):
            yield Fore.BLUE + line + Fore.RESET
        else:
            yield line


def copy_tools_to_thisrun(config):
    """
    Copies the tools, namelists and runscripts to the experiment directory,
    making sure that they don't overwrite previously existing files unless
    the ``-U`` flag is used.
    Parameters
    ----------
    config : dict
        Dictionary containing the configuration information.
    """
    gconfig = config["general"]

    # Directory where the original runscript is
    fromdir = os.path.realpath(gconfig["started_from"])
    # Directory where the runscript is copied to
    scriptsdir = os.path.realpath(gconfig["experiment_scripts_dir"])
    # basedir + expid
    expdir = os.path.realpath(gconfig["experiment_dir"])

    # Paths inside the experiment directory where esm_tools and namelists
    # are copied to. Those are not functional but a reference to what was
    # the original state when the experiment was firstly started
    tools_dir = scriptsdir + "/esm_tools/functions"
    namelists_dir = scriptsdir + "/esm_tools/namelists"

    if config["general"]["verbose"]:
        print("Started from :", fromdir)
        print("Scripts Dir : ", scriptsdir)

    # Update namelists and esm_tools. These have no effect on the final
    # simulation as only the installed esm_tools with their runscripts
    # are used, unless esm_tools and namelists paths are manually defined
    # by the model or user (not recommended for the esm_tools path)
    if os.path.isdir(tools_dir) and gconfig["update"]:
        shutil.rmtree(tools_dir, ignore_errors=True)
    if os.path.isdir(namelists_dir) and gconfig["update"]:
        shutil.rmtree(namelists_dir, ignore_errors=True)

    # In case there is no esm_tools or namelists in the experiment folder,
    # copy from the default esm_tools path
    if not os.path.isdir(tools_dir):
        print("Copying standard yamls from: ", esm_rcfile.EsmToolsDir("FUNCTION_PATH"))
        esm_tools.copy_config_folder(tools_dir)
    if not os.path.isdir(namelists_dir):
        print(
            "Copying standard namelists from: ",
            esm_rcfile.EsmToolsDir("NAMELIST_PATH"),
        )
        esm_tools.copy_namelist_folder(namelists_dir)

    # check for recursive creation of the file tree. This prevents the risk of
    # creating a run directory tree inside the `scriptsdir`
    # example:
    # fromdir:    /mnt/lustre02/work/project/username/basedir
    # scriptsdir: /mnt/lustre02/work/project/username/basedir/expid/scripts
    # it is assumed that if 5th parent of the scriptsdir is inside the fromdir
    # then probably some sort of infinite recursion is entered. These lines
    # protect such problems
    scriptsdir_deep_parents = list(pathlib.Path(scriptsdir).parents)[5:]
    deep_nesting_found = pathlib.Path(expdir) in scriptsdir_deep_parents
    if deep_nesting_found:
        error_type = "runtime error"
        error_text = (
            f"deep recursion is detected in {__file__}:\n"
            f"- scriptsdir:         {scriptsdir}\n"
            f"- fromdir:            {fromdir}\n"
            f"- experiment dir:     {expdir}"
        )
        # exit right away to prevent further recursion. There might still be
        # running instances of esmr_runscripts and something like
        # `killall esm_runscripts` might be required
        esm_parser.user_error(error_type, error_text)

    # If ``fromdir`` and ``scriptsdir`` are the same, this is already a computing
    # simulation which means we want to use the script in the experiment folder,
    # so no copying is needed
    if (fromdir == scriptsdir) and not gconfig["update"]:
        if config["general"]["verbose"]:
            print("Started from the experiment folder, continuing...")
        return config
    # Not computing but initialisation
    else:
        if not fromdir == scriptsdir:
            if config["general"]["verbose"]:
                print("Not started from experiment folder, restarting...")
        else:
            print("Tools were updated, restarting...")

        # At this point, ``fromdir`` and ``scriptsdir`` are different. Update the
        # runscript if necessary
        update_runscript(
            fromdir, scriptsdir, gconfig["scriptname"], gconfig, "runscript"
        )

        if gconfig.get("iterative_coupling", False):
            index = 1
            while "model" + str(index) in config:
                update_runscript(
                    fromdir,
                    scriptsdir,
                    config["model" + str(index)]["runscript"],
                    gconfig,
                    "runscript",
                )
                index += 1

        # Update the ``additional_files`` if necessary
        for tfile in gconfig["additional_files"]:
            update_runscript(fromdir, scriptsdir, tfile, gconfig, "additional file")

        # remove the update option otherwise it will enter an infinite loop
        original_command = gconfig["original_command"]
        options_to_remove = [" -U ", " --update "]
        for option in options_to_remove:
            original_command = original_command.replace(option, " ")

        # Before resubmitting the esm_runscripts, the path of the runscript
        # needs to be modified. Remove the absolute/relative path
        runscript_absdir, runscript = os.path.split(gconfig["runscript_abspath"])
        original_command_list = original_command.split()
        new_command_list = []
        for command in original_command_list:
            # current command will contain the full path, so replace it with
            # the YAML file only since we are going to execute it from the
            # `scriptsdir` now
            if runscript in command:
                # gconfig['scriptname'] or `runscript` only contains the YAML file name
                command = runscript
            new_command_list.append(command)

        new_command = " ".join(new_command_list)
        restart_command = f"cd {scriptsdir}; esm_runscripts {new_command}"

        # prevent continuous addition of --no-motd
        if not "--no-motd" in restart_command:
            restart_command += " --no-motd "

        if config["general"]["verbose"]:
            print(restart_command)
        os.system(restart_command)

        gconfig["profile"] = False
        end_it_all(config)


def _create_folders(config, filetypes):
    """
    Generates the experiment file tree. Foldres are created for every filetype
    except for "ignore".
    """
    for filetype in filetypes:
        if not filetype == "ignore":
            if not filetype == "work":
                if not os.path.exists(config["experiment_" + filetype + "_dir"]):
                    os.makedirs(config["experiment_" + filetype + "_dir"])
            if not os.path.exists(config["thisrun_" + filetype + "_dir"]):
                os.makedirs(config["thisrun_" + filetype + "_dir"])


def _create_setup_folders(config):
    """
    Creates all folders for an experiment. This is the plugin function called
    during the recipe. See also ``_create_folders`` for actual creation

    This also creates a small marker file at the top of
    the experiment so that the "root" can be found from inside.
    """
    _create_folders(config["general"], config["general"]["all_filetypes"])
    with open(
        config["general"]["experiment_dir"] + "/.top_of_exp_tree", "w"
    ) as top_marker:
        top_marker.write(f"Top of experiment {config['general']['expid']}")
    return config


def _create_component_folders(config):
    for component in config["general"]["valid_model_names"]:
        _create_folders(config[component], config["general"]["all_model_filetypes"])
    return config


def initialize_experiment_logfile(config):
    """
    Initializes the log file for the entire experiment.

    Creates a file ``${BASE_DIR}/${EXPID}/log/${EXPID}_${setup_name}.log``
    to keep track of start/stop times, job id numbers, and so on. Use the
    function ``write_to_log`` to put information in this file afterwards.

    The user can specify ``experiment_log_file`` under the ``general``
    section of the configuration to override the default name. Timestamps
    for each message are given by the section
    ``experiment_log_file_dateformat``, or defaults to ``Tue Mar 17
    09:36:38 2020``, i.e. ``"%c"``. Please use ``stftime`` compatable
    formats, as described here: https://strftime.org

    Parameters
    ----------
    dict :
        The experiment configuration

    Return
    ------
    dict :
        As per convention for the plug-in system; this gives back the
        entire config.

    Attention
    ---------
        Calling this has some filesystem side effects. If the run number in
        the general configuration is set to 1, and a file exists for
        ``general.exp_log_file``; this file is removed; and re-initialized.
    """

    if config["general"]["run_number"] == 1:
        if os.path.isfile(config["general"]["experiment_log_file"]):
            os.remove(config["general"]["experiment_log_file"])

        log_msg = f"# Beginning of Experiment {config['general']['expid']}"
        write_to_log(config, [log_msg], message_sep="")

        write_to_log(
            config,
            [
                str(config["general"]["jobtype"]),
                str(config["general"]["run_number"]),
                str(config["general"]["current_date"]),
                str(config["general"]["jobid"]),
                "- start",
            ],
        )

    # Write trace-log file now that we know where to do that
    if "trace_sink" in dir(logger):
        logfile_path = (
            f"{config['general']['experiment_dir']}/log"
            f"/{config['general']['expid']}_esm_runscripts_"
            f"{config['general']['run_datestamp']}.log"
        )

        logger.trace_sink.def_path(logfile_path)

    return config


def update_runscript(fromdir, scriptsdir, tfile, gconfig, file_type):
    """
    Updates the script ``tfile`` in the directory ``scriptdir`` with the file in
    the directory ``fromdir`` if the update flag (``-U``) is used during the call of
    ``esm_runscripts``. If that flag is not used and the source and target are different
    then raises a user-friendly error recommending to use the ``-U`` flag with the warning
    that the files will be overwritten.
    Parameters
    ----------
    cls : obj
        Compute object.
    fromdir : str
        Path of the source.
    scriptsdir : str
        Path of the target.
    tfile : str
        Name of the script to be updated.
    gconfig : dict
        Dictionary containing the general information about the compute task.
    file_type : str
        String specifying the nature of the file, only necessary for printing information
        and for the error description.
    Exceptions
    ----------
    UserError
        If the target and source are different and the ``-U`` flag is not used when calling
        ``esm_runscripts``, returns an error.
    """

    # if `tfile` contains a full path of the runscript then remove the leading path
    tfile = os.path.basename(tfile)

    # If the target file in ``scriptsdir`` does not exist, then copy the file
    # to the target.
    if not os.path.isfile(scriptsdir + "/" + tfile):
        oldscript = fromdir + "/" + tfile
        print(oldscript)
        shutil.copy2(oldscript, scriptsdir)
    # If the target path exists compare the two scripts
    else:
        import difflib
        import esm_parser

        script_o = open(fromdir + "/" + tfile).readlines()
        script_t = open(scriptsdir + "/" + tfile).readlines()

        diffobj = difflib.SequenceMatcher(a=script_t, b=script_o)
        # If the files are different
        if not diffobj.ratio() == 1:
            # Find differences
            differences = (
                f"{fromdir + '/' + tfile} differs from "
                + f"{scriptsdir + '/' + tfile}:\n"
            )
            for line in color_diff(difflib.unified_diff(script_t, script_o)):
                differences += line

            # If the --update flag is used, notify that the target script will
            # be updated and do update it
            if gconfig["update"]:
                esm_parser.user_note(
                    f"Original {file_type} different from target",
                    differences + "\n" + f"{scriptsdir + '/' + tfile} will be updated!",
                )
                oldscript = fromdir + "/" + tfile
                print(oldscript)
                shutil.copy2(oldscript, scriptsdir)
            # If the --update flag is not called, exit with an error showing the
            # user how to proceed
            else:
                esm_parser.user_note(
                    f"Original {file_type} different from target",
                    differences
                    + "\n"
                    + "Note: You can choose to use -U flag in the esm_runscripts call "
                    + "to automatically update the runscript (WARNING: This "
                    + f"will overwrite your {file_type} in the experiment folder!)\n",
                )
                correct_input = False
                while not correct_input:
                    update_choice = input(
                        f"Do you want that {scriptsdir + '/' + tfile} is "
                        + "updated with the above changes? (y/n): "
                    )
                    if update_choice == "y":
                        correct_input = True
                        oldscript = fromdir + "/" + tfile
                        print(oldscript)
                        shutil.copy2(oldscript, scriptsdir)
                        print(f"{scriptsdir + '/' + tfile} updated!")
                    elif update_choice == "n":
                        correct_input = True
                        esm_parser.user_error(
                            f"Original {file_type} different from target",
                            differences
                            + "\n"
                            + "You can choose to -U flag in the esm_runscripts call "
                            + "to update the runscript without asking (WARNING: This "
                            + f"will overwrite your {file_type} in the experiment folder!)\n\n",
                        )
                    else:
                        print(f"'{update_choice}' is not a valid answer.")


def _copy_preliminary_files_from_experiment_to_thisrun(config):
    # I don't like this one bit. DB
    filelist = [
        (
            "scripts",
            f"{config['general']['expid']}_{config['general']['setup_name']}.date",
            "copy",
        )
    ]

    for filetype, filename, copy_or_link in filelist:
        source = config["general"]["experiment_" + filetype + "_dir"]
        dest = config["general"]["thisrun_" + filetype + "_dir"]
        if copy_or_link == "copy":
            method = shutil.copy2
        elif copy_or_link == "link":
            method = os.symlink
        if os.path.isfile(source + "/" + filename):
            method(source + "/" + filename, dest + "/" + filename)
    this_script = config["general"]["scriptname"]
    shutil.copy2("./" + this_script, config["general"]["thisrun_scripts_dir"])

    for additional_file in config["general"]["additional_files"]:
        shutil.copy2(additional_file, config["general"]["thisrun_scripts_dir"])
    return config
