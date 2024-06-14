import os
import pathlib
import shutil
import subprocess
import sys

import questionary
from colorama import Fore

import esm_parser
import esm_tools
from loguru import logger

from . import filelists
from .helpers import end_it_all, evaluate, write_to_log


def run_job(config):
    """
    Run prepexp job.

    Parameters
    ----------
    config : dict
        Dictionary containing the configuration information.
    """
    evaluate(config, "prepexp", "prepexp_recipe")
    return config


def color_diff(diff):
    """
    Adds color to text from a diff:
    - Green for lines starting with ``+``
    - Red for lines starting with ``-``
    - Blue for lines starting with ``^``

    Parameters
    ----------
    diff : iterable object of strings to be colored
    """
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
    tools_dir = f"{scriptsdir}/esm_tools/configs"
    namelists_dir = f"{scriptsdir}/esm_tools/namelists"

    if config["general"]["verbose"]:
        logger.info(f"Started from : {fromdir}")
        logger.info(f"Scripts Dir : {scriptsdir}")

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
        logger.info(f"Copying standard yamls from: {esm_tools.get_config_filepath()}")
        esm_tools.copy_config_folder(tools_dir)
    if not os.path.isdir(namelists_dir):
        logger.info(
            f"Copying standard namelists from: {esm_tools.get_namelist_filepath()}"
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

    # If ``fromdir`` and ``scriptsdir`` are different, we are not in the experiment.
    # In this case, update the runscript if necessary.
    if not fromdir == scriptsdir:
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

    return config


def _call_esm_runscripts_internally(config, command, exedir):
    """
    - Removes update flags from command input.
    - Adds additional flags to command input.
    - Adds esm_runscripts command if necessary.
    - Calls esm_runscipts internally in a subprocess call.

    Parameters
    ----------
    config : dict
        Dictionary containing the configuration information.
    command : str
        Command or esm_runscripts arguments
    exedir : str
        Path from which the command is to be executed.

    """

    # Remove the update option otherwise it will enter an infinite loop.
    options_to_remove = [" -U ", " --update "]
    for option in options_to_remove:
        command = command.replace(option, " ")

    # Check if 'esm_runscripts' command is given in 'command' argument.
    if not command.startswith("esm_runscripts"):
        command = f"esm_runscripts {command}"

    # Add non-interaction flags, current jobtype, and current task (phase) [-t]
    # if not already in 'command'
    non_interaction_flags = [
        "--no-motd",
        f"--last-jobtype {config['general']['jobtype']}",
        f"-t {config['general']['jobtype']}",
    ]
    for ni_flag in non_interaction_flags:
        # prevent continuous addition of ``ni_flag``
        if ni_flag not in command:
            command += f" {ni_flag} "

    # Check if the path exists, in which 'command' should be executed
    if os.path.exists(exedir):
        subprocess.check_call(command.split(), cwd=exedir)
    else:
        error_type = "runtime error in function ``_call_esm_runscripts_internally``"
        error_text = f"{exedir} does not exists. Aborting."
        esm_parser.user_error(error_type, error_text)

    logger.debug(command)

    # Exit after resubmission of esm_runscripts
    end_it_all(config)


def call_esm_runscripts_from_prepexp(config):
    """
    Recipe step that creates a esm_runscripts command and submits this
    to the function that modifies (if necessary) and executes this command
    in a subprocess call, if the current folder is NOT the experiment folder.
    The function will return and do nothing, if it is called already
    from the experiment folder.


    Parameters
    ----------
    config : dict
        Dictionary containing the configuration information.

    Returns
    -------
    config : dict
        Dictionary containing the configuration information.
    """

    gconfig = config["general"]

    fromdir = os.path.realpath(gconfig["started_from"])
    scriptsdir = os.path.realpath(gconfig["experiment_scripts_dir"])

    # Return if already called from the experiment folder without update flag
    if (fromdir == scriptsdir) and not gconfig["update"]:
        logger.debug("Started from the experiment folder, continuing...")
        return config

    # Not computing but initialisation
    else:
        logger.debug("Not started from experiment folder, restarting...")

        scriptsdir = os.path.realpath(gconfig["experiment_scripts_dir"])

        original_command = gconfig["original_command"]

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

        _call_esm_runscripts_internally(config, new_command, scriptsdir)

        return config


def _create_folders(config, filetypes):
    """
    Generates the experiment file tree. Folders are created for every filetype
    except for "ignore".

    Parameters
    ----------
    config : dict
        Dictionary containing the configuration information.
    filetypes: list

    """
    for filetype in filetypes:
        if not filetype == "ignore":
            if not filetype == "work":
                if not os.path.exists(config[f"experiment_{filetype}_dir"]):
                    os.makedirs(config[f"experiment_{filetype}_dir"])
            if not os.path.exists(config[f"thisrun_{filetype}_dir"]):
                os.makedirs(config[f"thisrun_{filetype}_dir"])


def _create_setup_folders(config):
    """
    Creates all folders for an experiment. This is the plugin function called
    during the recipe. See also ``_create_folders`` for actual creation

    This also creates a small marker file at the top of
    the experiment so that the "root" can be found from inside.

    Parameters
    ----------
    config : dict
        Dictionary containing the configuration information.
    """
    _create_folders(config["general"], config["general"]["all_filetypes"])
    with open(
        f"{config['general']['experiment_dir']}/.top_of_exp_tree", "w"
    ) as top_marker:
        top_marker.write(f"Top of experiment {config['general']['expid']}")
    return config


def _create_component_folders(config):
    """
    Parameters
    ----------
    config : dict
        Dictionary containing the configuration information.
    """

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
    config : dict
        The experiment configuration

    Return
    ------
    config : dict
        As per convention for the plug-in system; this gives back the
        entire config.

    Attention
    ---------
        Calling this has some filesystem side effects. If the run number in
        the general configuration is set to 1, and a file exists for
        ``general.exp_log_file``; this file is removed; and re-initialized.
    """

    experiment_dir = config["general"]["experiment_dir"]
    expid = config["general"]["expid"]
    jobtype = config["general"].get("jobtype", None)
    jobtype = f"_{jobtype}" if jobtype else ""
    jobid = config["general"].get("jobid", None)
    jobid = f"_{jobid}" if jobid else ""
    it_coupled_model = config["general"]["iterative_coupled_model"]
    datestamp = config["general"]["run_datestamp"]

    if config["general"]["run_number"] == 1:
        if os.path.isfile(config["general"]["experiment_log_file"]):
            os.remove(config["general"]["experiment_log_file"])

        log_msg = f"# Beginning of Experiment {expid}"
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
            f"{experiment_dir}/log/"
            f"{expid}_{it_coupled_model}esm_runscripts{jobtype}_{datestamp}{jobid}.log"
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
    if not os.path.isfile(f"{scriptsdir}/{tfile}"):
        oldscript = f"{fromdir}/{tfile}"
        logger.info(oldscript)
        shutil.copy2(oldscript, scriptsdir)
    # If the target path exists compare the two scripts
    else:
        import difflib

        import esm_parser

        script_o = open(f"{fromdir}/{tfile}").readlines()
        script_t = open(f"{scriptsdir}/{tfile}").readlines()

        diffobj = difflib.SequenceMatcher(a=script_t, b=script_o)
        # If the files are different
        if not diffobj.ratio() == 1:
            # Find differences
            differences = (
                f"{fromdir}/{tfile} differs from " + f"{scriptsdir}/'{tfile}:\n"
            )
            for line in color_diff(difflib.unified_diff(script_t, script_o)):
                differences += line

            # If the --update flag is used, notify that the target script will
            # be updated and do update it
            if gconfig["update"]:
                esm_parser.user_note(
                    f"Original {file_type} different from target",
                    f"{differences}\n{scriptsdir}/{tfile} will be updated!",
                )
                oldscript = f"{fromdir}/{tfile}"
                logger.info(oldscript)
                shutil.copy2(oldscript, scriptsdir)
            # If the --update flag is not called, exit with an error showing the
            # user how to proceed
            else:
                esm_parser.user_note(
                    f"Original {file_type} different from target",
                    differences
                    + "\n"
                    + "Note: You can choose to use ``-U`` flag in the "
                    + "``esm_runscripts`` call to automatically update the runscript "
                    + f"(WARNING: This will overwrite your {file_type} in the "
                    + "experiment folder!)\n",
                )
                update_choice = questionary.confirm(
                    f"Do you want that {scriptsdir}/{tfile} is "
                    + "updated with the above changes?"
                ).ask()
                if update_choice:
                    oldscript = f"{fromdir}/{tfile}"
                    logger.info(oldscript)
                    shutil.copy2(oldscript, scriptsdir)
                    logger.info(f"{scriptsdir}/{tfile} updated!")
                else:
                    logger.info("Submission stopped")
                    sys.exit(1)


def _copy_preliminary_files_from_experiment_to_thisrun(config):
    """
    - Copies the setup *.date file from <experiment>/scripts/ folder
      to <experiment>/run_xxxxxxxx-xxxxxxxx/scripts/ folder.
    - Copies the runscript yaml file from current folder (<experiment>/scripts)
      to <experiment>/run_xxxxxxxx-xxxxxxxx/scripts/<runscript>
    - Copies 'additional_files' (if any, e.g. fesom_output.yaml, that are called
      via 'further_reading' in the runscript or other config file) from ...
      to <experiment>/run_xxxxxxxx-xxxxxxxx/scripts/ folder.

    Parameters
    ----------
    config : dict
        Dictionary containing the configuration information.
    """

    filelist = [
        (
            "scripts",
            f"{config['general']['expid']}_{config['general']['setup_name']}.date",
            "copy",
        ),
        (
            "scripts",
            f"{config['general']['scriptname']}",
            "copy",
        ),
    ]

    for additional_file in config["general"].get("additional_files", []):
        filelist.append(("scripts", additional_file, "copy"))

    for filetype, filename, copy_or_link in filelist:
        source = config["general"].get(f"experiment_{filetype}_dir", "")
        dest = config["general"].get(f"thisrun_{filetype}_dir", "")

        method = filelists.get_method(copy_or_link)

        if os.path.isfile(f"{source}/{filename}"):
            method(f"{source}/{filename}", f"{dest}/{filename}")

    return config
