import sys
from datetime import datetime

import colorama
import esm_parser
import esm_plugin_manager
import esm_tools
import git
from loguru import logger


def vprint(message, config):
    if config["general"]["verbose"]:
        print(message)


# TODO: to be replaced by loguru. WIP (deniz)
def print_datetime(config):
    """prints the datetime of the operation if `verbose_line_numbers` option is True"""
    if config["general"].get("verbose_datetime_info", False):
        print(datetime.now(), flush=True)


def evaluate(config, job_type, recipe_name):

    # Check for a user defined compute recipe in the setup section of the
    # general section. If nothing is found, recipe_steps should evaluate to
    # None and the default is used
    try:
        setup_name = config["general"]["setup_name"]
        recipe_steps = config.get(setup_name, {}).get(recipe_name) or config[
            "general"
        ].get(recipe_name)
    except KeyError:
        print("Your configuration is incorrect.")
        print("It should include headings for the setup_name as well as general!")
        sys.exit(1)

    FUNCTION_PATH = esm_tools.get_config_filepath()

    ####################################################################################
    # LA: hotfix for non-matching python paths
    if (
        "/global/AWIsoft/tkleiner/lib/python2.7/site-packages/netCDF4-1.2.4-py2.7-linux-x86_64.egg"  # noqa: E501
        in sys.path
    ):
        sys.path.remove(
            "/global/AWIsoft/tkleiner/lib/python2.7/site-packages/netCDF4-1.2.4-py2.7-linux-x86_64.egg"  # noqa:501
        )
    if "/global/AWIsoft/tkleiner/lib/python2.7/site-packages" in sys.path:
        sys.path.remove("/global/AWIsoft/tkleiner/lib/python2.7/site-packages")
    ####################################################################################

    recipe = FUNCTION_PATH + "/esm_software/esm_runscripts/esm_runscripts.yaml"
    need_to_parse_recipe = True
    plugins_bare = FUNCTION_PATH + "/esm_software/esm_runscripts/esm_plugins.yaml"
    need_to_parse_plugins = True

    framework_recipe = esm_plugin_manager.read_recipe(
        recipe, {"job_type": job_type}, need_to_parse_recipe
    )
    if recipe_steps:
        framework_recipe["recipe"] = recipe_steps
    framework_plugins = esm_plugin_manager.read_plugin_information(
        plugins_bare, framework_recipe, need_to_parse_plugins
    )
    esm_plugin_manager.check_plugin_availability(framework_plugins)

    config = esm_plugin_manager.work_through_recipe(
        framework_recipe, framework_plugins, config
    )
    return config


########################################################################################
#                                  general stuff                                       #
########################################################################################
def end_it_all(config):
    if config["general"]["profile"]:
        for line in timing_info:
            print(line)
    if config["general"]["verbose"]:
        print("Exiting entire Python process!")
    sys.exit()


def write_to_log(config, message, message_sep=None):
    """
    Puts a message into the experiment log file

    Parameters
    ----------
    message : list
        A list of the message elements; which is joined by either (highest
        to lowest): 1) the message_sep argument passed to the method, 2)
        The user's chosen separator, as written in
        ``config["general"]["experiment_log_file_message_sep"]``, 3)
        An empty space ``" "``.
    message_sep : None
        The hard-coded message separator to use; which ignores user choices.

    Note
    ----
    The user can control two things regarding the logfile format:

    1) The datestamp formatting, which is taken from the config
       section ``general.experiment_log_file_dateformat``.
    2) The message separators; taken from
       ``general.experiment_log_file_message_sep``. Note that if the
       programmer passes a ``message_sep`` argument; this one wins over
       the user choice.
    """
    try:
        with open(config["general"]["experiment_log_file"], "a+") as logfile:
            line = assemble_log_message(config, message, message_sep)
            logfile.write(line + "\n")
    except KeyError:
        import esm_parser

        print("Sorry; couldn't find 'experiment_log_file' in config['general']...")
        esm_parser.pprint_config(config["general"])
        raise


def assemble_log_message(
    config, message, message_sep=None, timestampStr_from_Unix=False
):
    """Assembles message for log file. See doc for write_to_log"""
    message = [str(i) for i in message]
    dateTimeObj = datetime.now()
    strftime_str = config["general"].get("experiment_log_file_dateformat", "%c")
    if message_sep is None:
        message_sep = config["general"].get("experiment_log_file_message_sep", " ")
    if timestampStr_from_Unix:
        if strftime_str == "%c":
            # date doesn't do %c as it is supposed to
            strftime_str = '"%a %b  %e %T %Y"'
        timestampStr = "$(date +" + strftime_str + ")"
    else:
        timestampStr = dateTimeObj.strftime(strftime_str)
    # TODO: Do we want to be able to specify a timestamp separator as well?
    line = timestampStr + " : " + message_sep.join(message)
    return line


def update_reusable_filetypes(config, reusable_filetypes=None):
    """
    Removes some filetypes from the "resuable files" list. Filetypes to be
    removed are either specified in the config, or passed as an argument.

    Reusable filetypes, declared in general
    (``config["general"]["reusable_filetypes"]``), or in model specific
    (``config[<model>]["reusable_filetypes"]``) forces files of a given type,
    such as ``bin``, ``input``, ``forcing``, etc., to be copied only once into
    the general experiment folder, and then **reuse** them through out every
    run. If a given file type is not present in this list, that file type will
    be copied from its external source at the beginning of every run (i.e. for
    binaries from the folder where the model is compiled).

    This method takes care of removing the reusable file types from
    ``reusable_filetypes``, specified by the user in the ``esm_runscripts``
    call with the ``--update-filetypes`` flag.

    Parameters
    ----------
    config : dict
        Dictionary containing the information about the experiment.
    reusable_filetypes : list
        List of reusable file types that can be specified by the user.

    Returns
    -------
    dict or list
        If the user gives no ``reusable_filetypes`` as input. A dict
        ``config`` is returned, which contains the changes requested
        by the user with the ``--update-flag``.

        If the user gives ``reusable_filetypes`` as input, a list is
        returned. This list ``reusable_filetypes`` contains the changes
        requested by the user with the ``--update-flag``.
    """
    # Set the logic for what to return
    if reusable_filetypes:
        return_config = False
    else:
        return_config = True
        reusable_filetypes = config["general"].get("reusable_filetypes", [])

    update_filetypes = (
        config["general"].get("command_line_config", {}).get("update_filetypes", [])
    )
    if config["general"].get("verbose", False) and update_filetypes:
        print("User requests that the following filetypes be updated:")
        [print(f"* {filetype}") for filetype in update_filetypes]

    # NOTE(MAM, PG): Originally defined in prepare.py
    # https://tinyurl.com/2p8awzsu
    potentially_reusable_filetypes = config["general"]["potentially_reusable_filetypes"]

    # Loop through the file types specified by the user with the ``--update-filetypes``
    # flag
    for update_filetype in update_filetypes:
        # Check if that file type exists/makes sense. Otherwise, through an error
        if update_filetype not in potentially_reusable_filetypes:
            esm_parser.user_error(
                "update-filetypes",
                f"``{update_filetype}`` specified by you in ``--update-filetypes`` is "
                + "not a ESM-Tools file type. Please, select one (or more) of the "
                + "following file types:\n\t- "
                + "\n\t- ".join(potentially_reusable_filetypes),
            )
        # Actually remove the file types specified by the user from
        # ``reusable_filetypes``
        if update_filetype in reusable_filetypes:
            # Remove duplicates just in case
            reusable_filetypes = list(set(reusable_filetypes))
            # Do the removal
            if config["general"].get("verbose", False):
                print(f"Removing {update_filetype}")
            reusable_filetypes.remove(update_filetype)
        elif config["general"].get("verbose", False):
            print(
                f"- The file type ``{update_filetype}`` you are trying to update was"
                "not reusable across runs in the first place, so it's been always "
                "updated with the external source, and it will still be."
            )

    if config["general"].get("verbose", False):
        print("The following filetypes will be re-used from already copied sources:")
        for reusable_filetype in reusable_filetypes:
            print(f"* {reusable_filetype}")
    config["general"]["reusable_filetypes"] = reusable_filetypes

    if return_config:
        return config
    return reusable_filetypes


##############################
# SINK CLASS FOR LOGURU.LOGGER
##############################


class SmartSink:
    """
    A class for smart sinks that allow for logging (using ``logger`` from loguru), even
    if the file path of the log file is not yet defined. The actual sink is not the
    instanced object itself, but the method ``sink`` of the instance. The log record is
    saved in ``self.log_record`` and the log file is written using the path specified
    in ``self.path``. If the path is not specified, the log is stored only in the
    ``self.log_record``. When the path is finally specified, ``self.log_record`` is
    dumped into the log file and from that moment, any time ``logger`` logs something it
    will also be written into the file. To specify the path the method ``def_path``
    needs to be used.
    """

    def __init__(self):
        # Initialise instance variables
        self.log_record = []
        self.path = None

    def sink(self, message):
        """
        The actual sink for loguru's ``logger``. Once you define a logger level a sink
        needs to be provided. Standard sinks include file paths, methods, etc.
        Providing this method as a sink (``logger.add(<name_of_the_instance>.sink,
        level="<your_level>", ...)``) enables the functionality of the SmartSink object.

        Parameters
        ----------
        message : str
            String containing the logging message.
        """
        if self.path:
            self.write_log(message, "a")
        self.log_record.append(message)

    def write_log(self, message, wmode):
        """
        Method to write the logs into the disk.

        Parameters
        ----------
        message : str, list
            String containing the logging message or list containing more than one
            logging message, to be written in the file.
        wmode : str
            Writing mode to choose among ``"w"`` or ``"a"``.
        """
        if isinstance(message, str):
            message = [message]
        with open(self.path, wmode) as log:
            for line in message:
                log.write(line)

    def def_path(self, path):
        """
        Method to define the path of the file. Once the path is defined, the log record
        is written into the file.

        Parameters
        ----------
        path : str
            Path of the logging file.
        """
        self.path = path
        self.write_log(self.log_record, "a")


################################################################################
# Git Checks of esm-tools
#
# NOTE(PG): These functions would likely be better as a separate file


def is_git_repo(path):
    """
    Determines whether or not a directory is a git repository.

    Parameters
    ----------
    path : str or pathlib.Path
        The location to check

    Returns
    bool :
        True or False, depending on if the location is a git directory
    """
    try:
        git.Repo(path).git_dir
        return True
    except git.exc.InvalidGitRepositoryError:
        return False
    except git.exc.NoSuchPathError:
        logger.error("You specified a non-existant directory")
        # Is False the right answer here? I am not 100% sure...
        return False


class GitDirtyError(git.exc.GitError):
    """Thrown if the git repository is dirty"""


def get_git_hash(path, allow_dirty=True):
    """
    Gets the commit has of a git directory stored at ``path``

    Parameters
    ----------
    path : str or pathlib.Path
        Path to determine the git hash for
    allow_dirty : bool
        Complain or not complain about a dirty git repo.

    Returns
    -------
    githash : str
        The commit has the repository is currently on

    Raises
    ------
    GitDirtyError :
       Raised if the repo is "dirty"; thus meaning that the hash is not meaningful
    """
    repo = git.Repo(path)
    if not allow_dirty and repo.is_dirty():
        raise GitDirtyError(
            f"Your repo at {path} is dirty, thus the git hash is not meaningful!"
        )
    return repo.git.rev_parse(repo.head, short=True)


def get_git_branch(path):
    """Gets the name of the current git branch for repo at path

    Parameters
    ----------
    path : str
        The path of the repository to examine

    Returns
    -------
    str :
        The branch name
    """
    repo = git.Repo(path)
    if not repo.head.is_detached:
        return repo.head.reference.name
    commit = repo.head.commit
    for tag in repo.tags:
        if commit.hexsha == tag.commit.hexsha:
            return f"DETACHED HEAD at tag {tag.name}"
    else:
        return f"DETACHED HEAD at commit {repo.head.commit.hexsha}"


def get_git_diffs(path, add_colors={"+": colorama.Fore.GREEN, "-": colorama.Fore.RED}):
    """
    Gets the differences

    Parameters
    ----------
    path : str
        The git repository to check
    add_colors : dict
        A dictionary of ``colorama.Fore`` properties for added lines, stored
        under the key ``'+'`` and removed lines, stored under the key ``'-'``.

    Returns
    -------
    str :
        The (maybe) colorized difference as a multi-line string.
    """
    repo = git.Repo(path)
    diffs = repo.git.diff(repo.commit()).split("\n")
    if add_colors:
        for index, diff in enumerate(diffs):
            if diff.startswith("+"):
                diff = f'{add_colors["+"]}{diff}{colorama.Style.RESET_ALL}'
            elif diff.startswith("-"):
                diff = f'{add_colors["-"]}{diff}{colorama.Style.RESET_ALL}'
            diffs[index] = diff
    # In case no diffs are detected, we still want to return an empty
    # list.
    if not diffs:
        diffs = "\n".join([])
    return "\n".join(diffs)


def get_all_git_info(path):
    """Gets all information needed for the vcs dump file

    Returns
    -------
    dict :
        A dictionary with the following information: git hash, branch name, differences, .....
    """
    git_info = {
        "path": path,
        "hash": get_git_hash(path),
        "branch_name": get_git_branch(path),
        # NOTE(PG): Dumping to YAML is not friendly with colors, it seems...
        "diffs": get_git_diffs(path, add_colors=False),
    }
    return git_info
