import sys
from datetime import datetime
import os
import tempfile

import esm_rcfile

import esm_plugin_manager
import esm_tools
import esm_parser


def symlink(target, link_name, overwrite=False):
    '''
    unfortunately Python os.symlink does not have a force option. So, this is a
    good workaround.
    source: https://stackoverflow.com/questions/8299386/modifying-a-symlink-in-python/55742015#55742015
    Create a symbolic link named link_name pointing to target.
    If link_name exists then FileExistsError is raised, unless overwrite=True.
    When trying to overwrite a directory, IsADirectoryError is raised.
    '''

    if not overwrite:
        try:
            os.symlink(target, link_name)
        except FileExistsError:
            error_type = "File Exists"
            error_text = f"{link_name} already exists. Use overwrite=True"
            esm_parser.user_error(error_type, error_text, exit_code=1)
        return

    # os.replace() may fail if files are on different filesystems
    link_dir = os.path.dirname(link_name)

    # Create link to target with temporary filename
    while True:
        temp_link_name = tempfile.mktemp(dir=link_dir)

        # os.* functions mimic as closely as possible system functions
        # The POSIX symlink() returns EEXIST if link_name already exists
        # https://pubs.opengroup.org/onlinepubs/9699919799/functions/symlink.html
        try:
            os.symlink(target, temp_link_name)
            break
        except FileExistsError:
            pass

    # Replace link_name with temp_link_name
    try:
        # Pre-empt os.replace on a directory with a nicer message
        if not os.path.islink(link_name) and os.path.isdir(link_name):
            raise IsADirectoryError(f"Cannot symlink over existing directory: '{link_name}'")
        os.replace(temp_link_name, link_name)
    except Exception as e:
        if os.path.islink(temp_link_name):
            os.remove(temp_link_name)
        error_type = "Error"
        error_text = repr(e)
        esm_parser.user_error(error_type, error_text, exit_code=1)


def vprint(message, config):
    if config["general"]["verbose"]:
        print(message)


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
        print(
            "Your configuration is incorrect, and should include headings for %s as well as general!"
            % setup_name
        )
        sys.exit(1)

    FUNCTION_PATH = esm_rcfile.EsmToolsDir("FUNCTION_PATH")
    recipe = FUNCTION_PATH + "esm_software/esm_runscripts/esm_runscripts.yaml"
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


#########################################################################################
#                                   general stuff                                       #
#########################################################################################
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
        The user's chosen seperator, as written in
        ``config["general"]["experiment_log_file_message_sep"]``, 3)
        An empty space ``" "``.
    message_sep : None
        The hard-coded message seperator to use; which ignores user choices.

    Note
    ----
    The user can control two things regarding the logfile format:

    1) The datestamp formatting, whjich is taken from the config
       section ``general.experiment_log_file_dateformat``.
    2) The message seperators; taken from
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
        timestampStr = "$(date +" + strftime_str + ")"
    else:
        timestampStr = dateTimeObj.strftime(strftime_str)
    # TODO: Do we want to be able to specify a timestamp seperator as well?
    line = timestampStr + " : " + message_sep.join(message)
    return line


############################## SINK CLASS FOR LOGURU.LOGGER ###########################

class SmartSink():
    '''
    A class for smart sinks that allow for logging (using ``logger`` from loguru), even
    if the file path of the log file is not yet defined. The actual sink is not the
    instanced object itself, but the method ``sink`` of the instance. The log record is
    saved in ``self.log_record`` and the log file is written using the path specified
    in ``self.path``. If the path is not specified, the log is stored only in the
    ``self.log_record``. When the path is finally specified, ``self.log_record`` is
    dumped into the log file and from that moment, any time ``logger`` logs something it
    will also be written into the file. To specify the path the method ``def_path``
    needs to be used.
    '''

    def __init__(self):
        # Initialise instance variables
        self.log_record = []
        self.path = None

    def sink(self, message):
        '''
        The actual sink for loguru's ``logger``. Once you define a logger level a sink
        needs to be provided. Standard sinks include file paths, methods, etc.
        Providing this method as a sink (``logger.add(<name_of_the_instance>.sink,
        level="<your_level>", ...)``) enables the functionality of the SmartSink object.

        Parameters
        ----------
        message : str
            String containing the logging message.
        '''
        if self.path:
            self.write_log(message, "a")
        self.log_record.append(message)

    def write_log(self, message, wmode):
        '''
        Method to write the logs into the disk.

        Parameters
        ----------
        message : str, list
            String containing the logging message or list containing more than one
            logging message, to be written in the file.
        wmode : str
            Writing mode to choose among ``"w"`` or ``"a"``.
        '''
        if isinstance(message, str):
            message = [message]
        with open(self.path, wmode) as log:
            for line in message:
                log.write(line)

    def def_path(self, path):
        '''
        Method to define the path of the file. Once the path is defined, the log record
        is written into the file.

        Parameters
        ----------
        path : str
            Path of the logging file.
        '''
        self.path = path
        self.write_log(self.log_record, "w")
