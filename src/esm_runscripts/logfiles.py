import os
import sys

from loguru import logger

import _io

from . import helpers


###############################
# WORKFLOW STATUS LOG FUNCTIONS
###############################

def progress(self, message, *args, **kwargs):
    """
    Log a message at the PROGRESS level for loguru.
    """
    return self.log("PROGRESS", message, *args, **kwargs)

def set_progress_level(config):
    """
    Set the PROGRESS level for loguru logger.
    """
    if not hasattr(logger, "progress"):
        PROGRESS_LEVEL = 15
        logger.level("PROGRESS", no=PROGRESS_LEVEL)
        logger.__class__.progress = progress

    experiment_log_file = config["general"]["experiment_log_file"]
    logger.add(
        experiment_log_file,
        format="{time: YYYY-MM-DD HH:mm:ss.SSS} | {level} | {message}",
        filter=lambda record: record["level"].name == "PROGRESS"
    )

def initialize_logging(command_line_config):
    jobtype = command_line_config["jobtype"]
    verbose = command_line_config["verbose"]
    debug = command_line_config["debug"]
    trace = command_line_config["trace"]
    task_log_files = command_line_config["task_log_files"]

    logger.remove()
    #logger.add(task_sink.sink, level="TRACE")

    # Redirect stdout to a SmartSink if the jobtype is observe_compute to avoid printing
    # in the slurm log at the same time that compute is printing.
    if command_line_config["jobtype"] in ["observe_compute"]:
        stdout_sink = SmartSink(print_in_stdout=False, task_log_files=task_log_files)
    else:
        stdout_sink = SmartSink(print_in_stdout=True, task_log_files=task_log_files)

    # Store the sink in the logger
    logger.stdout_sink = stdout_sink
    stdout = stdout_sink.sink

    # Set logging level based on the command line arguments
    if trace:
        logger.add(stdout, level="TRACE")
        logger.trace(f"Started from: {command_line_config['started_from']}")
        logger.trace(f"starting (jobtype): {jobtype}")
        logger.trace(command_line_config)
    elif verbose or debug:
        logger.add(stdout, level="DEBUG", format="{message}")
        logger.debug(f"Started from: {command_line_config['started_from']}")
        logger.debug(f"starting (jobtype): {jobtype}")
        logger.debug(command_line_config)
    else:
        logger.add(stdout, level="INFO", format="{message}")

def initialize_logfiles(config, org_jobtype):
    """
    Initialize the general log file for the experiment.

    Parameters
    ----------
    config : dict, esm_parser.DictWithProvenance
        Configuration dictionary containing general settings for the experiment.
    org_jobtype: str
        
    """
    # Return if the experiment log dir does not exist or if the job type is "inspect"
    experiment_log_file = config["general"]["experiment_log_file"]
    experiment_log_dir = os.path.dirname(experiment_log_file)
    if not os.path.isdir(experiment_log_dir) or org_jobtype == "inspect":
        return config

    raise NotImplementedError("THIS LINE SHOULD NOT BE REACHED ANYMORE")
    config = set_logfile_name(config, "")

    logger.progress(
        config,
        [
            org_jobtype,
            logfile_run_number,
            str(config["general"]["current_date"]),
            str(config["general"]["jobid"]),
            "- start",
        ],
    )

    return config


def finalize_logfiles(config, task):

    run_number = str(config["general"]["run_number"])
    current_date = str(config["general"]["current_date"])
    jobid = str(config["general"]["jobid"])

    logger.progress(f"{task} {run_number} {current_date} {jobid} - done")


def set_logfile_name(config, jobtype=None):

    if not jobtype:
        jobtype = config["general"]["jobtype"]

    expid = config["general"]["expid"]
    setup_name = config["general"]["setup_name"]
    filejobtype = jobtype
    run_datestamp = config["general"]["run_datestamp"]
    experiment_log_dir = config["general"]["experiment_log_dir"]
    thisrun_log_dir = config["general"]["thisrun_log_dir"]

    filename = f"{expid}_{setup_name}_{filejobtype}_{run_datestamp}.log"

    config["general"]["logfile_path"] = f"{experiment_log_dir}/{filename}"
    config["general"]["logfile_path_in_run"] = f"{thisrun_log_dir}/{filename}"

    return config

def get_task_logfile_path(config):
    """
    Get the path to the log file based on the configuration and job type.

    Parameters
    ----------
    config : dict
        Configuration dictionary containing general settings for the experiment.
    jobtype : str, optional
        The type of job for which to get the log file path. If not provided, it uses
        the job type from the configuration.

    Returns
    -------
    str
        The path to the log file.
    """
    jobtype = config["general"]["jobtype"]
    experiment_dir = config["general"]["experiment_dir"]
    expid = config["general"]["expid"]
    setup_name = config["general"]["setup_name"]
    it_coupled_model = config["general"]["iterative_coupled_model"]
    datestamp = config["general"]["run_datestamp"]
    task_logfile_path = (
        f"{experiment_dir}/log/"
        f"{expid}_{setup_name}_{it_coupled_model}{jobtype}_{datestamp}.log"
    )

    return task_logfile_path

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

    def __init__(self, print_in_stdout=True, task_log_files=True):
        # Initialise instance variables
        self.log_record = []
        self.path = None
        self.print_in_stdout = print_in_stdout
        self.task_log_files = task_log_files

    def sink(self, message):
        """
        The actual sink for loguru's ``logger``. Once you define a logger level a sink
        needs to be provided. Standard sinks include file paths, methods, etc.
        Providing this method as a sink (``logger.add(<name_of_the_instance>.sink,
        level="<your_level>", ...)``) enables the functionality of the SmartSink object.
        This sink will store the log messages in the ``self.log_record`` list until
        the path is defined using the ``def_path`` method. Once the path is defined,
        the log messages will be written to the file specified by the path. If
        ``print_in_stdout`` is set to ``True``, the log messages will also be printed to
        the standard output (``sys.stdout``).

        Parameters
        ----------
        message : str
            String containing the logging message.
        """
        # If the path is defined, write the message to the log file
        if self.path and self.task_log_files:
            self.write_log(message, "a")
        # If the path is not defined, but logging must happen in the task log files
        # store the message in the log record buffer
        elif self.task_log_files:
            self.log_record.append(message)
        # If printing in stdout is disabled keep a bufer independent of the task_log_files
        # option
        elif not self.print_in_stdout:
            self.log_record.append(message)

        # Print the message to stdout if the option is enabled
        if self.print_in_stdout:
            print(message, end="")

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
        # Writes into a task log file if that option is enabled or if printing in stdout
        # of that task is disable (store it into a file until is flushed to stdout)
        if self.task_log_files or not self.print_in_stdout:
            self.path = path
            self.write_log(self.log_record, "w")
            self.log_record = []  # Clear the log record after writing it to the file
        else:
            logger.debug(
                "Task log files are disabled. No log file will be written."
            )

    def flush_to_stdout(self):
        """
        Flushes the log record to stdout. This is useful if you want to see the logs
        immediately after they are written, without waiting for the file to be defined.
        """
        if self.log_record:
            for line in self.log_record:
                print(line, end="")
            self.log_record = []
        elif self.path and os.path.exists(self.path):
            with open(self.path, "r") as log:
                for line in log:
                    print(line, end="")
