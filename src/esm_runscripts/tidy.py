import filecmp
import os
import sys
import re
import time
import pathlib

import psutil
import shutil

from . import coupler, database_actions, helpers
from .filelists import copy_files, resolve_symlinks


def run_job(config):
    config["general"]["relevant_filetypes"] = [
        "log",
        "mon",
        "outdata",
        "restart_out",
        #"bin",
        #"config",
        #"forcing",
        #"input",
        #"restart_in",
        "ignore",
        "unknown",
    ]
    helpers.evaluate(config, "tidy", "tidy_recipe")
    return config


def init_monitor_file(config):
    called_from = config["general"]["last_jobtype"]
    monitor_file = config["general"]["monitor_file"]

    monitor_file.write("tidy job initialized \n")
    monitor_file.write(
        "attaching to process " + str(config["general"]["launcher_pid"]) + " \n"
    )
    monitor_file.write("Called from a " + called_from + "job \n")
    return config


def get_last_jobid(config):
    called_from = config["general"]["last_jobtype"]
    last_jobid = "UNKNOWN"
    if called_from == "compute":
        with open(config["general"]["experiment_log_file"], "r") as logfile:
            lastline = [
                l for l in logfile.readlines() if "compute" in l and "start" in l
            ][-1]
            last_jobid = lastline.split(" - ")[0].split()[-1]
    config["general"]["last_jobid"] = last_jobid
    return config


def copy_stuff_back_from_work(config):
    config = copy_files(
        config, config["general"]["relevant_filetypes"], "work", "thisrun"
    )
    return config


def wait_and_observe(config):
    if config["general"]["submitted"]:
        monitor_file = config["general"]["monitor_file"]
        thistime = 0
        error_check_list = assemble_error_list(config)
        while job_is_still_running(config):
            monitor_file.write("still running \n")
            config["general"]["next_test_time"] = thistime
            config = check_for_errors(config)
            thistime = thistime + 10
            time.sleep(10)
        thistime = thistime + 100000000
        config["general"]["next_test_time"] = thistime
        config = check_for_errors(config)
    return config


def tidy_coupler(config):
    if config["general"]["standalone"] == False:
        config["general"]["coupler"].tidy(config)
    return config


def wake_up_call(config):
    called_from = config["general"]["last_jobtype"]
    monitor_file = config["general"]["monitor_file"]
    last_jobid = config["general"]["last_jobid"]
    monitor_file.write("job ended, starting to tidy up now \n")
    # Log job completion
    if called_from != "command_line":
        helpers.write_to_log(
            config,
            [
                called_from,
                str(config["general"]["run_number"]),
                str(config["general"]["current_date"]),
                last_jobid,
                "- done",
            ],
        )
    # Tell the world you're cleaning up:
    helpers.write_to_log(
        config,
        [
            str(config["general"]["jobtype"]),
            str(config["general"]["run_number"]),
            str(config["general"]["current_date"]),
            str(config["general"]["jobid"]),
            "- start",
        ],
    )
    return config


def assemble_error_list(config):
    gconfig = config["general"]
    known_methods = ["warn", "kill"]
    # experiment outputs are written to this log file
    stdout = \
        f"{gconfig['experiment_scripts_dir']}/{gconfig['expid']}"\
        f"_compute_{gconfig['run_datestamp']}_{gconfig['jobid']}.log"

    error_list = [
        ("error", stdout, "warn", 60, 60, "keyword error detected, watch out")
    ]

    for model in config:
        if "check_error" in config[model]:
            for trigger in config[model]["check_error"]:
                search_file = stdout
                method = "warn"
                frequency = 60
                message = "keyword " + trigger + " detected, watch out"
                if isinstance(config[model]["check_error"][trigger], dict):
                    if "file" in config[model]["check_error"][trigger]:
                        search_file = config[model]["check_error"][trigger]["file"]
                        if search_file == "stdout" or search_file == "stderr":
                            search_file = stdout
                    if "method" in config[model]["check_error"][trigger]:
                        method = config[model]["check_error"][trigger]["method"]
                        if method not in known_methods:
                            method = "warn"
                    if "message" in config[model]["check_error"][trigger]:
                        message = config[model]["check_error"][trigger]["message"]
                    if "frequency" in config[model]["check_error"][trigger]:
                        frequency = config[model]["check_error"][trigger]["frequency"]
                        try:
                            frequency = int(frequency)
                        except:
                            frequency = 60
                elif isinstance(config[model]["check_error"][trigger], str):
                    pass
                else:
                    continue
                error_list.append(
                    (trigger, search_file, method, frequency, frequency, message)
                )
    config["general"]["error_list"] = error_list
    return config


def check_for_errors(config):
    new_list = []
    error_check_list = config["general"]["error_list"]
    monitor_file = config["general"]["monitor_file"]
    time = config["general"]["next_test_time"]
    for (
        trigger,
        search_file,
        method,
        next_check,
        frequency,
        message,
    ) in error_check_list:
        warned = 0
        if next_check <= time:
            if os.path.isfile(search_file):
                with open(search_file) as origin_file:
                    for line in origin_file:
                        if trigger.upper() in line.upper():
                            if method == "warn":
                                warned = 1
                                monitor_file.write("WARNING: " + message + "\n")
                                break
                            elif method == "kill":
                                harakiri = "scancel " + config["general"]["jobid"]
                                monitor_file.write("ERROR: " + message + "\n")
                                monitor_file.write("Will kill the run now..." + "\n")
                                monitor_file.flush()
                                print("ERROR: " + message)
                                print("Will kill the run now...", flush=True)
                                database_actions.database_entry_crashed(config)
                                os.system(harakiri)
                                sys.exit(42)
            next_check += frequency
        if warned == 0:
            new_list.append(
                (trigger, search_file, method, next_check, frequency, message)
            )
    config["general"]["error_list"] = new_list
    return config


def job_is_still_running(config):
    if psutil.pid_exists(config["general"]["launcher_pid"]):
        return True
    return False


def _increment_date_and_run_number(config):
    config["general"]["run_number"] += 1
    config["general"]["current_date"] += config["general"]["delta_date"]
    return config


def _write_date_file(config):  # self, date_file=None):
    monitor_file = config["general"]["monitor_file"]
    # if not date_file:
    date_file = \
        f"{config['general']['experiment_scripts_dir']}"\
        f"/{config['general']['expid']}_{config['general']['setup_name']}.date"
    
    with open(date_file, "w") as date_file:
        date_file.write(
            config["general"]["current_date"].output()
            + " "
            + str(config["general"]["run_number"])
        )
    monitor_file.write("writing date file \n")
    return config


def clean_run_dir(config):
    """
     This plugin allows you to clean up the ``run_${DATE}`` folders.
     To do that you can use the following variables under the
     ``general`` section of your runscript (documentation follows order
     of code as it is executed):

     * ``clean_runs``: **This is the most important variable for most
       users**. It can take the following values:
         * ``True``: removes the ``run_`` directory after each run
           (**overrides every other** ``clean_`` **option**).

         * ``False``: does not remove any ``run_`` directory (default)
           if no ``clean_`` variable is defined.

         * ``<int>``: giving an integer as a value results in deleting
           the ``run_`` folders except for the last <int> runs
           (recommended option as it allows for debugging of crashed
           simulations).

       .. Note::
          ``clean_runs: (bool)`` is incompatible with
          ``clean_this_rundir`` and ``clean_runs: (int)`` is incompatible
          with ``clean_old_rundirs_except`` (an error will be raised
          after the end of the first simulation). The functionality of
          ``clean_runs`` variable **alone will suffice most of the
          standard user requirements**. If finer tunning for the removal
          of ``run_`` directories is required you can used the following
          variables instead of ``clean_runs``.

     * ``clean_this_rundir``: (bool) Removes the entire run directory
       (equivalent to ``clean_runs: (bool)``). ``clean_this_rundir: True``
       **overrides every other** ``clean_`` **option**.

     * ``clean_old_rundirs_except``: (int) Removes the entire run
       directory except for the last <x> runs (equivalent to
       ``clean_runs: (int)``).

     * ``clean_old_rundirs_keep_every``: (int) Removes the entire
       run directory except every <x>th run. Compatible with
       ``clean_old_rundirs_except`` or ``clean_runs: (int)``.

     * ``clean_<filetype>_dir``: (bool) Erases the run directory
       for a specific filetype. Compatible with all the other options.

     * ``clean_size``: (int or float) Erases all files with size
       greater than ``clean_size``, must be specified in bytes! Compatible
       with all the other options.

     Example
     -------

     To delete all the ``run_`` directories in your experiment include this
     into your runscript:

     .. code-block:: yaml

        general:
                clean_runs: True

     To keep the last 2 ``run_`` directories:

     .. code-block:: yaml

        general:
                clean_runs: 2

     To keep the last 2 runs and every 5 runs:

     .. code-block:: yaml

        general:
                clean_old_rundirs_except: 2
                clean_old_rundirs_keep_every: 5
     """
    _clean_run_determine_user_choice(config)
    _clean_this_rundir(config)
    _clean_old_rundirs_except(config)
    _clean_old_runs_filetypes(config)
    _clean_old_runs_size(config)
    return config


def _clean_run_determine_user_choice(config):
    """
    Determine user choice from a simple switch.
    The user sets::
    general:
        clean_runs: <x>
    where ``x`` can be one of:
    * ``True`` Removes the current run dir
    * ``False`` Keeps run dir
    * ``int`` (must be >= 0) keep last ``x`` run dirs
    """
    user_clean = config["general"].get("clean_runs")
    # TODO(PG): It might be nice if these sorts of checks happened earlier
    # in the job, before it even gets to this function
    if user_clean is None:
        return  # Skip the rest of the function
    if isinstance(user_clean, bool):
        if "clean_this_rundir" not in config["general"]:
            config["general"]["clean_this_rundir"] = user_clean
        else:
            print("------------------------------------------")
            print("You have set both in your config:")
            print()
            print("general:")
            print("    clean_this_rundir: ", config["general"]["clean_this_rundir"])
            print("    clean_runs: ", user_clean)
            print()
            print("Please only use one of these!")
            print("------------------------------------------")
            sys.exit(1)
    elif isinstance(user_clean, int):
        if "clean_old_rundirs_except" not in config["general"]:
            config["general"]["clean_old_rundirs_except"] = user_clean
        else:
            print("------------------------------------------")
            print("You have set both in your config:")
            print()
            print("general:")
            print(
                "    clean_old_rundirs_except: ",
                config["general"]["clean_old_rundirs_except"],
            )
            print("    clean_runs: ", user_clean)
            print()
            print("Please only use one of these!")
            print("------------------------------------------")
            sys.exit(1)
    else:
        print("------------------------------------------")
        print("Type Error!")
        print("You have set this in your config:")
        print("general:")
        print("    clean_runs: ", user_clean)
        print()
        print("This is of type: ", type(user_clean))
        print("However, only the following types are valid:")
        print("   * boolean")
        print("   * integer (greater or equal to 0!)")
        print("Please correct that")
        print("------------------------------------------")
        sys.exit(1)


def _clean_this_rundir(config):
    if config["general"].get("clean_this_rundir", False):
        rm_r(config["general"]["thisrun_dir"])


def _clean_old_rundirs_except(config):
    all_run_folders_in_experiment = RunFolders(config)

    number_rundirs_keep_every = config["general"].get("clean_old_rundirs_keep_every")
    runs_to_keep_via_keepevery = []
    if number_rundirs_keep_every:
        try:
            assert isinstance(number_rundirs_keep_every, int)
            assert number_rundirs_keep_every >= 1
        except AssertionError:
            print("Please ensure that you use an integer in your configuration:")
            print("-------------------------------------------------------------")
            print()
            print("general:")
            print("   clean_old_rundirs_keep_every: <x>")
            print()
            print("-------------------------------------------------------------")
            print("<x> **MUST** be an integer greater or equal than 1!")
            sys.exit(1)
        runs_to_keep_via_keepevery = all_run_folders_in_experiment[
            ::number_rundirs_keep_every
        ]

    number_rundirs_to_keep = config["general"].get("clean_old_rundirs_except")
    runs_to_keep_via_end_select = []
    if number_rundirs_to_keep:
        try:
            assert isinstance(number_rundirs_to_keep, int)
            assert number_rundirs_to_keep > 1
        except AssertionError:
            print("Please ensure that you use an integer in your configuration:")
            print("-------------------------------------------------------------")
            print()
            print("general:")
            print("   clean_old_rundirs_except: <x>")
            print()
            print("-------------------------------------------------------------")
            print("<x> **MUST** be an integer greater than 1!")
            sys.exit(1)
        runs_to_keep_via_end_select = all_run_folders_in_experiment[
            -number_rundirs_to_keep:
        ]
    if number_rundirs_keep_every or number_rundirs_to_keep:
        runs_to_keep = set(runs_to_keep_via_keepevery + runs_to_keep_via_end_select)
    else:
        runs_to_keep = set(all_run_folders_in_experiment)
    runs_to_clean = set(all_run_folders_in_experiment) - runs_to_keep
    for run in list(runs_to_clean):
        rm_r(run)


def _clean_old_runs_filetypes(config):
    all_filetypes = config["general"]["all_filetypes"]
    for filetype in all_filetypes:
        if config["general"].get("clean_" + filetype + "_dir", False):
            rm_r(config["general"]["thisrun_" + filetype + "_dir"])


def _clean_old_runs_size(config):
    rmsize = config["general"].get("clean_size", False)
    if rmsize:
        flist = []
        for root, _, files in os.walk(config["general"]["thisrun_dir"]):
            for file_ in files:
                size = os.path.getsize(root + "/" + file_)
                if size >= rmsize:
                    flist.append(root + "/" + file_)
        for file_ in flist:
            os.remove(file_)


def start_various_jobtypes_after_compute(config):
    monitor_file = config["general"]["monitor_file"]
    # Jobs that should be started directly from the compute job:
    next_jobs = ["post"]  # Later also: "viz", "couple", ("analysis"...?)
    for jobtype in next_jobs:
        do_jobtype = False
        for model in config:
            # Allows for both "do_post: True" or "post: True" in config:
            if (
                config[model].get(f"do_{jobtype}", False) or
                config[model].get(jobtype, False)
            ):
                do_jobtype = True
        if do_jobtype:
            monitor_file.write(f"{jobtype} for this run:\n")
            command_line_config = config["general"]["command_line_config"]
            command_line_config["jobtype"] = jobtype
            command_line_config["original_command"] = command_line_config[
                "original_command"
            ].replace("compute", jobtype)
            monitor_file.write(f"Initializing {jobtype} object with:\n")
            monitor_file.write(str(command_line_config))
            # NOTE(PG) Non top level import to avoid circular dependency:
            from .sim_objects import SimulationSetup
            jobtype_obj = SimulationSetup(command_line_config)
            monitor_file.write("f{jobtype} object built....\n")
            if f"{jobtype}_update_compute_config_before_resubmit" in jobtype_obj.config:
                monitor_file.write(f"{jobtype} object needs to update the calling job config:\n")
                # FIXME(PG): This might need to be a deep update...?
                config.update(jobtype.config[f"{jobtype}_update_compute_config_before_resubmit"])
            monitor_file.write(f"Calling {jobtype} job:\n")
            jobtype_obj()
    return config


def start_post_job(config):
    monitor_file = config["general"]["monitor_file"]
    do_post = False
    for model in config:
        if "post_processing" in config[model]:
            if config[model]["post_processing"]:
                do_post = True

    if do_post:
        monitor_file.write("Post processing for this run:\n")
        command_line_config = config["general"]["command_line_config"]
        command_line_config["jobtype"] = "post"
        command_line_config["original_command"] = command_line_config[
            "original_command"
        ].replace("compute", "post")
        monitor_file.write("Initializing post object with:\n")
        monitor_file.write(str(command_line_config))
        # NOTE(PG) Non top level import to avoid circular dependency:
        from .sim_objects import SimulationSetup
        this_post = SimulationSetup(command_line_config)
        monitor_file.write("Post object built; calling post job:\n")
        this_post()
    return config


def all_done(config):
    helpers.write_to_log(
        config,
        [
            str(config["general"]["jobtype"]),
            str(config["general"]["run_number"]),
            str(config["general"]["current_date"]),
            str(config["general"]["jobid"]),
            "- done",
        ],
    )

    database_actions.database_entry_success(config)
    return config

def signal_tidy_completion(config):
    helpers.write_to_log(
        config,
        [
            str(config["general"]["jobtype"]),
            str(config["general"]["run_number"]),
            str(config["general"]["current_date"]),
            str(config["general"]["jobid"]),
            "- done",
        ],
    )
    return config


def maybe_resubmit(config):
    monitor_file = config["general"]["monitor_file"]
    monitor_file.write("resubmitting \n")
    command_line_config = config["general"]["command_line_config"]
    command_line_config["jobtype"] = "compute"
    command_line_config["original_command"] = command_line_config[
        "original_command"
    ].replace("tidy_and_resubmit", "compute")

    # seb-wahl: end_date is by definition (search for 'end_date') smaller than final_date
    # hence we have to use next_date = current_date + increment
    if config["general"]["next_date"] >= config["general"]["final_date"]:
        monitor_file.write("Reached the end of the simulation, quitting...\n")
        helpers.write_to_log(config, ["# Experiment over"], message_sep="")
    else:
        monitor_file.write("Init for next run:\n")
        # NOTE(PG) Non top level import to avoid circular dependency:
        from .sim_objects import SimulationSetup
        next_compute = SimulationSetup(command_line_config)
        next_compute(kill_after_submit=False)
    return config


# DONT LIKE THE FOLLOWING PART...
# I wish it was closer to the copy_files routine in filelists,
# but as it is really a different thing - moving everything
# found compared to copying everything in filelists - a second
# implementation might be OK... (DB)



def throw_away_some_infiles(config):
    if config["general"]["run_number"] == 1:
        return config
    monitor_file = config["general"]["monitor_file"]
    monitor_file.write("throwing away restart_in files \n")
    for model in config["general"]["valid_model_names"]:
        print(f"{model}")
        if "thisrun_restart_in_dir" in config[model]:
            if os.path.isdir(config[model]["thisrun_restart_in_dir"]):
                for root, dirs, files in os.walk(config[model]["thisrun_restart_in_dir"]):
                    for name in files:
                        source = os.path.join(root, name)
                        os.remove(source)
                        print(f"Removing {source}")
    return config



def copy_all_results_to_exp(config):
    monitor_file = config["general"]["monitor_file"]
    monitor_file.write("Copying stuff to main experiment folder \n")
    for root, dirs, files in os.walk(config["general"]["thisrun_dir"], topdown=False):
        if config["general"]["verbose"]:
            print("Working on folder: " + root)
        if root.startswith(config["general"]["thisrun_work_dir"]) or root.endswith(
            "/work"
        ):
            if config["general"]["verbose"]:
                print("Skipping files in work.")
            continue

        for name in files:
            source = os.path.join(root, name)
           
            if not os.stat(source).st_size > 0: # skip empty files
                continue

            if config["general"]["verbose"]:
                print("File: " + source)
            destination = source.replace(
                config["general"]["thisrun_dir"], config["general"]["experiment_dir"]
            )
            destination_path = destination.rsplit("/", 1)[0]
            if not os.path.exists(destination_path):
                os.makedirs(destination_path)
            if not os.path.islink(source):
                if os.path.isfile(destination):
                    if filecmp.cmp(source, destination):
                        if config["general"]["verbose"]:
                            print("File " + source + " has not changed, skipping.")
                        continue
                    else:
                        if os.path.isfile(
                            destination + "_" + config["general"]["run_datestamp"]
                        ):
                            print(
                                "Don't know where to move "
                                + destination
                                + ", file exists"
                            )
                            continue
                        else:
                            if os.path.islink(destination):
                                os.remove(destination)
                            else:
                                os.rename(
                                    destination,
                                    destination
                                    + "_"
                                    + config["general"]["last_run_datestamp"],
                                )
                            newdestination = (
                                destination + "_" + config["general"]["run_datestamp"]
                            )
                            if config["general"]["verbose"]:
                                print("Moving file " + source + " to " + newdestination)
                            os.rename(source, newdestination)
                            os.symlink(newdestination, destination)
                            continue
                try:
                    if config["general"]["verbose"]:
                        print("Moving file " + source + " to " + destination)
                    try:
                        os.rename(source, destination)
                    except:  # Fill is still open... create a hard (!) link instead
                        os.link(source, destination)

                except:
                    print(
                        ">>>>>>>>>  Something went wrong moving "
                        + source
                        + " to "
                        + destination
                    )
            else:
                linkdest = resolve_symlinks(source,config["general"]["verbose"])
                #newlinkdest = (
                #    destination.rsplit("/", 1)[0] + "/" + linkdest.rsplit("/", 1)[-1]
                #)
                if os.path.islink(destination):
                    destdest = resolve_symlinks(source,config["general"]["verbose"])
                    if linkdest == destdest:
                        # both links are identical, skip
                        continue
                    #os.remove(destination)
                if os.path.isfile(destination):
                    os.rename(
                        destination,
                        destination + "_" + config["general"]["last_run_datestamp"],
                    )
                os.symlink(linkdest, destination)
    return config


# Utility functions:
def rm_r(path):
    """
    Python equivalent of rm -r

    Parameters
    ----------
    path : str
        Path or directory to remove
    """
    if not os.path.exists(path):
        return
    if os.path.isfile(path) or os.path.islink(path):
        os.unlink(path)
    else:
        shutil.rmtree(path)


def size_bytes_to_human(num, suffix="B"):
    for unit in ["", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi"]:
        if abs(num) < 1024.0:
            return "%3.1f%s%s" % (num, unit, suffix)
        num /= 1024.0
    return "%.1f%s%s" % (num, "Yi", suffix)


# PG: BROKEN!!!
def size_human_to_bytes(s, suffix="B"):
    for unit in ["", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi"]:
        num = float(s.replace(unit, ""))
        if abs(num) < 1024.0:
            return num
        num *= 1024.0
    return num


class RunFolders(list):
    """
    Logs the ``run_`` directories in ``<experiment_id>/log/run_folders.log``,
    updating it with new folders. The resulting object is a list of ``run_`` paths
    that exist or existed during the run time (even if they got deleted). This
    is useful for indexing operations such as ``<object_name>[::<interval>]``
    used when removing ``run_`` folders.
    Notes
    -----
    It keeps the folder names sorted so there is no need of sorting out of the
    object, and it also prevents the existence of duplicates.
    """

    def __init__(self, config):
        """
        The initialization of the object:
        * Loads the existing paths of the ``run_`` folders
        * Loads previous ``run_`` folder names from the logging file
        * Adds the current folder names to the logging file
        * Returns a list of ``pathlib.Path`` folder paths
        """

        # Load paths from ``config``
        self.exp_dir = config["general"]["experiment_dir"]
        self.log_path = self.exp_dir + "/log/run_folders.log"

        # Load existing folders
        self.current_folders = [
            folder for folder in os.listdir(self.exp_dir) if folder.startswith("run_")
        ]
        self.current_folders = [
            self.exp_dir + "/" + folder for folder in self.current_folders
        ]

        # Check if the ``run_folders.log`` file exists, and if not, create it
        if not os.path.exists(self.log_path):
            with open(self.log_path, "w") as log_file:
                pass

        # Load previous run names from ``run_folders.log``
        self.folders = []
        self.load()

        # Add current folders
        self.update()

        # Add folders to the list
        for folder in self.folders:
            super().append(pathlib.Path(folder))

    def load(self):
        """
        Loads the existing paths of the ``run_`` folders.
        """
        with open(self.log_path, "r") as log_file:
            for folder in log_file.readlines():
                self.folders.append(folder.strip())

    def save(self):
        """
        Saves all folder names.
        """
        with open(self.log_path, "w") as log_file:
            log_file.writelines([folder + "\n" for folder in self.folders])

    def update(self):
        """
        Updates the folders read from the log file with the currently existing
        folders, removes duplicates, sorts them and save them into the log file.
        """
        # Update with ``self.curren_folders``
        for folder in self.current_folders:
            self.folders.append(folder)
        # Remove duplicates
        self.folders = list(dict.fromkeys(self.folders))
        # Sort folders
        self.folders.sort()
        # Save to the log file
        self.save()
