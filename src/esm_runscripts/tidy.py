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
from . import logfiles


def run_job(config):
    config["general"]["relevant_filetypes"] = [
        "log",
        "mon",
        "outdata",
        "restart_out",
        # "bin",
        # "config",
        # "forcing",
        # "input",
        # "restart_in",
        "ignore",
        "unknown",
    ]
    helpers.evaluate(config, "tidy", "tidy_recipe")
    return config


def copy_stuff_back_from_work(config):
    config = copy_files(
        config, config["general"]["relevant_filetypes"], "work", "thisrun"
    )
    return config


def tidy_coupler(config):
    if config["general"]["standalone"] == False:
        config["general"]["coupler"].tidy(config)
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


def throw_away_some_infiles(config):
    if config["general"]["run_number"] == 1:
        return config
    monitor_file = logfiles.logfile_handle
    monitor_file.write("throwing away restart_in files \n")
    for model in config["general"]["valid_model_names"]:
        print(f"{model}")
        if "thisrun_restart_in_dir" in config[model]:
            if os.path.isdir(config[model]["thisrun_restart_in_dir"]):
                for root, dirs, files in os.walk(
                    config[model]["thisrun_restart_in_dir"]
                ):
                    for name in files:
                        source = os.path.join(root, name)
                        os.remove(source)
                        print(f"Removing {source}")
    return config


def copy_all_results_to_exp(config):
    monitor_file = logfiles.logfile_handle
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

            if not os.stat(source).st_size > 0:  # skip empty files
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
                linkdest = resolve_symlinks(source)
                # newlinkdest = (
                #    destination.rsplit("/", 1)[0] + "/" + linkdest.rsplit("/", 1)[-1]
                # )
                if os.path.islink(destination):
                    destdest = resolve_symlinks(source)
                    if linkdest == destdest:
                        # both links are identical, skip
                        continue
                    # os.remove(destination)
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
