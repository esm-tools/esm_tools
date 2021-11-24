#!/usr/bin/env python3
"""
This program rectifies a runaway experiment which has suffered the infamous
"wind speed bug"

Purpose
=======
Sometimes, a run may crash due to a numerical instability induced by too high
wind speeds.  Unfortunately, the esm-tools keep running in this case, and
produce many tiny run_????????-???????? folders. It would be nice to have a
small utility program which is able to restart the run from the last know
"useful" state. This would involve the following:

1. Programmatically identifying which year was the last correct one
2. Removing the following run_* folders
3. Resetting the date file to start the run again
4. Identifying and applying a "disturbance" to the air viscosity to overcome
   the numerical instability. (More info here: https://tinyurl.com/mhhb989c)
5. Launching the job in check mode, and informing the user about missing files.
   Some of these are normal to be missing, and that should be explicitly stated.
6. If desired, actually submitting a job, or, telling the user how to do this
   on their own

Usage
=====
Run the following

    $ module load python3
    $ ./cleanup_windspeed_run.py
    # ... tab complete the path to the top of a runaway experiment ...
    # ... answer the questions ...
    # done!

Author
======
Paul Gierz (pgierz@awi.de)
AWI Bremerhaven
03.2021

Review and Test
===============
No one yet :-(

Funding
=======
PalMod II
Federal Ministry for Education and Research, Germany
"""

import os
import shutil
import subprocess
import sys

from loguru import logger
import questionary


def banner():
    print("--------------------------------------------------------")
    print()
    print("This script cleans up a run-away esm-tools run. It will:")
    print()
    print("* Reset the date file to the last appropriate year")
    print("* Remove extra run_????0101-????1231 folders")
    print("* Launch a job in check mode for you")
    print()
    print("A log of the work is placed in the log folder of your")
    print("experiment under cleanup_runaway_run.log")
    print()
    print("--------------------------------------------------------")


def set_up_logger(path):
    """Sets up logging"""
    logger.remove()
    logger.add(sys.stdout, format="{message}")
    try:
        logger.add(path + "/log/cleanup_runaway_run.log")
        logger.info(f"Everything will be logged to {path}/log/cleanup_runaway_run.log")
    except PermissionError:
        logger.error("Sorry, due to permission, I won't be able to log my activities")


def get_path():
    """Gets the path to the experiment you want to fix"""
    return questionary.path("Please enter the top path (Tab Complete):").ask()


def find_last_valid_year(path):
    """
    Determines the last year that ran by looking at the restart/echam folder

    Arguments
    ---------
    path : str
        The experiment path (top level folder) which should be examined.

    Returns
    -------
    year : int
        The next year that shall be run
    """
    expid = path.split("/")[-1]
    restart_folder = f"{path}/restart/echam"
    restart_files = sorted([f for f in os.listdir(restart_folder) if "echam" in f])
    # Remove the file without a specific date:
    restart_files.remove(f"restart_{expid}_echam.nc")
    logger.info(f"Last 5 files in {expid} for restart/echam:")
    for restart_file in restart_files[-5:]:
        logger.info(f"* {restart_file}")
    last_year = (
        restart_files[-1]
        .replace(f"restart_{expid}_", "")
        .replace("_echam.nc", "")
        .replace("1231", "")
    )
    correct_last_year_guess = questionary.confirm(
        f"I'm going to use {last_year} as the last valid year. Is that correct?"
    ).ask()
    if not correct_last_year_guess:
        last_year = questionary.text("Whats the last year? (without 1231)").ask()
    year = int(last_year) + 1
    logger.info(f"OK, the next job that will be run will be for the year {year}")
    return year


def remove_overflow_run_folders(path, date):
    """Cleans up unneeded run folders"""
    run_folders = sorted(
        [folder for folder in os.listdir(path) if folder.startswith("run_")]
    )
    logger.info("The following run folders were detected:")
    for run_folder in run_folders:
        logger.info(f"* {run_folder}")
    run_folder_dict = {}
    for run_folder in run_folders:
        run_date = run_folder.replace("run_", "").split("-")[0].replace("0101", "")
        run_folder_dict[int(run_date)] = run_folder
    remove_list = []
    for run_date, run_folder in run_folder_dict.items():
        if run_date >= date:
            remove_list.append(run_folder)
    if remove_list:
        logger.info(f"The following will be removed, as they are beyond {date}.")
        for delete in remove_list:
            logger.info(f"* {path}/{delete}")
        logger.info(
            "You can select which ones to keep, but bear in mind that this might cause the run to crash."
        )
        logger.info("Which of the following do you want to keep?")
        runs_to_keep = questionary.checkbox(
            "Select and then push Enter, or just Enter to erase all:",
            choices=remove_list,
        ).ask()
        for keep in runs_to_keep:
            remove_list.remove(keep)
        for delete in remove_list:
            logger.info(f"Deleting {path}/{delete}")
            shutil.rmtree(f"{path}/{delete}")
    else:
        logger.info(f"No overflow run folders found")


def reset_date_file(path, date):
    """Resets date file to the correct date to resume a simulation"""
    old_date_files = [f for f in os.listdir(f"{path}/scripts") if f.endswith(".date")]
    if len(old_date_files) > 1:
        logger.info("Multiple date files found!")
        old_date_file = questionary.select(
            "Which is the right one", choices=old_date_files
        ).ask()
    else:
        old_date_file = old_date_files[0]
    logger.info(f"Resetting {old_date_file}")
    with open(f"{path}/scripts/{old_date_file}", "r") as old_file:
        old_file_contents = old_file.read()
        broken_date, broken_run_number = old_file_contents.split(" ")
    logger.info(f"Broken date:          {broken_date}")
    logger.info(f"Broken run number:    {broken_run_number}")
    broken_year = int(broken_date.split("-")[0])
    overflow_runs = broken_year - date
    logger.info(f"Overflow runs:        {overflow_runs}")
    new_date = str(broken_date).replace(str(broken_year), str(date))
    new_run_number = int(broken_run_number) - overflow_runs
    logger.info(f"Fixed date:           {new_date}")
    logger.info(f"Fixed run number:     {new_run_number}")
    with open(f"{path}/scripts/{old_date_file}.new", "w") as new_file:
        new_file_contents = f"{new_date} {new_run_number}"
        new_file.write(new_file_contents)
    logger.info("Old date file contents:")
    logger.info(old_file_contents)
    logger.info("New date file contents:")
    logger.info(new_file_contents)
    replace_date_file = questionary.confirm("Replace old file by new file?").ask()
    if replace_date_file:
        os.rename(
            f"{path}/scripts/{old_date_file}.new", f"{path}/scripts/{old_date_file}"
        )


def launch_job_in_check_mode(path):
    """Launches a job in check mode"""
    expid = path.split("/")[-1]
    logger.info(f"Assumed expid: {expid}")
    expid_guess_correct = questionary.confirm("Is that correct?").ask()
    while not expid_guess_correct:
        expid = questionary.text("Please enter the expid...").ask()
        logger.info(f"Assumed expid: {expid}")
        expid_guess_correct = questionary.confirm("Is that correct?").ask()

    runscript_paths = [f for f in os.listdir(f"{path}/scripts") if f.endswith(".yaml")]
    if len(runscript_paths) == 0:
        logger.error("Couldn't find the runscript")
        runscript_path = questionary.path(
            "Please enter the runscript path (Tab-Complete)"
        ).ask()
    elif len(runscript_paths) == 1:
        runscript_path = runscript_paths[0]
    else:
        logger.info("Multiple possible runscripts found!")
        runscript_path = questionary.select(
            "Which is the right one", choices=runscript_paths
        ).ask()

    command = f"esm_runscripts {runscript_path} -e {expid} -c"
    logger.info("Launching in check mode like this:")
    logger.info(command)
    subprocess.run(command, shell=True, check=True, cwd=f"{path}/scripts")
    logger.info("Note that it is normal for some of the missing files to be listed:")
    logger.info("* namelist:    namelist.diag         (in FESOM)")
    logger.info(
        "* restart:     rad restart file      (in ECHAM, if not using concurrent radiation)"
    )
    logger.info("* restart:     nitro restart file    (in JSBACH)")
    logger.info("* restart:     land restart file     (in JSBACH)")
    return command


def set_disturb_year_file(path, year):
    """Sets the disturb year file"""
    logger.info("Setting the disturb_years file")
    logger.info(f"Looking in {path}/scripts for log files")
    logger.info(f"The following files with {year} were found:")
    for f in os.listdir(f"{path}/scripts"):
        if str(year) in f:
            logger.info(f"* {f}")
    sys.exit(1)


def submit_or_tell_user(path, command):
    """Either launches the job, or tells user how to do it"""
    logger.info(
        "If you are satisfied with the check run, you can launch the job now with:"
    )
    logger.info(command.replace("-c", ""))
    logger.info("I can do this for you now")
    launch_job = questionary.confirm("Launch the job now?").ask()
    if launch_job:
        subprocess.run(command, shell=True, check=True, cwd=f"{path}/scripts")
    else:
        logger.info(
            f"\n\n\n Final command can be: \n\t >>> {command.replace('-c', '')}"
        )


def main():
    """Wraps up all functionality"""
    banner()
    path = get_path()
    set_up_logger(path)
    print()  # New line to break up the log a bit
    year = find_last_valid_year(path)
    print()  # New line to break up the log a bit
    remove_overflow_run_folders(path, year)
    print()  # New line to break up the log a bit
    reset_date_file(path, year)
    print()  # New line to break up the log a bit
    set_disturb_year_file(path, year)
    print()  # New line to break up the log a bit
    command = launch_job_in_check_mode(path)
    submit_or_tell_user(path, command)
    sys.exit(0)
    # Celebrate
    #                                       .       .
    #                                      / `.   .' \
    #                              .---.  <    > <    >  .---.
    #                              |    \  \ - ~ ~ - /  /    |
    #                               ~-..-~             ~-..-~
    #                           \~~~\.'                    `./~~~/
    #                 .-~~^-.    \__/                        \__/
    #     Yippee!   .'  O    \     /               /       \  \
    #              (_____,    `._.'               |         }  \/~~~/
    #               `----.          /       }     |        /    \__/
    #                     `-.      |       /      |       /      `. ,~~|
    #                         ~-.__|      /_ - ~ ^|      /- _      `..-'   f: f:
    #                              |     /        |     /     ~-.     `-. _||_||_
    #                              |_____|        |_____|         ~ - . _ _ _ _ _>
    #


if __name__ == "__main__":
    main()
