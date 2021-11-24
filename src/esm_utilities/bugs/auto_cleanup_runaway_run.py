#!/usr/bin/env python3
"""
This program rectifies a runaway experiment.

Purpose
=======
Sometimes, a run may crash due to hardware problems (lack of disk space, for
example). Unfortunately, the esm-tools keep running in this case, and produce
many tiny run_????????-???????? folders. It would be nice to have a small
utility program which is able to restart the run from the last know "useful"
state. This would involve the following:

1. Programmatically identifying which year was the last correct one
2. Removing the following run_* folders
3. Resetting the date file to start the run again
4. Launching the job in check mode, and informing the user about missing files.
   Some of these are normal to be missing, and that should be explicitly stated.
5. If desired, actually submitting a job, or, telling the user how to do this
   on their own

Usage
=====
Run the following::

    $ module load python3
    $ ./cleanup_runaway_run.py
    # ... tab complete the path to the top of a runaway experiment ...
    # ... answer the questions ...
    # done!

Author
======
Paul Gierz (pgierz@awi.de)
AWI Bremerhaven
01.2021

Review and Test
===============
Gregor Knorr (gknorr@awi.de)

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


RUN_FOLDER_PATTERN = "run_${datestamp}"
COMPUTE_LOG_FILE_PATTERNS = "${expid}_compute_${datestamp}_${jobid}.log"
SAD_FILE_LOG_PATTERNS = "${expid}_compute_"


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
    if sys.argv[1] and os.path.exists(sys.argv[1]):
        set_up_logger(sys.argv[1])
        logger.info(f"Using {sys.argv[1]}")
        return sys.argv[1]
    path = questionary.path("Please enter the top path (Tab Complete):").ask()
    set_up_logger(path)
    return path


def find_last_valid_year(path):
    """Determines the last year that ran by looking at the restart/echam folder"""
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


def remove_overflow_log_files(path, year):
    script_dir = f"{path}/scripts"
    all_files = os.listdir(script_dir)
    expid = path.split("/")[-1]
    remove_log_files = []
    remove_sad_files = []
    remove_monitoring_files = []
    year += 1  # PG: Keep also one extra year, just to be safe
    for f in sorted(all_files):
        if f.startswith(f"{expid}_compute") and "-" in f and f.endswith(".log"):
            this_year = int(f.replace(f"{expid}_compute_", "")[:4])
            if this_year > year:
                remove_log_files.append(f)
        # monitoring_file_22450101-22451231.out
        if f.startswith(f"monitoring_file_"):
            this_year = int(f.replace("monitoring_file_", "")[:4])
            if this_year > year:
                remove_monitoring_files.append(f)
        if f.startswith(f"{expid}_compute") and "-" in f and f.endswith(".sad"):
            this_year = int(f.replace(f"{expid}_compute_", "")[:4])
            if this_year > year:
                remove_sad_files.append(f)
        if f.startswith(f"{expid}_compute_{year}"):
            if "sad" in f:
                sad_parts = f.split(".sad")
                if sad_parts[1]:
                    try:
                        if year in sad_parts[1]:
                            remove_sad_files.append(f)
                    except TypeError:
                        logger.error(f"Something wrong with {f}, skipping...")
    logger.info(
        f"The following log files can be removed, as they point to years beyond {year}:"
    )
    for f in remove_log_files + remove_sad_files + remove_monitoring_files:
        logger.info(f"* {f}")
    proceed = questionary.confirm("Remove logs?").ask()
    if proceed:
        for f in remove_log_files + remove_sad_files + remove_monitoring_files:
            os.remove(f"{path}/scripts/{f}")
    # Clean up broken symlinks
    for f in all_files:
        if os.path.islink(f"{path}/scripts/{f}") and not os.path.exists(
            f"{path}/scripts/{f}"
        ):
            os.remove(f"{path}/scripts/{f}")


def main():
    """Wraps up all functionality"""
    banner()
    path = get_path()
    print()  # New line to break up the log a bit
    year = find_last_valid_year(path)
    print()  # New line to break up the log a bit
    remove_overflow_run_folders(path, year)
    print()  # New line to break up the log a bit
    reset_date_file(path, year)
    print()  # New line to break up the log a bit
    remove_overflow_log_files(path, year)
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
