import copy
import glob
import os
import re
import shutil
import time
import yaml
from esm_parser import determine_computer_from_hostname
from loguru import logger

from .output import *
from .read_shipped_data import *
from .test_utilities import *

# Bold strings
bs = "\033[1m"
be = "\033[0m"

# Define default files for comparisson
compare_files = {
    "comp": ["comp-"],
    "run": [".run", "finished_config", "namelists", "filelist"],
}

"""
``config.yaml``:
    This file includes exceptions rules to be applied during the tests (such as copying
    instead of downloading the source code for compilation), and output and restart
    files to be checked that they exist after an actual run. This file needs to exist
    in each ``resources/runscripts/<model>`` folder.
"""

# Coded by @pgierz, but not finished, therefore commented.
# class Comparison:
#    """Compares two ESM Tools files"""
#
#    def __init__(self, test_file, truth_file):
#        """
#        Parameters
#        ----------
#        test_file : str
#            str representation (already opened and read in) of the file (e.g.
#            run file, compilatoin script, namelist) which you want to check
#        truth_file : str
#            str representation of the file which is known to be a valid truth.
#        """
#        self.test_file = test_file
#        self.truth_file = truth_file
#
#    @classmethod
#    def from_filepaths(cls, test_file, truth_file):
#        with open(test_file, "r") as f1:
#            test_file = f1.read()
#        with open(test_file, "r") as f2:
#            truth_file = f2.read()
#        return cls(test_file, truth_file)
#
#    @classmethod
#    def from_test_filepath_and_pkg(cls, test_file, truth_file):
#        """
#        Given a test filepath, and a relative truth path, return a new Comparison.
#
#        For the relative truth path, we are reading from the package. So,
#        assuming you want to use the following as your truth:
#
#        >>> truth_file = "ollie/run/awicm/awicm2-initial-monthly/scripts/awicm2-initial-monthly_compute_20000101 -20000131.run"
#
#        This would check the run file for a run of awicm using a awicm2
#        initialization with monthly restarts which is run on the ollie HPC.
#        """
#        # get last tested (Truth)
#        with open(test_file, "r") as f:
#            test_file = f1.read()
#        truth_file = read_shipped_data.get_last_tested(truth_file)
#        return cls(test_file, truth_file)
#
#
# class CompileFileComparison(Comparison):
#    pass
#
#
# class SadFileComparison(Comparison):
#    pass
#
#
# class FinishedESMConfigComparison(Comparison):
#    pass
#
#
# def NamelistsComparison(Comparison):
#    pass


#######################################################################################
# TESTS
#######################################################################################
def comp_test(info):
    """
    Downloads/copies the source codes of the models to be tested and compiles them. If
    the ``--check`` flag is provided in the ``esm_tests`` call, it does not
    download/copy and compile the source codes, but produces the ``comp-*.sh``
    compilation scripts instead, for comparison purposes with last tested versions of
    them.

    Parameters
    ----------
    info : esm_tests.Info
        Dictionary that contains the testing info.
    """
    # Load dictionaries from ``info``
    scripts_info = info["scripts"]
    user_info = info["user"]
    this_computer = info["this_computer"]

    # Set the regex format for check compilations
    cd_format = re.compile("         cd (.*)")

    # Set the counter to 0
    c = 0
    logger.info("")
    # Loop through the models and their scripts
    for model, scripts in scripts_info.items():
        if model == "general":
            continue
        # Loop through the scripts of each model
        for script, v in scripts.items():
            # If the user has specified the ``hold`` flag, wait for input
            if info["hold"] and c != 0:
                input("Press ENTER to continue...")
            # Calculate progress
            c += 1
            progress = round(c / scripts_info["general"]["num_scripts"] * 100, 1)

            # Define variables from the runscript
            version = v["version"]
            if v["comp_command"]:
                comp_command = f"{v['comp_command']} --no-motd -k"
            else:
                comp_command = f"esm_master comp-{model}-{version} --no-motd -k"
            general_model_dir = f"{user_info['test_dir']}/comp/{model}"
            model_dir = f"{user_info['test_dir']}/comp/{model}/{model}-{version}"
            title = f"COMPILING ({progress}%) {bs}{model}-{version}{be}"
            logger.info("\t" + info["group_output"]["startg"].format(title))

            # Create the folders where this test should run and change directory
            if not os.path.isdir(general_model_dir):
                os.makedirs(general_model_dir)
            os.chdir(general_model_dir)

            # Check if the test compilation has been already run (the compilation
            # directory already exists). This is particularly important for different
            # testing scripts that use the same version of a model, so that compilation
            # is not run more than once
            if os.path.isdir(model_dir):
                v["action"] = {"comp": "Directory already exists"}
                logger.info(f"\t\tDirectory already exists, skipping")
                with open(f"{model_dir}/comp.out") as o:
                    out = o.read()
            else:
                # ACTUAL COMPILATION
                # ==================
                # Get the source code if actual compilation is required
                if info["actually_compile"]:
                    # Downloading or copying
                    # ----------------------
                    # Load the ``config.yaml`` for this model to find if the source
                    # code needs to be downloaded or copied
                    with open(f"{os.path.dirname(v['path'])}/config.yaml", "r") as cf:
                        config_test = yaml.load(cf, Loader=yaml.FullLoader)
                    copying = (
                        config_test["comp"]
                        .get("cp_instead_of_download", {})
                        .get(this_computer, {})
                        .get(f"{model}-{version}")
                    )
                    # If copying of the source code is required go ahead, copy and,
                    # if necessary, untar
                    if copying:
                        logger.info("\t\tCopying")
                        out = ""
                        if os.path.isdir(copying):
                            shutil.copytree(copying, general_model_dir)
                        else:
                            shutil.copy2(copying, general_model_dir)
                        if copying.endswith("tar.gz"):
                            logger.info("\t\tUntaring")
                            file2untar = (
                                f"{general_model_dir}/{os.path.basename(copying)}"
                            )
                            out += sh(f"tar -xvf {file2untar}")
                            os.remove(file2untar)
                    # If download of the source code is required, run
                    # ``esm_master get-<model>-<version>``
                    else:
                        get_command = f"esm_master get-{model}-{version} --no-motd"
                        logger.info("\t\tDownloading")
                        out = sh(get_command)
                        if "Traceback (most recent call last):" in out:
                            logger.error(f"\t\t\tProblem downloading!\n\n{out}")

                # CHECK COMPILATION
                # =================
                # No actual compilation: for a check compilation where no real
                # compilation occurs, but the ``comp-`` scripts are generated, we need
                # to trick esm_master into thinking that all the folders source code
                # folders are available, even if they were not downloaded or copied
                else:
                    # Evaluate and create folders to trick esm_master
                    # -----------------------------------------------
                    # Run a ``esm_master --check``
                    out = sh(f"{comp_command} -c")
                    folders = []
                    # Search for the folders to be created
                    for line in out.split("\n"):
                        if "cd" in line and "cd .." not in line:
                            found_format = cd_format.findall(line)
                            if len(found_format) > 0:
                                if (
                                    ";" not in found_format[0]
                                    and "/" not in found_format[0]
                                ):
                                    folders.append(found_format[0])
                    if len(folders) == 0:
                        logger.warning(
                            f'NOT TESTING {model}{version}: "cd" command not found'
                        )
                        continue

                    # Make the folders needed to trick esm_master
                    prim_f = folders[0]
                    folders.append(f"{model}-{version}")
                    folders = [x for x in set(folders)]
                    os.mkdir(prim_f)
                    for folder in folders:
                        os.mkdir(prim_f + "/" + folder)

                    # Get files from the
                    # ``esm_test/runscripts/<model>/comp_files4check_runs`` (i.e.
                    # namelists that are hosted in another repository could be placed
                    # there so that checks can run successfully without having to
                    # download the code)
                    copy_comp_files4check_runs(
                        script, v, f"{general_model_dir}/{prim_f}"
                    )

                # Compile
                if info["actually_compile"]:
                    logger.info("\t\tCompiling")
                else:
                    logger.info("\t\tWritting compilation scripts")
                out = sh(comp_command)

                # Write output file
                with open(f"{model_dir}/comp.out", "w") as o:
                    o.write(out)

                # Move and cleanup files
                for f in os.listdir(general_model_dir):
                    if "comp-" in f and not os.path.isfile(f"{model_dir}/{f}"):
                        shutil.move(f"{general_model_dir}/{f}", model_dir)
                    elif "comp-" in f and os.path.isfile(f"{model_dir}/{f}"):
                        os.remove(f"{general_model_dir}/{f}")
                    if f == "dummy_script.sh":
                        os.remove(f"{general_model_dir}/{f}")

            # Checks
            success = check(info, "comp", model, version, out, script, v)
            if success:
                logger.info("\t\tSuccess!")
            logger.info(f'\t\t{info["group_output"]["endg"]}')


def run_test(info):
    """
    Runs the simulations and notifies when they are finished. If the ``--check`` flag
    is given in the ``esm_tests`` call, check simulations are run
    (``esm_runscripts -c``).

    Parameters
    ----------
    info : esm_tests.Info
        Dictionary that contains the testing info.
    """
    # Load dictionaries from ``info``
    scripts_info = info["scripts"]
    user_info = info["user"]
    actually_run = info["actually_run"]
    run_errors = ["ERROR:", "slurmstepd: error: *** STEP", "PBS: job killed: walltime"]

    # Set the counter to 0
    c = 0
    submitted = []
    # Loop through the models and their scripts
    for model, scripts in scripts_info.items():
        if model == "general":
            continue
        # Loop through the scripts of each model
        for script, v in scripts.items():
            # If the user has specified the ``hold`` flag, wait for input
            if info["hold"] and c != 0:
                input("Press ENTER to continue...")
            c += 1
            progress = round(c / scripts_info["general"]["num_scripts"] * 100, 1)

            # Define variables from the runscript
            version = v["version"]
            runscript_path = v["path"]
            general_run_dir = f"{user_info['test_dir']}/run/{model}/"
            run_dir = f"{general_run_dir}/{script}"
            model_dir = f"{user_info['test_dir']}/comp/{model}/{model}-{version}"
            title = f"SUBMITTING ({progress}%) {bs}{model}/{script}{be}"
            logger.info(f"\t" + info["group_output"]["startg"].format(title))

            # Create the folders where this test should run and change directory
            if not os.path.isdir(general_run_dir):
                os.makedirs(general_run_dir)

            # If the simulation already exists, only read the ``.out`` file
            if os.path.isdir(run_dir):
                v["action"]["submission"] = "Directory already exists"
                logger.info(f"\t\tDirectory already exists, skipping")
                with open(f"{run_dir}/run.out", "r") as o:
                    out = o.read()
                if actually_run:
                    submitted.append((model, script))
            # If the simulation does not exists, run it or check it
            else:
                # Change the path to that of the runscript
                os.chdir(os.path.dirname(runscript_path))

                # Define the check flag if necessary
                additional_actions = ""
                if actually_run:
                    check_flag = ""
                else:
                    check_flag = "-c"
                    # In case of iterative coupling, submit the subscripts too
                    if v["iterative_coupling"]:
                        for imodel, mdata in v["iterative_models"].items():
                            additional_actions = (
                                f"{additional_actions}; "
                                f"echo {'='*10}; echo {imodel}; echo {'='*10}; "
                                f"esm_runscripts "
                                f"{os.path.dirname(v['path'])}/{mdata['script']} "
                                f"-e {script} --open-run {check_flag}"
                            )
                # Export test variables and run the simulation
                env_vars = [
                    f"ACCOUNT='{user_info['account']}'",
                    f"ESM_TESTING_DIR='{general_run_dir}'",
                    f"MODEL_DIR='{model_dir}'",
                ]
                run_command = (
                    f"esm_runscripts {v['path']} -e {script} --open-run "
                    f"{check_flag}{additional_actions}"
                )
                out = sh(run_command, env_vars)

                # Write output file
                if os.path.isdir(run_dir):
                    with open(f"{run_dir}/run.out", "w") as o:
                        o.write(out)

                if actually_run:
                    submitted.append((model, script))

            # Check submission
            success = check(info, "submission", model, version, out, script, v)

            logger.info(f'\t\t{info["group_output"]["endg"]}')

    # Check if simulations are finished
    total_sub = len(submitted)
    subc = 1
    if total_sub > 0:
        logger.info(
            "\nWaiting for submitted runs to finish... You can choose to cancel the "
            + "script now and come back to it at a later state with the same command "
            + "you submitted it, but remember to remove the '-d' so that nothing is "
            + "deleted and this script can be resumed from where it was."
        )
    infoc = 10
    while submitted:
        cc = 0
        finished_runs = []
        # Loop through models and scripts
        for model, script in submitted:
            v = scripts_info[model][script]
            version = v["version"]
            progress = round(subc / total_sub * 100, 1)
            exp_dir = f"{user_info['test_dir']}/run/{model}/{script}/"
            exp_dir_log = f"{exp_dir}/log/"
            nmodels_success = 1
            with open(f"{exp_dir}/run.out", "r") as so:
                submission_out = so.read()
            if "ERROR:" in submission_out or not v["state"].get("submission", False):
                subc, finished_runs, success = experiment_state_action(
                    info,
                    "Submission failed!",
                    False,
                    v,
                    finished_runs,
                    cc,
                    subc,
                    model,
                    script,
                    version,
                    progress,
                )
            # Search through the ``_compute_`` files for a string that indicates that
            # the run has finished
            for f in os.listdir(exp_dir_log):
                if "_compute_" in f and ".log" in f:
                    with open(f"{exp_dir_log}/{f}") as m:
                        observe_out = m.read()
                        # If the run has finished successfully label the state for
                        # ``run_finished`` as ``True`` and run a check for files that
                        # should have been created
                        if "Reached the end of the simulation, quitting" in observe_out:
                            if nmodels_success == v["nmodels_iterative_coupling"]:
                                subc, finished_runs, success = experiment_state_action(
                                    info,
                                    "Success!",
                                    True,
                                    v,
                                    finished_runs,
                                    cc,
                                    subc,
                                    model,
                                    script,
                                    version,
                                    progress,
                                )
                            else:
                                nmodels_success += 1
                        # If the run has errors label the state for ``run_finished`` as
                        # ``False`` and run a check for files that should have been
                        # created anyway
                        elif any([run_error in observe_out for run_error in run_errors]):
                            subc, finished_runs, success = experiment_state_action(
                                info,
                                "Simulation crashed!",
                                False,
                                v,
                                finished_runs,
                                cc,
                                subc,
                                model,
                                script,
                                version,
                                progress,
                            )

            # Update the testing index for the next iteration
            cc += 1

        # Remove finished runs from the ``submitted`` list
        finished_runs = list(set(finished_runs))
        for indx in finished_runs[::-1]:
            del submitted[indx]

        # Wait 10 iterations to notify the state
        if infoc == 10 and len(submitted) > 0:
            infoc = 0
            runs = ""
            for model, script in submitted:
                runs += f"\t- {model}/{script}\n"
            logger.info(f"\nWaiting for the following runs to finish:\n{runs}")
        else:
            infoc += 1

        # Wait 30 seconds to check again the state of the submitted tests
        if len(submitted) > 0:
            time.sleep(30)

    logger.info("")


#######################################################################################
# CHECKS
#######################################################################################
def check(info, mode, model, version, out, script, v):
    """
    Checks for errors, missing files and compares current files with previous files in
    ``resources/last_tested``. Behaves differently for different modes.

    Modes:
        * ``comp``: checks compilation scripts
        * ``submission``: checks submission files (``.run``, namelists, ...)
        * ``run``: same as ``submission`` but also checks for files produced by the
            simulation (restarts and output)

    Parameters
    ----------
    info : esm_tests.Info
        Dictionary that contains the testing info.
    mode : str
        Mode of the check.
    model : str
        Model for which the check is run.
    version : str
        Version of the model for which the check is run.
    out : str
        Output of the shell operation that is to be checked.
    script : str
        Name of the script to be checked.
    v : dict
        Dictionary containing script variables such as ``version``, ``comp_command``, ...

    Returns
    -------
    success : bool
        ``True`` if all checks were passed, ``False`` if not.
    """
    # Set variables
    success = True
    mode_name = {"comp": "compilation", "submission": "submission", "run": "runtime"}
    last_tested_dir = info["last_tested_dir"]
    this_computer = info["this_computer"]
    user_info = info["user"]
    actually_run = info["actually_run"]

    # Load config for this mode
    with open(f"{os.path.dirname(v['path'])}/config.yaml", "r") as c:
        config_test = yaml.load(c, Loader=yaml.FullLoader)

    # Build the ignore_dict based on the general, model and script specific ignores
    ignore_dict = copy.deepcopy(info["ignore"])
    model_ignore = config_test.get("ignore_compare", {}).get("all", {})
    deep_update(ignore_dict, model_ignore, extend_lists=True)
    script_ignore = config_test.get("ignore_compare", {}).get(script, {})
    deep_update(ignore_dict, script_ignore, extend_lists=True)

    # There are 3 modes: comp, submission and run, and 2 config_modes: comp and run.
    # This is because submission shares a lot of operations with run so they are in the
    # same config_mode group
    if mode == "submission":
        config_mode = "run"
    else:
        config_mode = mode

    # Check if the config_mode is defined in the ``config.yaml`` file, and load the
    # info for this type of test
    if config_mode not in config_test:
        logger.error(
            f"Missing '{mode}' section in '{os.path.dirname(v['path'])}/config.yaml'!"
        )
    config_test = config_test[config_mode]

    # Set mode variables
    if mode == "comp":
        if info["actually_compile"]:
            test_type = "actual"
        else:
            test_type = "check"
        actually_do = info["actually_compile"]
        subfolder = f"{model}-{version}"
    elif mode == "run":
        if actually_run:
            test_type = "actual"
        else:
            test_type = "check"
        actually_do = actually_run
        subfolder = script
    elif mode == "submission":
        # Do not perform the file checks before the simulation is finished
        if actually_run:
            test_type = "actual"
        else:
            test_type = "check"
        actually_do = False
        subfolder = script

    # Check for errors
    if actually_do or mode == "submission":
        # Check for errors in the output
        errors = config_test.get(test_type, {}).get("errors", [])
        # Get exceptions
        error_exceptions = {}
        cleaned_errors = []
        for error in errors:
            cleaned_error = error.split(" except ")[0]
            error_exceptions[cleaned_error] = find_exceptions(error)
            cleaned_errors.append(cleaned_error)
        errors = cleaned_errors
        # Add specific errors
        if mode == "comp":
            errors.append("errors occurred!")
        if mode == "submission":
            errors.extend(["Traceback (most recent call last):", "ERROR"])
        # Loop through errors
        for error in errors:
            if error in out and script not in error_exceptions.get(error, []):
                logger.error(f"\t\tError during {mode_name[mode]}!\n\n{out}")
                success = False
        if mode != "run":
            v["state"][mode] = success
    else:
        if mode == "comp":
            # No need to check for this in check mode
            v["state"][mode] = success

    # Check for missing files during an actual operation (not a check, not submission)
    if actually_do:
        # Check if files exist
        files_checked = exist_files(
            config_test.get(test_type, {}).get("files", []),
            f"{user_info['test_dir']}/{mode}/{model}/{subfolder}",
            version,
        )
        v["state"][f"{mode}_files"] = files_checked
        success = success and files_checked

    # Compare scripts with previous, if existing
    this_compare_files = copy.deepcopy(compare_files[config_mode])
    # TODO: The iterative coupling needs a rework. Therefore, no testing for files
    # is develop. Include the tests after iterative coupling is reworked
    if config_mode == "run" and v["iterative_coupling"]:
        this_compare_files = []
    this_compare_files.extend(config_test.get(test_type, {}).get("compare", []))
    this_test_dir = f"{config_mode}/{model}/{subfolder}/"
    v["state"][f"{mode}_files_identical"] = True
    # Loop through the files to be compared
    for cfile in this_compare_files:
        # Load lines to be ignored
        ignore_lines = ignore_dict.get(cfile, [])
        # Get relative paths of the files to be compared
        subpaths_source, subpaths_target = get_rel_paths_compare_files(
            info, cfile, v, this_test_dir
        )
        for sp, sp_t in zip(subpaths_source, subpaths_target):
            # Check if the source exists (this simulation file)
            if not os.path.isfile(f"{user_info['test_dir']}/{sp}"):
                logger.error(f"\t\t'{sp}' file is missing!")
                identical = False
            else:
                # Check if the target exists (in last_tested folder)
                if os.path.isfile(f"{last_tested_dir}/{this_computer}/{sp_t}"):
                    # Check if files are identical
                    identical, differences = print_diff(
                        info,
                        f"{last_tested_dir}/{this_computer}/{sp_t}",
                        f"{user_info['test_dir']}/{sp}",
                        sp,
                        ignore_lines,
                    )
                    success += identical
                    # Update state dictionaries
                    if not identical:
                        v["differences"] = v.get("differences", {})
                        v["differences"][config_mode] = v["differences"].get(
                            config_mode, {}
                        )
                        v["differences"][config_mode][sp] = differences
                        v["state"][f"{mode}_files_identical"] &= False
                    else:
                        v["state"][f"{mode}_files_identical"] &= True
                else:
                    logger.warning(f"\t\t'{sp_t}' file not yet in 'last_tested'")

    return success


def experiment_state_action(
    info, message, no_err, v, finished_runs, cc, subc, model, script, version, progress
):
    """
    Triggers checks for finished runs and reports their state.

    Parameters
    ----------
    info : esm_tests.Info
        Dictionary that contains the testing info
    message : str
        Message to report about the state
    no_err : bool
        Boolean indicating whether errors occur (``False``) or not (``True``)
    v : dict
        Dictionary containing script variables such as ``version``, ``comp_command``, ...
    finished_runs : list
        List of indexes for runs finished
    cc : int
        Run counter
    subc : int
        Counter for subruns
    model : str
        Model for which the check is run
    script : str
        Name of the script to be checked
    version : str
        Version of the model for which the check is run
    progress : float
        Percentage of the progress

    Returns
    -------
    subc : int
        Counter for subruns
    finished_runs : list
        List of indexes for runs finished
    success : bool
        Boolean indicating the success of the run
    """
    logger.info(f"\tRUN FINISHED ({progress}%) {model}/{script}")
    if no_err:
        logger.info(f"\t\t{message}")
    else:
        logger.error(f"\t\t{message}")
    finished_runs.append(cc)
    subc += 1
    v["state"]["run_finished"] = no_err
    # Run a check for run finished
    success = check(info, "run", model, version, "", script, v)

    return subc, finished_runs, success


def check_perfect(info, results):
    """
    Exits if something is not perfect in the ``results``, to trigger a failted test
    in GitHub Actions.

    Parameters
    ----------
    info : esm_tests.Info
        Dictionary with the general info about the tests.
    results : dict
        Dictionary containing the results of the testing.
    """
    all_tests_passed = True
    if info["actually_compile"]:
        comp_perfect = "compiles"
    else:
        comp_perfect = "comp files identical"
    if info["actually_run"]:
        run_perfect = "runs"
    else:
        run_perfect = "run files identical"
    for model, versions in results.items():
        for version, scripts in versions.items():
            for script, computers in scripts.items():
                for computer, data in computers.items():
                    if data["compilation"] != comp_perfect:
                        all_tests_passed = False
                    if data["run"] != run_perfect:
                        all_tests_passed = False
    if not all_tests_passed:
        sys.exit(
            "Some of the tests were not successful. Exited to trigger a failed test in GitHub Actions"
        )


def exist_files(files, path, version):
    """
    Checks whether the required files defined in
    ``resources/runscripts/<model>/config.yaml`` are present or not and sets the state
    if the ``files_check`` ``True`` (exist) or ``False`` (do not exist) depending on
    that.

    Each element of the list of files in the ``config.yaml`` can contain the following
    special functionalities:
    - ``*``: to trigger a wildcard evaluation
    - ``except [<list of versions>]``: do not check this file for the versions inside
      the ``[]``
    - ``in [<list of versions>]``: check this file only for the versions inside the
      ``[]``

    Parameters
    ----------
    files : list
        List of files to be checked, extracted from the ``config.yaml``
    path : str
        Path of the experiment
    version : str
        Version of the model
    """
    files_checked = True
    # Loop through files
    for f in files:
        exception_list = find_exceptions(f)

        # Command's logic
        if " except " in f and version in exception_list:
            continue
        elif " in " in f and not version in exception_list:
            continue
        else:
            f_path = f.split(" ")[0]

        # Check for files with wildcards
        if "*" in f_path:
            listing = glob.glob(f"{path}/{f_path}")
            if len(listing) == 0:
                logger.error(
                    f"\t\tNo files following the pattern '{f_path}' were created!"
                )
                files_checked = False
        # Check for files without wildcards
        else:
            if not os.path.isfile(f"{path}/{f_path}"):
                logger.error(f"\t\t'{f_path}' does not exist!")
                files_checked = False

    return files_checked

def find_exceptions(string_to_be_checked):
    """
    Finds exceptions on the ``string_to_be_checked`` with the format
    `` except [<item1>, <item2>, ...]``, and returns the list of the exceptions
    specified between the square brackets in the string.

    Parameters
    ----------
    string_to_be_checked : str
        String to be searched for exceptions

    Returns
    -------
    exception_list : list
        List of exceptions found on the string
    """
    exception_list = []
    # Get the commands inside the ``[]``
    if " [" in string_to_be_checked and string_to_be_checked[-1] == "]":
        exception_list = re.findall(r"(?<=\[)([^]]+)(?=\])", string_to_be_checked)
        if len(exception_list) > 1:
            raise Exception("You should only have one list per file")
        exception_list = [x.replace(" ", "") for x in exception_list[0].split(",")]

    return exception_list
