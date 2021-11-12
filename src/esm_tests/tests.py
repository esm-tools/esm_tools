from dataclasses import dataclass
import copy
import difflib
import glob
import os
import re
import shutil
import time
import colorama
import yaml
from esm_parser import determine_computer_from_hostname
from esm_runscripts import color_diff
from loguru import logger

from .read_shipped_data import *
from .test_utilities import *

# Bold strings
bs = "\033[1m"
be = "\033[0m"

# Define default files for comparisson
compare_files = {"comp": ["comp-"], "run": [".run", "finished_config", "namelists"]}


class Comparison:
    """Compares two ESM Tools files"""

    def __init__(self, test_file, truth_file):
        """
        Parameters
        ----------
        test_file : str
            str representation (already opened and read in) of the file (e.g.
            run file, compilatoin script, namelist) which you want to check
        truth_file : str
            str representation of the file which is known to be a valid truth.
        """
        self.test_file = test_file
        self.truth_file = truth_file

    @classmethod
    def from_filepaths(cls, test_file, truth_file):
        with open(test_file, "r") as f1:
            test_file = f1.read()
        with open(test_file, "r") as f2:
            truth_file = f2.read()
        return cls(test_file, truth_file)

    @classmethod
    def from_test_filepath_and_pkg(cls, test_file, truth_file):
        """
        Given a test filepath, and a relative truth path, return a new Comparison.

        For the relative truth path, we are reading from the package. So,
        assuming you want to use the following as your truth:

        >>> truth_file = "ollie/run/awicm/awicm2-initial-monthly/scripts/awicm2-initial-monthly_compute_20000101 -20000131.run"

        This would check the run file for a run of awicm using a awicm2
        initialization with monthly restarts which is run on the ollie HPC.
        """
        # get last tested (Truth)
        with open(test_file, "r") as f:
            test_file = f1.read()
        truth_file = read_shipped_data.get_last_tested(truth_file)
        return cls(test_file, truth_file)


class CompileFileComparison(Comparison):
    pass


class SadFileComparison(Comparison):
    pass


class FinishedESMConfigComparison(Comparison):
    pass


def NamelistsComparison(Comparison):
    pass


#######################################################################################
# TESTS
#######################################################################################
def comp_test(info):
    scripts_info = info["scripts"]
    cd_format = re.compile("         cd (.*)")
    user_info = info["user"]
    this_computer = info["this_computer"]

    c = 0
    for model, scripts in scripts_info.items():
        if model == "general":
            continue
        for script, v in scripts.items():
            if info["hold"] and c != 0:
                input("Press ENTER to continue...")
            c += 1
            progress = round(c / scripts_info["general"]["num_scripts"] * 100, 1)
            version = v["version"]
            if v["comp_command"]:
                comp_command = f"{v['comp_command']} --no-motd -k"
            else:
                comp_command = f"esm_master comp-{model}-{version} --no-motd -k"
            general_model_dir = f"{user_info['test_dir']}/comp/{model}"
            model_dir = f"{user_info['test_dir']}/comp/{model}/{model}-{version}"
            logger.info(f"\tCOMPILING ({progress}%) {model}-{version}:")
            if not os.path.isdir(general_model_dir):
                os.makedirs(general_model_dir)
            os.chdir(general_model_dir)

            if os.path.isdir(model_dir):
                v["action"] = {"comp": "Directory already exists"}
                logger.info(f"\t\tDirectory already exists, skipping")
                with open(f"{model_dir}/comp.out") as o:
                    out = o.read()
            else:
                # Gets the source code if actual compilation is required
                if info["actually_compile"]:
                    # Downloading or copying
                    with open(f"{os.path.dirname(v['path'])}/config.yaml", "r") as cf:
                        config_test = yaml.load(cf, Loader=yaml.FullLoader)
                    copying = (
                        config_test["comp"]
                        .get("cp_instead_of_download", {})
                        .get(this_computer, {})
                        .get(f"{model}-{version}")
                    )
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
                    else:
                        get_command = f"esm_master get-{model}-{version} --no-motd"
                        logger.info("\t\tDownloading")
                        out = sh(get_command)
                        if "Traceback (most recent call last):" in out:
                            logger.error(f"\t\t\tProblem downloading!\n\n{out}")
                # For no compilation trick esm_master into thinking that the source code has been downloaded
                else:
                    # Evaluate and create folders to trick esm_master
                    out = sh(f"{comp_command} -c")
                    folders = []
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
                            f'NOT TESTING {model + version}: "cd" command not found'
                        )
                        continue
                    prim_f = folders[0]
                    folders.append(f"{model}-{version}")
                    folders = [x for x in set(folders)]
                    os.mkdir(prim_f)
                    for folder in folders:
                        os.mkdir(prim_f + "/" + folder)

                    # Get files from the esm_test/runscripts/<model>/comp_files4check_runs
                    # (i.e. namelists that are hosted in another repository could be
                    # placed there so that checks can run successfully without having to
                    # download the code).
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

    return scripts_info


def run_test(info):
    scripts_info = info["scripts"]
    user_info = info["user"]
    actually_run = info["actually_run"]
    c = 0
    submitted = []
    # Loop through tests
    for model, scripts in scripts_info.items():
        if model == "general":
            continue
        for script, v in scripts.items():
            if info["hold"] and c != 0:
                input("Press ENTER to continue...")
            c += 1
            progress = round(c / scripts_info["general"]["num_scripts"] * 100, 1)
            version = v["version"]
            runscript_path = v["path"]
            general_run_dir = f"{user_info['test_dir']}/run/{model}/"
            run_dir = f"{general_run_dir}/{script}"
            model_dir = f"{user_info['test_dir']}/comp/{model}/{model}-{version}"
            logger.info(f"\tSUBMITTING ({progress}%) {model}/{script}:")
            if not os.path.isdir(general_run_dir):
                os.makedirs(general_run_dir)

            # Check if the simulation exists
            if os.path.isdir(run_dir):
                v["action"]["submission"] = "Directory already exists"
                logger.info(f"\t\tDirectory already exists, skipping")
                with open(f"{run_dir}/run.out", "r") as o:
                    out = o.read()
                if actually_run:
                    submitted.append((model, script))
            else:
                os.chdir(os.path.dirname(runscript_path))

                if actually_run:
                    check_flag = ""
                else:
                    check_flag = "-c"
                # Export test variables
                env_vars = [
                    f"ACCOUNT='{user_info['account']}'",
                    f"ESM_TESTING_DIR='{general_run_dir}'",
                    f"MODEL_DIR='{model_dir}'",
                ]
                run_command = (
                    f"esm_runscripts {v['path']} -e {script} --open-run {check_flag}"
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
        for model, script in submitted:
            v = scripts_info[model][script]
            progress = round(subc / total_sub * 100, 1)
            exp_dir = f"{user_info['test_dir']}/run/{model}/{script}/"
            exp_dir_log = f"{exp_dir}/log/"
            for f in os.listdir(exp_dir_log):
                if "_compute_" in f and ".log" in f:
                    with open(f"{exp_dir_log}/{f}") as m:
                        observe_out = m.read()
                        if "Reached the end of the simulation, quitting" in observe_out:
                            logger.info(
                                f"\tRUN FINISHED ({progress}%) {model}/{script}"
                            )
                            logger.info(f"\t\tSuccess!")
                            finished_runs.append(cc)
                            subc += 1
                            v["state"]["run_finished"] = True
                            success = check(
                                info,
                                "run",
                                model,
                                version,
                                "",
                                script,
                                v,
                            )
                        elif "ERROR:" in observe_out:
                            logger.info(
                                f"\tRUN FINISHED ({progress}%) {model}/{script}"
                            )
                            logger.error(f"\t\tSimulation crashed!")
                            finished_runs.append(cc)
                            subc += 1
                            v["state"]["run_finished"] = False
                            success = check(
                                info,
                                "run",
                                model,
                                version,
                                "",
                                script,
                                v,
                            )
            if not info["keep_run_folders"]:
                folders_to_remove = [
                    "run_",
                    "restart",
                    "outdata",
                    "input",
                    "forcing",
                    "unknown",
                ]
                logger.debug(f"\t\tDeleting {folders_to_remove}")
                for folder in os.listdir(exp_dir):
                    for fr in folders_to_remove:
                        if fr in folder:
                            shutil.rmtree(f"{exp_dir}/{folder}")
                            continue
            # append to finished runs
            cc += 1
        finished_runs = list(set(finished_runs))
        for indx in finished_runs[::-1]:
            del submitted[indx]

        if infoc == 10 and len(submitted) > 0:
            infoc = 0
            runs = ""
            for model, script in submitted:
                runs += f"\t- {model}/{script}\n"
            logger.info(f"\nWaiting for the following runs to finish:\n{runs}")
        else:
            infoc += 1
        if len(submitted) > 0:
            time.sleep(30)

    return scripts_info


#######################################################################################
# CHECKS
#######################################################################################
def check(info, mode, model, version, out, script, v):
    success = True
    mode_name = {"comp": "compilation", "submission": "submission", "run": "runtime"}
    last_tested_dir = info["last_tested_dir"]
    this_computer = info["this_computer"]
    user_info = info["user"]
    actually_run = info["actually_run"]

    # Load config for this mode
    with open(f"{os.path.dirname(v['path'])}/config.yaml", "r") as c:
        config_test = yaml.load(c, Loader=yaml.FullLoader)
    if mode == "submission":
        config_mode = "run"
    else:
        config_mode = mode
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
        # Add specific errors
        if mode == "comp":
            errors.append("errors occurred!")
        if mode == "submission":
            errors.extend(["Traceback (most recent call last):", "ERROR"])
        # Loop through errors
        for error in errors:
            if error in out:
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
        )
        v["state"][f"{mode}_files"] = files_checked
        success = success and files_checked

    # Compare scripts with previous, if existing
    this_compare_files = copy.deepcopy(compare_files[config_mode])
    this_compare_files.extend(config_test.get(test_type, {}).get("compare", []))
    this_test_dir = f"{config_mode}/{model}/{subfolder}/"
    v["state"][f"{mode}_files_identical"] = True
    for cfile in this_compare_files:
        ignore_lines = info["ignore"].get(cfile, [])
        subpaths_source, subpaths_target = get_rel_paths_compare_files(
            info, cfile, this_test_dir
        )
        for sp, sp_t in zip(subpaths_source, subpaths_target):
            if not os.path.isfile(f"{user_info['test_dir']}/{sp}"):
                logger.error(f"\t\t'{sp}' file is missing!")
                identical = False
            else:
                # Check if it exist in last_tested
                if os.path.isfile(f"{last_tested_dir}/{this_computer}/{sp_t}"):
                    identical, differences = print_diff(
                        f"{last_tested_dir}/{this_computer}/{sp_t}",
                        f"{user_info['test_dir']}/{sp}",
                        sp,
                        ignore_lines,
                    )
                    success += identical
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


def exist_files(files, path):
    files_checked = True
    for f in files:
        if "*" in f:
            listing = glob.glob(f"{path}/{f}")
            if len(listing) == 0:
                logger.error(f"\t\tNo files following the pattern '{f}' were created!")
                files_checked = False
        else:
            if not os.path.isfile(f"{path}/{f}"):
                logger.error(f"\t\t'{f}' does not exist!")
                files_checked = False
    return files_checked


def get_rel_paths_compare_files(info, cfile, this_test_dir):
    user_info = info["user"]
    subpaths = []
    if cfile == "comp-":
        for f in os.listdir(f"{user_info['test_dir']}/{this_test_dir}"):
            if cfile in f:
                subpaths.append(f"{this_test_dir}/{f}")
        if len(subpaths) == 0:
            logger.error("\t\tNo 'comp-*.sh' file found!")
    elif cfile in [".run", "finished_config"]:
        files_to_folders = {".run": "scripts", "finished_config": "config"}
        ctype = files_to_folders[cfile]
        ldir = os.listdir(f"{user_info['test_dir']}/{this_test_dir}")
        ldir.sort()
        for f in ldir:
            # Take the first run directory
            if "run_" in f:
                cf_path = f"{this_test_dir}/{f}/{ctype}/"
                cfiles = glob.glob(f"{user_info['test_dir']}/{cf_path}/*{cfile}*")
                # If not found, try in the general directory
                if len(cfiles) == 0:
                    cf_path = f"{this_test_dir}/{ctype}/"
                    cfiles = glob.glob(f"{user_info['test_dir']}/{cf_path}/*{cfile}*")
                # Make sure we always take the first run
                cfiles.sort()
                num = 0
                if os.path.islink(
                    f"{user_info['test_dir']}/{cf_path}/{cfiles[num].split('/')[-1]}"
                ):
                    num = 1
                subpaths.append(f"{cf_path}/{cfiles[num].split('/')[-1]}")
                break
    elif cfile == "namelists":
        # Get path of the finished_config
        s_config_yaml, _ = get_rel_paths_compare_files(
            info, "finished_config", this_test_dir
        )
        namelists = extract_namelists(f"{user_info['test_dir']}/{s_config_yaml[0]}")
        ldir = os.listdir(f"{user_info['test_dir']}/{this_test_dir}")
        ldir.sort()
        for f in ldir:
            # Take the first run directory
            if "run_" in f:
                cf_path = f"{this_test_dir}/{f}/work/"
                for n in namelists:
                    if not os.path.isfile(f"{user_info['test_dir']}/{cf_path}/{n}"):
                        logger.debug(f"'{cf_path}/{n}' does not exist!")
                    subpaths.append(f"{cf_path}/{n}")
                break
    else:
        subpaths = [f"{this_test_dir}/{cfile}"]

    # Remove run directory from the targets
    subpaths_source = subpaths
    subpaths_target = []
    datestamp_format = re.compile(r"_[\d]{8}-[\d]{8}$")
    for sp in subpaths:
        sp_t = ""
        pieces = sp.split("/")
        for p in pieces:
            if "run_" not in p:
                sp_t += f"/{p}"
        # Remove the datestamp
        if datestamp_format.findall(sp_t):
            sp_t = sp_t.replace(datestamp_format.findall(sp_t)[0], "")
        subpaths_target.append(sp_t)

    return subpaths_source, subpaths_target


def extract_namelists(s_config_yaml):
    # Read config file
    with open(s_config_yaml, "r") as c:
        config = yaml.load(c, Loader=yaml.FullLoader)

    namelists = []
    for component in config.keys():
        namelists_component = config[component].get("namelists", [])
        for nml in namelists_component:
            if nml in config[component].get("config_sources", {}):
                namelists.append(nml)

    return namelists


#######################################################################################
# OUTPUT
#######################################################################################
def print_diff(sscript, tscript, name, ignore_lines):
    script_s = open(sscript).readlines()
    script_t = open(tscript).readlines()

    # Check for ignored lines
    new_script_s = []
    for line in script_s:
        ignore_this = False
        for iline in ignore_lines:
            if iline in line:
                ignore_this = True
        if not ignore_this:
            new_script_s.append(line)
    script_s = new_script_s
    new_script_t = []
    for line in script_t:
        ignore_this = False
        for iline in ignore_lines:
            if iline in line:
                ignore_this = True
        if not ignore_this:
            new_script_t.append(line)
    script_t = new_script_t

    diffobj = difflib.SequenceMatcher(a=script_s, b=script_t)
    differences = ""
    if diffobj.ratio() == 1:
        logger.info(f"\t\t'{name}' files are identical")
        identical = True
    else:
        # Find differences
        pdifferences = ""
        for line in color_diff(difflib.unified_diff(script_s, script_t)):
            differences += line
            pdifferences += f"\t\t{line}"

        logger.info(f"\n\tDifferences in {name}:\n{pdifferences}\n")
        # input("Press enter to continue...")
        identical = False

    return identical, differences


def save_files(info, user_choice):
    scripts_info = info["scripts"]
    actually_run = info["actually_run"]
    user_info = info["user"]
    last_tested_dir = info["last_tested_dir"]
    runscripts_dir = get_runscripts_dir()
    this_computer = info["this_computer"]
    if not user_choice:
        not_answered = True
        while not_answered:
            # Ask if saving of the compared files is required
            save = input(
                f"Would you like to save the files in the '{last_tested_dir}' folder for later "
                + "comparisson and/or committing to GitHub?[y/n]: "
            )
            if save == "y":
                not_answered = False
            elif save == "n":
                logger.info(f"No files will be saved in '{last_tested_dir}'")
                not_answered = False
                return
            else:
                print(f"'{save}' is not a valid answer!")

    # Select test types
    if info["actually_compile"]:
        test_type_c = "actual"
    else:
        test_type_c = "check"
    if actually_run:
        test_type_r = "actual"
    else:
        test_type_r = "check"

    logger.info(f"Saving files to '{last_tested_dir}'...")
    # Loop through models
    for model, scripts in scripts_info.items():
        if model == "general":
            continue
        model_config = f"{runscripts_dir}/{model}/config.yaml"
        if not os.path.isfile(model_config):
            logger.error(f"'{model_config}' not found!")
        with open(model_config, "r") as c:
            config_test = yaml.load(c, Loader=yaml.FullLoader)
        compare_files_comp = copy.deepcopy(compare_files["comp"])
        compare_files_comp.extend(
            config_test.get("comp", {}).get(test_type_c, {}).get("compare", [])
        )
        compare_files_run = copy.deepcopy(compare_files["run"])
        compare_files_run.extend(
            config_test.get("run", {}).get(test_type_r, {}).get("compare", [])
        )
        # Loop through scripts
        for script, v in scripts.items():
            version = v["version"]
            # Loop through comp and run
            for mode in ["comp", "run"]:
                if mode == "comp":
                    this_compare_files = compare_files_comp
                    subfolder = f"{model}-{version}"
                elif mode == "run":
                    this_compare_files = compare_files_run
                    subfolder = f"{script}"
                this_test_dir = f"{mode}/{model}/{subfolder}/"
                # Loop through comparefiles
                for cfile in this_compare_files:
                    subpaths_source, subpaths_target = get_rel_paths_compare_files(
                        info, cfile, this_test_dir
                    )
                    for sp, sp_t in zip(subpaths_source, subpaths_target):
                        if os.path.isfile(f"{last_tested_dir}/{sp_t}"):
                            logger.debug(
                                f"\t'{sp_t}' file in '{last_tested_dir}' will be overwritten"
                            )
                        if not os.path.isdir(
                            os.path.dirname(f"{last_tested_dir}/{this_computer}/{sp_t}")
                        ):
                            os.makedirs(
                                os.path.dirname(
                                    f"{last_tested_dir}/{this_computer}/{sp_t}"
                                )
                            )
                        shutil.copy2(
                            f"{user_info['test_dir']}/{sp}",
                            f"{last_tested_dir}/{this_computer}/{sp_t}",
                        )
    # Load current state
    with open(get_state_yaml_path(), "r") as st:
        current_state = yaml.load(st, Loader=yaml.FullLoader)
    # Update with this results
    results = format_results(info)
    current_state = deep_update(current_state, results)
    current_state = sort_dict(current_state)
    with open(get_state_yaml_path(), "w") as st:
        state = yaml.dump(current_state)
        st.write(state)


def print_results(results, info):
    colorama.init(autoreset=True)

    if info.get("bulletpoints", False):
        bp = "- "
    else:
        bp = ""

    logger.info("")
    logger.info("")
    logger.info(f"{bs}RESULTS{be}")
    logger.info("")
    for model, versions in results.items():
        logger.info(f"{bp}{colorama.Fore.CYAN}{model}:")
        for version, scripts in versions.items():
            logger.info(f"    {bp}{colorama.Fore.MAGENTA}{version}:")
            for script, computers in scripts.items():
                logger.info(f"        {bp}{colorama.Fore.WHITE}{script}:")
                for computer, data in computers.items():
                    if data["compilation"]:
                        compilation = f"{colorama.Fore.GREEN}compiles"
                    else:
                        compilation = f"{colorama.Fore.RED}compilation failed"
                    if data["run"]:
                        run = f"{colorama.Fore.GREEN}runs"
                    else:
                        run = f"{colorama.Fore.RED}run failed"
                    logger.info(
                        f"            {bp}{colorama.Fore.WHITE}{computer}:\t{compilation}\t{run}"
                    )
    logger.info(f"{colorama.Fore.WHITE}")
    logger.info("")


def format_results(info):
    scripts_info = info["scripts"]
    results = {}
    for model, scripts in scripts_info.items():
        if model == "general":
            continue
        results[model] = {}
        for script, v in scripts.items():
            version = v["version"]
            results[model][version] = results[model].get(version, {})
            results[model][version][script] = results[model][version].get(script, {})
            state = v["state"]
            compilation = (
                state["comp"]
                and state.get("comp_files", True)
                and state.get("comp_files_identical", True)
            )
            run = (
                state.get("run_finished", True)
                and state.get("run_files", True)
                and state["submission"]
                and state.get("submission_files_identical", True)
            )
            results[model][version][script][info["this_computer"]] = {
                "compilation": compilation,
                "run": run,
            }

    return results


def sort_dict(dict_to_sort):
    if isinstance(dict_to_sort, dict):
        dict_to_sort = {key: dict_to_sort[key] for key in sorted(dict_to_sort.keys())}
        for key, value in dict_to_sort.items():
            dict_to_sort[key] = sort_dict(value)

    return dict_to_sort


