import os
import sys
import subprocess
import argparse
import math
import yaml
import re
import shutil
import time
import glob
import difflib
import copy
import colorama
import regex as re
import collections.abc

from loguru import logger

from esm_runscripts import color_diff
from esm_parser import determine_computer_from_hostname


# Bold strings
bs = "\033[1m"
be = "\033[0m"

# Define default files for comparisson
compare_files = {"comp": ["comp-"], "run": [".sad", "finished_config", "namelists"]}

#######################################################################################
# INITIALIZATION
#######################################################################################
def user_config(info):
    # Check for user configuration file
    user_config = f"{info['script_dir']}/user_config.yaml"
    print()
    if not os.path.isfile(user_config):
        # Make the user configuration file
        answers = {}
        print(
            "{bs}Welcome to ESM-Tests! Automatic testing for ESM-Tools devs\n"
            + "**********************************************************{be}\n"
            + "Please answer the following questions. If you ever need to change the "
            + "configuration, you can do that in the the esm_tests/user_config.yaml\n"
        )
        answers["account"] = input(
            "What account will you be using for testing? (default: None) "
        )
        if not answers["account"] or answers["account"] == "None":
            answers["account"] = None
        answers["test_dir"] = input(
            "In which directory would you like to run the tests? "
        )
        with open(user_config, "w") as uc:
            out = yaml.dump(answers)
            uc.write(out)

    # Load the user info
    with open(user_config, "r") as uc:
        user_info = yaml.load(uc, Loader=yaml.FullLoader)
    print(f"{bs}Running tests with the following configuration:{be}")
    print(f"{bs}-----------------------------------------------{be}")
    yprint(user_info)

    return user_info


def get_scripts(info):
    runscripts_dir = f"{info['script_dir']}/runscripts/"
    scripts_info = {}
    ns = 0
    # Load test info
    test_config = f"{info['script_dir']}/test_config.yaml"
    if os.path.isfile(test_config):
        with open(test_config, "r") as t:
            test_info = yaml.load(t, Loader=yaml.FullLoader)
    else:
        test_info = {}
    if len(test_info) > 0:
        test_all = False
    else:
        test_all = True
    for model in os.listdir(runscripts_dir):
        if test_all or test_info.get(model, False):
            # Check computer
            model_config = f"{runscripts_dir}/{model}/config.yaml"
            if not os.path.isfile(model_config):
                logger.error(f"'{model_config}' not found!")
            with open(model_config, "r") as c:
                config_test = yaml.load(c, Loader=yaml.FullLoader)
            computers = config_test.get("computers", False)
            if computers:
                if info["this_computer"] not in computers:
                    continue

            scripts_info[model] = {}
            for script in os.listdir(f"{runscripts_dir}/{model}"):
                if (
                    test_all
                    or isinstance(test_info.get(model), str)
                    or script in test_info.get(model, [])
                ) and os.path.isfile(f"{runscripts_dir}/{model}/{script}"):
                    if script != "config.yaml" and ".swp" not in script:
                        scripts_info[model][script.replace(".yaml", "")] = {}
                        scripts_info[model][script.replace(".yaml", "")][
                            "path"
                        ] = f"{runscripts_dir}/{model}/{script}"
                        scripts_info[model][script.replace(".yaml", "")]["state"] = {}
                        ns += 1
    scripts_info["general"] = {"num_scripts": ns}
    return scripts_info


def read_info_from_rs(scripts_info):
    new_loader = create_env_loader()
    for model, scripts in scripts_info.items():
        if model == "general":
            continue
        for script, v in scripts.items():
            with open(v["path"], "r") as rs:
                runscript = yaml.load(rs, Loader=yaml.SafeLoader)
            v["version"] = runscript[model]["version"]

    return scripts_info


def del_prev_tests(info, scripts_info):
    user_info = info["user"]
    logger.debug("Deleting previous tests")
    for model, scripts in scripts_info.items():
        if model == "general":
            continue
        for script, v in scripts.items():
            if os.path.isdir(
                f"{user_info['test_dir']}/comp/{model}/{model}-{v['version']}"
            ):
                shutil.rmtree(
                    f"{user_info['test_dir']}/comp/{model}/{model}-{v['version']}"
                )
                if len(os.listdir(f"{user_info['test_dir']}/comp/{model}")) == 0:
                    shutil.rmtree(f"{user_info['test_dir']}/comp/{model}")
            if os.path.isdir(f"{user_info['test_dir']}/run/{model}/{script}"):
                shutil.rmtree(f"{user_info['test_dir']}/run/{model}/{script}")
                if len(os.listdir(f"{user_info['test_dir']}/run/{model}")) == 0:
                    shutil.rmtree(f"{user_info['test_dir']}/run/{model}")


#######################################################################################
# FUNCTIONALITIES
#######################################################################################
def yprint(pdict):
    print(yaml.dump(pdict, default_flow_style=False))


def create_env_loader(tag="!ENV", loader=yaml.SafeLoader):
    # Necessary to ignore !ENV variables
    def constructor_env_variables(loader, node):
        return ""

    loader.add_constructor(tag, constructor_env_variables)
    return loader


def sh(inp_str, env_vars=[]):
    ev = ""
    for v in env_vars:
        ev += f"export {v}; "
    inp_str = f"{ev}{inp_str}"
    p = subprocess.Popen(
        inp_str, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=True
    )
    out = p.communicate()[0].decode("utf-8")
    return out


def deep_update(d, u):
    for k, v in u.items():
        if isinstance(v, collections.abc.Mapping):
            d[k] = deep_update(d.get(k, {}), v)
        else:
            d[k] = v

    return d


def copy_comp_files4check_runs(script, script_info, target_dir):
    files4check_dir = f"{os.path.dirname(script_info['path'])}/comp_files4check_runs/{script}"
    if os.path.isdir(files4check_dir):
        source_dir = f"{files4check_dir}/{os.listdir(files4check_dir)[0]}"
        combine_folders(source_dir, target_dir)


def combine_folders(source_dir, target_dir):
    if not os.path.isfile(target_dir):
        if os.path.isdir(source_dir):
            if not os.path.isdir(target_dir):
                os.mkdir(target_dir)
            for folder in os.listdir(source_dir):
                combine_folders(f"{source_dir}/{folder}", f"{target_dir}/{folder}")
        if os.path.isfile(source_dir):
            shutil.copy2(source_dir, target_dir)


#######################################################################################
# TESTS
#######################################################################################
def comp_test(scripts_info, info):
    cd_format = re.compile("         cd (.*)")
    user_info = info["user"]

    c = 0
    for model, scripts in scripts_info.items():
        if model == "general":
            continue
        for script, v in scripts.items():
            c += 1
            progress = round(c / scripts_info["general"]["num_scripts"] * 100, 1)
            version = v["version"]
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
                    # place there so that checks can run successfully without having to
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


def run_test(scripts_info, info):
    user_info = info["user"]
    actually_run = info["actually_run"]
    c = 0
    submitted = []
    # Loop through tests
    for model, scripts in scripts_info.items():
        if model == "general":
            continue
        for script, v in scripts.items():
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
            exp_dir_scripts = f"{exp_dir}/scripts/"
            for f in os.listdir(exp_dir_scripts):
                if "monitoring_file" in f and ".out" in f:
                    with open(f"{exp_dir_scripts}/{f}") as m:
                        monitoring_out = m.read()
                        if (
                            "Reached the end of the simulation, quitting"
                            in monitoring_out
                        ):
                            logger.info(
                                f"\tRUN FINISHED ({progress}%) {model}/{script}"
                            )
                            logger.info(f"\t\tSuccess!")
                            finished_runs.append(cc)
                            subc += 1
                            v["state"]["run_finished"] = True
                            success = check(
                                info, "run", model, version, "", script, v,
                            )
                        elif "ERROR:" in monitoring_out:
                            logger.info(
                                f"\tRUN FINISHED ({progress}%) {model}/{script}"
                            )
                            logger.error(f"\t\tSimulation crashed!")
                            finished_runs.append(cc)
                            subc += 1
                            v["state"]["run_finished"] = False
                            success = check(
                                info, "run", model, version, "", script, v,
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
    elif cfile in [".sad", "finished_config"]:
        files_to_folders = {".sad": "scripts", "finished_config": "config"}
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
        s_config_yaml, _ = get_rel_paths_compare_files(info, "finished_config", this_test_dir)
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
        namelists.extend(config[component].get("namelists", []))

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
        identical = False

    return identical, differences


def save_files(scripts_info, info, user_choice):
    actually_run = info["actually_run"]
    user_info = info["user"]
    last_tested_dir = info["last_tested_dir"]
    runscripts_dir = f"{info['script_dir']}/runscripts/"
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
    with open(f"{info['script_dir']}/state.yaml", "r") as st:
        current_state = yaml.load(st, Loader=yaml.FullLoader)
    # Update with this results
    results = format_results(info, scripts_info)
    current_state = deep_update(current_state, results)
    current_state = sort_dict(current_state)
    with open(f"{info['script_dir']}/state.yaml", "w") as st:
        state = yaml.dump(current_state)
        st.write(state)


def print_results(results):
    colorama.init(autoreset=True)

    print()
    print()
    print(f"{bs}RESULTS{be}")
    print()
    for model, versions in results.items():
        print(f"{colorama.Fore.CYAN}{model}:")
        for version, scripts in versions.items():
            print(f"    {colorama.Fore.MAGENTA}{version}:")
            for script, computers in scripts.items():
                print(f"        {colorama.Fore.WHITE}{script}:")
                for computer, data in computers.items():
                    if data["compilation"]:
                        compilation = f"{colorama.Fore.GREEN}compiles"
                    else:
                        compilation = f"{colorama.Fore.RED}compilation failed"
                    if data["run"]:
                        run = f"{colorama.Fore.GREEN}runs"
                    else:
                        run = f"{colorama.Fore.RED}run failed"
                    print(
                        f"            {colorama.Fore.WHITE}{computer}:\t{compilation}\t{run}"
                    )
    print()
    print()


def format_results(info, scripts_info):
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


#######################################################################################
# SCRIPT
#######################################################################################i

