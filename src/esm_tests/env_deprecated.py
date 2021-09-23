# MA: This script is not used anymore, it was made on the first place to test the
# environments when I did the refactorization of esm_environments. esm_tests took
# inspiration from some of the functions here, and I decided to keep it here
# just for the record.

import os
import argparse
import subprocess
import re
import shutil
from loguru import logger
import difflib
import yaml
import time
from esm_runscripts import color_diff

def compile_run_all(dir_name, esm_elements, option):

    # Create directory
    if "stable" in dir_name:
        if os.path.isdir(dir_name):
            logger.debug(f"{dir_name} already exists. Avoiding to overwrite.")
            return
    else:
        if os.path.isdir(dir_name):
            delete = input(f"{dir_name} already exists, do you want to delete it and calculate it again (y/n)? ")
            if delete is "y":
                shutil.rmtree(dir_name)
            else:
                logger.debug(f"{dir_name} already exists. Avoiding to overwrite.")
                return
    os.mkdir(dir_name)

    # Change directory
    top_dir = os.getcwd()
    os.chdir(dir_name)

    # Count total length for timebar
    niter = 0
    for element, versions in esm_elements.items():
        for version in versions:
            niter += 1

    logger.info(f"{dir_name}:")

    # Loop through the different model/setups versions producing the compilation scripts
    c = 0
    rep = 50
    for element, versions in esm_elements.items():
        for version in versions:
            c += 1

            if option=="compile":
                comp_specific(element, version)
            elif option=="run":
                run_specific(element, version)
            else:
                logger.error(f"Option {option} not valid!")

            run = c/niter
            print('|' + u'\u2588' * round(rep*run) + ' ' * round(rep*(1-run)) + '| ' + str(round(run*1000)/10) + '%   ' + element + version + ' '*20, end="\r")

    os.chdir(top_dir)


def comp_specific(element, version):
    cd_format = re.compile("         cd (.*)")
    if version != "":
        version = "-" + version
    # Evaluate and create folders to cheat esm_master
    out = sh("esm_master comp-" + element + version + " -c")
    folders = []
    for line in out.split("\n"):
        if "cd" in line and "cd .." not in line:
            found_format = cd_format.findall(line)
            if len(found_format) > 0:
                if ";" not in found_format[0] and "/" not in found_format[0]:
                    folders.append(found_format[0])
    if len(folders)==0:
        logger.warning(f'NOT TESTING {element + version}: "cd" command not found')
        return
    prim_f = folders[0]
    folders.append(element + version)
    folders = [x for x in set(folders)]
    if os.path.isdir(prim_f):
        shutil.move(prim_f, prim_f + "_bckp")
    os.mkdir(prim_f)
    for folder in folders:
        os.mkdir(prim_f + "/" + folder)

    # Run the compilation
    out = sh("esm_master comp-" + element + version)

    # Comp files handling
    lfiles = os.listdir()
    file_counter = 0
    for f in lfiles:
        if f.startswith("comp-"):
            if os.path.isfile(prim_f + "/" + f):
                logger.debug(f"{prim_f + '/' + f} already existed. REMOVED and copied again!")
                os.remove(prim_f + "/" + f)
            shutil.move(f, prim_f)
            file_counter += 1

    if file_counter == 0:
        logger.error(f'NOT TESTING {element + version}: no "comp" files')

    # Move directory to have the correct name
    shutil.move(prim_f, element + version)
    if os.path.isdir(prim_f + "_bckp"):
        shutil.move(prim_f + "_bckp", prim_f)


def run_specific(element, version):

    vname = "-" + version
    if len(version) == 0:
        vname = ""

    # Prepare runscript
    runscript = {
        "general": {
            "setup_name": element,
            "version": version,
            "compute_time": "03:00:00",
            "initial_date": "2000-01-01",
            "final_date": "2002-01-01",
            "base_dir": "/work/ollie/mandresm/esm_yaml_test/",
            "account": None,
        },
        element: {
            "model_dir": "nowhere",
        }
    }
    # Prepare the yaml file
    with open("generic_runscript.yaml", "w") as runscript_file:
        yaml.dump(runscript, runscript_file, default_flow_style=False)

    # Run checks
    this_dir = os.getcwd()
    out = sh(f"esm_runscripts generic_runscript.yaml -e env_testing -c -v --open-run -U")
    if os.path.isdir("/work/ollie/mandresm/esm_yaml_test/env_testing/scripts"):
        os.chdir("/work/ollie/mandresm/esm_yaml_test/env_testing/scripts")
    else:
        logger.error(f"{element}{vname} check run was not prepared successfully")
        return
    out = sh("esm_runscripts generic_runscript.yaml -e env_testing -c -v --open-run -U")
    os.chdir(this_dir)

    # File copying and cleanup
    os.mkdir(f"{element}{vname}")
    test_path = "/work/ollie/mandresm/esm_yaml_test/env_testing"
    sad_path = test_path + "/run_20000101-20001231/scripts/env_testing_compute_20000101-20001231.sad"
    if not os.path.isfile(sad_path):
        logger.error(f"{element}{vname} sad file was not generated properly")
    else:
        shutil.copy(sad_path, f"{element}{vname}/run-{element}{vname}")
    shutil.rmtree(test_path)


def sh(inp_str):
    p = subprocess.Popen(inp_str.split(" "), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out = p.communicate()[0].decode('utf-8')
    return out


def extract_esm_elements():
    # Extract model/setup names and versions
    out = sh("esm_master")
    out_split_comp = out.split("components:")
    comp_str = out_split_comp[-1]
    setup_str = out_split_comp[0].split("setups:")[-1]
    all_esm = comp_str.split("\n") + setup_str.split("\n")
    esm_elements = {}
    name_format = re.compile("    (.*?):")
    version_format = re.compile("       (.*?):")
    for line in all_esm:
        if len(line) > 4:
            if line[3] == " " and line[4] != " ":
                el_name = name_format.findall(line)[0]
                after_col = line.replace(" ", "")
                if len(after_col) > 0:
                    after_col = after_col.split(":")[1:]
                    if len(after_col) > 1:
                        esm_elements[el_name] = [after_col[0]]
                    elif len(after_col[0]) > 0:
                        esm_elements[el_name] = [""]
                    else:
                        esm_elements[el_name] = []
                else:
                    esm_elements[el_name] = []
            elif len(line) > 7:
                if line[6] == " " and line[7] != " ":
                    version = version_format.findall(line)[0]
                    esm_elements[el_name].append(version)
    return esm_elements


def compare_scripts(stable, test):
    # List all stable folders
    folders = os.listdir(stable)

    # Loop through the elements
    counter = 0
    total = len(folders)
    for element in folders:
        sta = stable + "/" + element
        tes = test + "/" + element
        if not os.path.isdir(tes):
            logger.error(f"{tes} does not exist!")
            continue
        files = os.listdir(sta)
        comps = []
        for f in files:
            if "comp-" in f or "run-" in f:
                comps.append(f)

        logger.info("\n" + len(element)*"=" + f"\n{element}\n" + len(element)*"=" + "\n")
        for comp in comps:
            logger.info(f"\n{comp}\n" + len(comp)*"-" + "\n")
            comp_s = sta + "/" + comp
            comp_t = tes + "/" + comp
            if not os.path.isfile(comp_t):
                logger.error(f"{comp_t} does not exist in {test}!")
                continue
            print_diff(comp_s, comp_t, comp)
        counter += 1
        cont = input(f"Press enter to continue... {str(round(counter/total*1000)/10)}%")


def print_diff(sscript, tscript, name):
    script_s = open(sscript).readlines()
    script_t = open(tscript).readlines()

    diffobj = difflib.SequenceMatcher(a=script_s, b=script_t)
    if diffobj.ratio() == 1:
        logger.info(f"{name} files are identical")
    else:
        differences = ""
        # Find differences
        for line in color_diff(difflib.unified_diff(script_s, script_t)):
            differences += line

        logger.info("Differences:\n" + differences + "\n")


def change_version(version, version_yaml):
    with open(version_yaml, "r") as yaml_file:
        yaml_load = yaml.load(yaml_file, Loader=yaml.FullLoader)
    if not version in yaml_load:
        logger.debug(f"{version} is not in {version_yaml}!")
        sys.exit(-1)

    current_dir = os.getcwd()
    for package, branch in yaml_load[version].items():
        logger.info(f"Installing {branch} for {package}...")
        os.chdir(f"{os.path.expanduser('~')}/{package}")
        sh(f"git checkout {branch}")
        sh("git pull")

    os.chdir(current_dir)


def change_computer(option, version_yaml):
    with open(version_yaml, "r") as yaml_file:
        yaml_load = yaml.load(yaml_file, Loader=yaml.FullLoader)
    computer = yaml_load.get("computer", False)
    if not computer:
        return

    all_machines_path = f"{os.path.expanduser('~')}/esm_tools/configs/machines/all_machines.yaml"

    if option == "change":
        with open(all_machines_path, "r") as machines_file:
            all_machines = yaml.load(machines_file, Loader=yaml.FullLoader)
        this_computer = {computer: all_machines["ollie"]}
        with open(all_machines_path, "w") as machines_file:
            yaml.dump(this_computer, machines_file, default_flow_style=False)
    elif option == "undo":
        this_dir = os.getcwd()
        os.chdir("/".join(all_machines_path.split("/")[:-1]))
        sh("git checkout all_machines.yaml")
        os.chdir(this_dir)


# Get arguments
parser = argparse.ArgumentParser(description="Tests compilation and runscripts")
parser.add_argument("option", default=None)
parser.add_argument("version_yaml", default=None)

args = vars(parser.parse_args())

# Option
option = args["option"]
version_yaml = args["version_yaml"]

# Logging
if os.path.isfile(f"esm_test_{option}.log"):
    os.remove(f"esm_test_{option}.log")
logger.add(f"esm_test_{option}.log", format="{time} {level} {message}")

if option=="compile" or option=="run":
    if option=="compile":
        folder_name = "comp"
    else:
        folder_name = "run"
    # Extract model/setup and version info
    esm_elements = extract_esm_elements()

    # Change to stable version
    change_computer("undo", version_yaml)
    change_version("stable", version_yaml)
    change_computer("change", version_yaml)

    # Compile/run
    compile_run_all(f"{folder_name}_stable", esm_elements, option)

    # Change to new version
    change_computer("undo", version_yaml)
    change_version("test", version_yaml)
    change_computer("change", version_yaml)

    # Compile/run
    compile_run_all(f"{folder_name}_test", esm_elements, option)

    # Print differences
    compare_scripts(f"{folder_name}_stable", f"{folder_name}_test")

elif option=="diff":
    # Print differences
    compare_scripts(f"{version_yaml}_stable", f"{version_yaml}_test")

else:
    print(f'"{option}" is not an option for testing')
