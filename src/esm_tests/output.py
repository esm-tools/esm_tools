import copy
import difflib
import os
import re
import shutil
import time
import colorama
import yaml
from esm_runscripts import color_diff
from loguru import logger

from .read_shipped_data import *
from .test_utilities import *

# Bold strings
bs = "\033[1m"
be = "\033[0m"

# Define default files for comparisson
compare_files = {
    "comp": ["comp-"],
    "run": [".run", "finished_config", "namelists"],
}


def print_diff(info, sfile, tfile, name, ignore_lines):
    """
    Prints the differences between two equivalent configuration files.

    Parameters
    ----------
    info : esm_tests.Info
        Dictionary that contains the testing info.
    sfile : str
        Path to the source file.
    tfile : str
        Path to the target file.
    name : str
        Relative path to the file, for printing purposes.
    ignore_lines : list
        List of strings for lines containing them to be ignored in the comparison.
    """
    file_s = open(sfile).readlines()
    file_t = open(tfile).readlines()

    # Delete dictionaries to be ignored from the finished_config.yaml
    if "finished_config.yaml" in tfile or "filelist" in tfile:
        file_t = del_ignore_dicts(info, file_t)

    # Substitute user lines in target string
    file_t = clean_user_specific_info(info, file_t)

    # Check for ignored lines
    new_file_s = []
    for line in file_s:
        ignore_this = False
        for iline in ignore_lines:
            if re.search(iline, line):
                ignore_this = True
        if not ignore_this:
            new_file_s.append(line)
    file_s = new_file_s
    new_file_t = []
    for line in file_t:
        ignore_this = False
        for iline in ignore_lines:
            if re.search(iline, line):
                ignore_this = True
        if not ignore_this:
            new_file_t.append(line)
    file_t = new_file_t

    diffobj = difflib.SequenceMatcher(a=file_s, b=file_t)
    differences = ""
    if diffobj.ratio() == 1:
        logger.info(f"\t\t'{name}' files are identical")
        identical = True
    else:
        # Find differences
        pdifferences = ""
        for line in color_diff(difflib.unified_diff(file_s, file_t)):
            differences += line
            pdifferences += f"\t\t{line}"

        logger.info(f"\n\tDifferences in \x1b[35m{name}:\x1b[0m\n{pdifferences}\n")
        # input("Press enter to continue...")
        identical = False

    return identical, differences


def del_ignore_dicts(info, yaml_file):
    """
    Deletes the dictionary items specified in ``info["ignore"]["finished_config_dicts"]
    from the ``yaml_file``.

    Parameters
    ----------
    info : esm_tests.Info
        Dictionary that contains the testing info.
    yaml_file : list
        List of lines read from a yaml file.

    Returns
    -------
    new_yaml_files : list
        List of lines from the yaml file, without the ignored dict items.
    """
    new_yaml_file = []
    indentation_level_key = 0
    dict_key_found = False
    in_dict = False

    # Loop through lines
    for line in yaml_file:
        indentation_level = len(line) - len(line.lstrip(" "))
        # If the key was found on the previous iteration, check if this line is
        # indented inside. If true, remove that line.
        if dict_key_found:
            in_dict = indentation_level > indentation_level_key
            in_dict |= (
                indentation_level == indentation_level_key
                and line.replace(" ", "")[0:1] == "- "
            )
            if not in_dict:
                dict_key_found = False
        # Reset dict_key_in_line
        dict_key_in_line = False
        # Loop through the keys to be ignored and check if they are present on the line
        for ig_key in info["ignore"].get("finished_config_dicts", []):
            dict_key_in_line |= bool(re.search(f"{ig_key}:", line))
        # If the key is in the line, set ``dict_key_found`` as ``True`` and store the
        # ``indentation_level`` for use on the next line iteration. If the key is not
        # found append the line to the ``new_yaml_file``
        if dict_key_in_line:
            dict_key_found = True
            indentation_level_key = indentation_level
        elif in_dict:
            pass
        else:
            new_yaml_file.append(line)

    return new_yaml_file


def save_files(info, user_choice):
    """
    Saves files of the current tests into the ``resources/last_tested`` folder and
    updates the ``resources/state.yaml`` file with the test results. For all files,
    it also substitutes the user-specific information (e.g. home directory, ``base_dir``
    by a common string, so that comparison between files from different users don't
    display those differences. For the ``finished_config.yaml`` it also removes the
    dictionary items specified in ``info["ignore"]["finished_config_dicts"]``.

    Parameters
    ----------
    info : esm_tests.Info
        Dictionary that contains the testing info.
    user_choice : bool
        Indicates whether the user specified to save (``True``) or whether the user has
        not specify anything (``False``).
    """
    # Load relevant variables from ``info``
    scripts_info = info["scripts"]
    actually_run = info["actually_run"]
    user_info = info["user"]
    last_tested_dir = info["last_tested_dir"]
    runscripts_dir = get_runscripts_dir()
    this_computer = info["this_computer"]
    # ``user_choice`` is set through the ``--save`` flag. If the flag is not used the
    # main program reaches this point and asks the user about saving files in
    # ``last_tested``. If the flag is used alone or the value is set to ``True``, then
    # it does save without asking. If the flag is used and the value is set to ``False``
    # then it doesn't save and it doesn't ask
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
        # Ignore ``general``
        if model == "general":
            continue
        # Load the model's ``config.yaml``
        model_config = f"{runscripts_dir}/{model}/config.yaml"
        if not os.path.isfile(model_config):
            logger.error(f"'{model_config}' not found!")
        with open(model_config, "r") as c:
            config_test = yaml.load(c, Loader=yaml.FullLoader)
        # Get name patters from the files to be compared
        compare_files_comp = copy.deepcopy(compare_files["comp"])
        compare_files_comp.extend(
            config_test.get("comp", {}).get(test_type_c, {}).get("compare", [])
        )
        compare_files_run = copy.deepcopy(compare_files["run"])
        # If it's not run in GitHub (but in an HPC) also check the prepcompute_filelist log
        if not info["in_github"]:
            compare_files_run.append("prepcompute_filelist")
        # TODO: The iterative coupling needs a rework. Therefore, no testing for files
        # is develop. Include the tests after iterative coupling is reworked
        if next(iter(scripts.values()))["iterative_coupling"]:
            compare_files_run = []
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
                    save_exp_date(info, model, script)
                this_test_dir = f"{mode}/{model}/{subfolder}/"
                # Loop through comparefiles
                for cfile in this_compare_files:
                    subpaths_source, subpaths_target = get_rel_paths_compare_files(
                        info, cfile, v, this_test_dir
                    )
                    for sp, sp_t in zip(subpaths_source, subpaths_target):
                        target_path = f"{last_tested_dir}/{this_computer}/{sp_t}"
                        if os.path.isfile(target_path):
                            logger.debug(
                                f"\t'{sp_t}' file in '{last_tested_dir}/{this_computer}' will be overwritten"
                            )
                        if not os.path.isdir(os.path.dirname(target_path)):
                            os.makedirs(os.path.dirname(target_path))
                        shutil.copy2(f"{user_info['test_dir']}/{sp}", target_path)

                        # Modify lines containing user-specific information
                        with open(target_path) as f:
                            stext = clean_user_specific_info(info, f.read())
                        with open(target_path, "w") as f:
                            f.write(stext)

                        # If check run and file is the ``finished_config.yaml``
                        if "finished_config.yaml" in target_path:
                            with open(target_path) as f:
                                yaml_file = f.readlines()
                            # Delete dictionaries to be ignored
                            yaml_file = del_ignore_dicts(info, yaml_file)
                            with open(target_path, "w") as f:
                                f.write("".join(yaml_file))

    if info["actually_compile"] and info["actually_run"]:
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


def save_exp_date(info, model, script):
    """
    If the test is an actual run, store the date of the test in the ``info``.

    Parameters
    ----------
    info : esm_tests.Info
        Dictionary that contains the testing info.
    model : str
        Model name.
    script : str
        Script name.
    """
    user_info = info["user"]
    if info["actually_run"]:
        exp_time = os.path.getmtime(f"{user_info['test_dir']}/run/{model}/{script}")
        exp_human_time = time.strftime("%d-%m-%Y", time.gmtime(exp_time))
        this_script = info["scripts"][model][script]
        success = True
        this_script["date"] = exp_human_time


def print_results(results, info):
    """
    Prints colourful test results or the content of the ``state.yaml``. The following
    items in the ``info`` object allow for format control:

    - ``bulletpoints``: adds bulletpoints to the output so that can be copy/pasted
      into markdown texts (e.g. GitHub comment) and keep the structure.

    - ``in_github``: adds ``|`` at the beginning of lines starting with spaces, so that
      the output in GitHub Actions keeps its structure.

    Parameters
    ----------
    results : dict
        Dictionary including the results of the test to be printed.
    info : esm_tests.Info
        Dictionary that contains the testing info.
    """
    colorama.init(autoreset=True)

    if info.get("bulletpoints", False):
        bp = "- "
    else:
        bp = ""

    if info["in_github"]:
        first_tab = "|   "
    else:
        first_tab = "    "

    logger.info("")
    logger.info("")
    logger.info(f"{bs}RESULTS{be}")
    logger.info("")
    for model, versions in results.items():
        logger.info(f"{bp}{colorama.Fore.CYAN}{model}:")
        for version, scripts in versions.items():
            logger.info(f"{first_tab}{bp}{colorama.Fore.MAGENTA}{version}:")
            for script, computers in scripts.items():
                logger.info(f"{first_tab}    {bp}{colorama.Fore.WHITE}{script}:")
                for computer, data in computers.items():
                    text = data["compilation"]
                    if "compiles" == text:
                        text_color = colorama.Fore.GREEN
                    elif "identical" in text and not info["actually_compile"]:
                        text_color = colorama.Fore.GREEN
                    elif "differ" in text:
                        text_color = colorama.Fore.YELLOW
                    else:
                        text_color = colorama.Fore.RED
                    compilation = f"{text_color}{text}"
                    text = data["run"]
                    if "runs" == text:
                        text_color = colorama.Fore.GREEN
                    elif "identical" in text and not info["actually_run"]:
                        text_color = colorama.Fore.GREEN
                    elif "differ" in text:
                        text_color = colorama.Fore.YELLOW
                    else:
                        text_color = colorama.Fore.RED
                    run = f"{text_color}{text}"
                    if data.get("date"):
                        text_color = colorama.Fore.WHITE
                        date = f"\t{text_color}{data['date']}"
                    else:
                        date = ""
                    logger.info(
                        f"{first_tab}        {bp}{colorama.Fore.WHITE}{computer}:\t{compilation}\t{run}{date}"
                    )
    logger.info(f"{colorama.Fore.WHITE}")
    logger.info("")


def format_results(info):
    """
    Takes the info about the tests from ``info`` and builds the ``results`` dictionary,
    taking into account whether the tests are checks or actual comps/runs, and the
    results of every step. Each script is then assign a string that describes the state
    of the last process for that experiment. If previous steps are not passed, it
    reports the first unsuccessful step.

    Parameters
    ----------
    info : esm_tests.Info
        Dictionary that contains the testing info.
    results : dict
        Dictionary containing the state of the first unsuccessful step of a given test,
        or the latest successful step if all steps were successful. The structure of
        this ``dict`` is::

          <model>:
              <version>:
                  <computer>:
                      comp: "compilation state"
                      run: "run state"
    """
    # Load current state
    with open(get_state_yaml_path(), "r") as st:
        current_state = yaml.load(st, Loader=yaml.FullLoader)

    scripts_info = info["scripts"]
    results = {}
    for model, scripts in scripts_info.items():
        if model == "general":
            continue
        results[model] = {}
        for script, v in scripts.items():
            version = str(v["version"])
            results[model][version] = results[model].get(version, {})
            results[model][version][script] = results[model][version].get(script, {})
            state = v["state"]

            if info["actually_compile"]:
                compilation = "compiles"
            else:
                compilation = "comp files identical"

            if not state.get("comp_files_identical", True):
                compilation = "comp files differ"
            if not state.get("comp_files", True):
                compilation = "comp files not found"
            if not state["comp"]:
                compilation = "compilation failed"

            if info["actually_run"]:
                run = "runs"
            else:
                run = "run files identical"

            if not state.get("submission_files_identical", True):
                run = "run files differ"
            if not state["submission"]:
                run = "submission failed"
            if not state.get("run_files", True):
                run = "run files not found"
            if not state.get("run_finished", True):
                run = "run failed"

            date = info["scripts"][model][script].get("date")

            results[model][version][script][info["this_computer"]] = {
                "compilation": compilation,
                "run": run,
                "date": date,
            }

    return results
