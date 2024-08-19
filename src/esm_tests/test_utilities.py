import collections.abc
import glob
import os
import re
import shutil
import subprocess
import sys
import urllib
import urllib.request
import yaml

import esm_tests

from loguru import logger


def yprint(pdict):
    """
    Prints variables of a dictionary as a ``yaml.dump``, for pretty printing. Uses the
    ``logger`` from ``loguru`.

    Parameters
    ----------
    pdict : dict
        Dictionary to print
    """
    logger.info(yaml.dump(pdict, default_flow_style=False))


def create_env_loader(tag="!ENV", loader=yaml.SafeLoader):
    """
    Environment loader for ``yaml.load`` to ignore ``!ENV`` variables in the
    ``esm_tests`` methods.

    Returns
    -------
    loader : yaml.loader
    """

    def constructor_env_variables(loader, node):
        return ""

    loader.add_constructor(tag, constructor_env_variables)
    return loader


def sh(inp_str, env_vars=[], verbose=False):
    """
    Runs a ``shell`` command specified in ``inp_str``, exporting the environment vars
    in ``env_vars``.

    Parameters
    ----------
    inp_str : str
        String containing the ``shell`` command.
    env_vars : list
        List of the environment variables to be exported.

    Returns
    -------
    out : str
        The ``stdout`` and ``stderr`` of the ``shell`` command.
    """
    ev = ""
    # Add the exporting of the environment vars to the command
    for v in env_vars:
        ev += f"export {v}; "
    inp_str = f"{ev}{inp_str}"
    # Run the ``shell`` command
    p = subprocess.Popen(
        inp_str, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=True
    )
    out = p.communicate()[0].decode("utf-8")
    if verbose:
        print(inp_str)
        print(out)
    return out


def deep_update(d, u, extend_lists=False):
    """
    Recursively merge dictionary ``u`` into dictionary ``d``. Values of ``u`` win
    always over those of ``d``.

    Parameters
    ----------
    d : dict
        Dictionary in which the merge happens.
    u : dict
        Dictionary that updates ``d``.
    extend_lists : bool
        If ``True`` extend lists, if ``False`` rewrite them.

    Returns
    -------
    d : dict
        Updated dictionary.
    """
    for k, v in u.items():
        if isinstance(v, collections.abc.Mapping):
            d[k] = deep_update(d.get(k, {}), v)
        elif isinstance(v, list) and isinstance(d[k], list) and extend_lists:
            d[k].extend(v)
        else:
            d[k] = v

    return d


def copy_comp_files4check_runs(script, script_info, target_dir):
    """
    Gets files from the ``esm_test/resources/runscripts/<model>/comp_files4check_runs``
    so that, for example, ``namelists`` that are hosted in another repository could be
    placed there, to do check runs successfully without having to download the code.

    Parameters
    ----------
    script : str
        Runscript name.
    script_info : dict
        Dictionary containing the information about the runscript.
    target_dir : str
        Target directory path in which the files need to be copied.
    """
    files4check_dir = (
        f"{os.path.dirname(script_info['path'])}/comp_files4check_runs/{script}"
    )
    if os.path.isdir(files4check_dir):
        source_dir = f"{files4check_dir}/{os.listdir(files4check_dir)[0]}"
        combine_folders(source_dir, target_dir)


def combine_folders(source_dir, target_dir):
    """
    Recursively copies the files of the source directory into the target directory,
    including folders and subfolders.

    Parameters
    ----------
    source_dir : str
        Source directory path.
    target_dir : str
        Target directory path.
    """
    # Check that the target path is not a directory
    if not os.path.isfile(target_dir):
        # Check if the source path is a directory
        if os.path.isdir(source_dir):
            # If the target directory does not exist, creates it
            if not os.path.isdir(target_dir):
                os.mkdir(target_dir)
            # Loop through the items inside the source dir and recursively copy them
            # into the target dir
            for folder in os.listdir(source_dir):
                combine_folders(f"{source_dir}/{folder}", f"{target_dir}/{folder}")
        # Check if the source path is a file, it it is copy the file into the target
        if os.path.isfile(source_dir):
            shutil.copy2(source_dir, target_dir)


def clean_user_specific_info(info, str2clean):
    """
    Given a string, perform user- and computer-specific cleanups.

    Parameters
    ----------
    str2clean : str
        String to be cleaned

    Returns
    -------
    str2clean : str
        Cleaned string
    """

    this_path = os.getcwd()
    clean_str = str2clean

    # Transform into a list if necessary
    if isinstance(str2clean, str):
        clean_str = [clean_str]

    # Add the `/mnt/lustre.*/` string to clean
    mnt = ""
    if this_path.startswith("/mnt/lustre"):
        mnt = "/".join(this_path.split("/")[:3])

    # Do the cleaning
    new_clean_str = []
    for line in clean_str:
        for key, string in info["rm_user_info"].items():
            if not string:
                continue
            line = line.replace(f"{mnt}{string}", f"<{key}>")
            line = line.replace(string, f"<{key}>")
        new_clean_str.append(line)
    clean_str = new_clean_str

    if isinstance(str2clean, str):
        str2clean = clean_str[0]
    else:
        str2clean = clean_str

    return str2clean


def print_state_online(info={}):
    """
    Returns the state of the tested models obtained directly from the repository online.
    This method is aimed to be used externally from ``esm_tests`` (i.e. throw the
    ``esm_tools test-state`` command).

    Parameters
    ----------
    info : esm_tests.Info
        Info containing the testing info. In this case not all the keys are needed.
        If not provided, defines the ``info`` keys needed.
    """

    if "resources_branch" in info:
        resources_branch = info["resources_branch"]
    else:
        resources_branch = "release"
    url = f"https://raw.githubusercontent.com/esm-tools/esm_tests_info/{resources_branch}/state.yaml"
    try:
        current_state = urllib.request.urlopen(url)
    except urllib.error.HTTPError:
        print(
            f"HTTP Error: Connection to file {url} containing update messages could not be established"
        )
        print("    The test state cannot be reported")
        return

    if not info:
        info = {
            "in_github": False,
            "actually_compile": False,
            "actually_run": False,
        }

    # Logger
    logger.remove()
    logger.add(
        sys.stderr,
        filter={"": "WARNING", "esm_tests": "DEBUG"},
        format="<level>{message}</level>",
    )

    state = yaml.load(current_state, Loader=yaml.FullLoader)
    esm_tests.tests.print_results(state, info)


def sort_dict(dict_to_sort):
    """
    Sorts the keys of the ``dict_to_sort`` dictionary recursively.

    Parameters
    ----------
    dict_to_sort : dict
        Dictionary to be sorted.

    Returns
    -------
    dict_to_sort : dict
        Dictionary sorted.
    """
    if isinstance(dict_to_sort, dict):
        dict_to_sort = {str(key): value for key, value in dict_to_sort.items()}
        dict_to_sort = {key: dict_to_sort[key] for key in sorted(dict_to_sort.keys())}
        for key, value in dict_to_sort.items():
            dict_to_sort[key] = sort_dict(value)

    return dict_to_sort


def get_rel_paths_compare_files(info, cfile, v, this_test_dir):
    """
    Returns the relative paths of the files in ``last_tested`` the corresponding ones
    in the current experiment.

    Parameters
    ----------
    info : esm_tests.Info
        Dictionary that contains the testing info
    cfile : str
        Name of the current file
    v : dict
        Dictionary containing script variables such as ``version``, ``comp_command``, ...
    this_test_dir : str
        Path of the current experiment

    Returns
    -------
    subpaths_source : list
        Relative paths of the file in the current experiment
    subpaths_target : list
        Relative paths of the file in the ``last_tested`` folder
    """
    # File types to check
    some_compare_files = [".run", "finished_config"]
    # Load relevant variables from ``info``
    user_info = info["user"]
    # Initialize ``subpaths`` list
    subpaths = []

    # If it's not run in GitHub (but in an HPC) also check the prepcompute_filelist log
    if not info["in_github"]:
        some_compare_files.append("prepcompute_filelist")
    # If the file type is ``comp-``, append all the files that match that string to
    # ``subpaths``
    if cfile == "comp-":
        for f in os.listdir(f"{user_info['test_dir']}/{this_test_dir}"):
            if cfile in f:
                subpaths.append(f"{this_test_dir}/{f}")
        if len(subpaths) == 0:
            logger.error("\t\tNo 'comp-*.sh' file found!")
    # If the file type matches ``.run`` or ``finished_config``, append all the files
    # that match that string to ``subpaths``. These files are taken always from the
    # first ``run_`` folder so that a check test and an actual test files are
    # comparable
    elif cfile in some_compare_files:
        files_to_folders = {
            ".run": "scripts",
            "finished_config": "config",
            "prepcompute_filelist": "log",
        }
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
                if len(cfiles) == 0:
                    break
                if os.path.islink(
                    f"{user_info['test_dir']}/{cf_path}/{cfiles[num].split('/')[-1]}"
                ):
                    num = 1
                subpaths.append(f"{cf_path}/{cfiles[num].split('/')[-1]}")
                break
    elif cfile == "namelists":
        # Get path of the finished_config
        s_config_yaml, _ = get_rel_paths_compare_files(
            info, "finished_config", v, this_test_dir
        )
        if len(s_config_yaml) > 0:
            namelists, models = extract_namelists(
                f"{user_info['test_dir']}/{s_config_yaml[0]}"
            )
            ldir = os.listdir(f"{user_info['test_dir']}/{this_test_dir}")
            ldir.sort()
            for f in ldir:
                # Take the first run directory
                if "run_" in f:
                    cf_path = f"{this_test_dir}/{f}/work/"
                    for n, model in zip(namelists, models):
                        namelist_path = f"{user_info['test_dir']}/{cf_path}/{n}"
                        if not os.path.isfile(namelist_path):
                            logger.debug(f"'{cf_path}/{n}' does not exist!")
                        # Is broken link
                        if os.path.islink(namelist_path) and not os.path.exists(
                            os.readlink(namelist_path)
                        ):
                            path_in_general_config = (
                                f"{this_test_dir}/config/{model}/{n}_{f.split('_')[-1]}"
                            )
                            if os.path.exists(
                                f"{user_info['test_dir']}/{path_in_general_config}"
                            ):
                                subpaths.append(f"{path_in_general_config}")
                            else:
                                logger.debug(f"'{cf_path}/{n}' does not exist!")
                        else:
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
    """
    Searches for the names of the namelists in the ``s_config_yaml`` (i.e.
    ``*_finished_config.yaml``)

    Parameters
    ----------
    s_config_yaml : str
        Path to the ``*finished_config.yaml`` file.

    Returns
    -------
    namelists : list
        List of namelist names associated to this experiment.
    components : list
        List of components associated to the namelists (same order as ``namelists``).
    """
    # Read config file
    with open(s_config_yaml, "r") as c:
        config = yaml.load(c, Loader=yaml.FullLoader)

    namelists = []
    components = []
    # Loop through the components to find the namelists
    for component in config.keys():
        namelists_component = config[component].get("namelists", [])
        for nml in namelists_component:
            config_sources = config[component].get("config_sources", {})
            if nml in config_sources or any(
                [nml in x for x in config_sources.values()]
            ):
                namelists.append(nml)
                components.append(component)

    # Adds OASIS ``namcouple``
    if "oasis3mct" in config:
        namelists.append("namcouple")
        components.append("oasis")

    return namelists, components
