import collections.abc
import os
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
        mnt =  "/".join(this_path.split("/")[:3])

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
    ``esm_tools --test-state`` command).

    Parameters
    ----------
    info : dict
        Info containing the testing info. In this case not all the keys are needed.
        If not provided, defines the ``info`` keys needed.
    """

    url = "https://raw.githubusercontent.com/esm-tools/esm_tests_info/main/state.yaml"
    try:
        current_state = urllib.request.urlopen(url)
    except urllib.error.HTTPError:
        print(f"HTTP Error: Connection to file {url} containing update messages could not be established")
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
