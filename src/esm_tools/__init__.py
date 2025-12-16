"""
=================================
ESM Tools (Package Documentation)
=================================

This package contains almost no code, but instead is where most of the YAML
configurations files are distributed. Default namelists are included as well as
example runscripts. This section **only** documents the code contained in the
module, please refer to the handbook for user documentation as well as API
documentation for the various sub-modules of the project.

Accessing Configuration
-----------------------

To access a particular configuration, you can use::

    >>> from esm_tools import read_config_file
    >>> ollie_config = read_config_file("machines/ollie")

Important note here is that the configuration file **has not yet been parsed**,
so it's just the dictionary representation of the YAML.
"""

__author__ = """Dirk Barbi, Paul Gierz"""
__email__ = "dirk.barbi@awi.de"
__version__ = "6.59.3"

import functools
import inspect
import operator
import os
import pathlib
import shutil
import site
import sys

import pkg_resources
import yaml
from loguru import logger

from .error_handling import *

# Setup Loguru for the following cases:
# A) If user sets
if os.environ.get("DEBUG_ESM_TOOLS"):
    logger.add(sys.stderr, filter={"": "WARNING", "esm_tools": "DEBUG"})
elif os.environ.get("CI"):
    logger.remove()
    logger.add(sys.stderr, filter={"": "WARNING"})
    logger.add(
        "out.log",
        filter={"": "WARNING", "esm_tools": "DEBUG"},
        backtrace=True,
        diagnose=True,
    )
elif os.environ.get("CI_VERBOSE"):
    logger.remove()
    logger.add(sys.stderr, filter={"": "WARNING", "esm_tools": "DEBUG"})
    logger.add(
        "out.log",
        filter={"": "WARNING", "esm_tools": "DEBUG"},
        backtrace=True,
        diagnose=True,
    )
else:
    logger.remove()


def caller_wrapper(func):
    @functools.wraps(func)
    def wrapped_func(*args, **kwargs):
        logger.debug(f"{inspect.stack()[1].function} --> {func.__name__}")
        return func(*args, **kwargs)

    return wrapped_func


def _transform(nested_list):
    """Transform irregular 2D list into a regular one."""
    regular_list = []
    for ele in nested_list:
        if type(ele) is list:
            regular_list.append(ele)
        else:
            regular_list.append([ele])
    return regular_list


def _get_venv_identifier():
    """
    Extract a stable identifier for the current virtual environment.

    In CI environments with concurrent job directories (e.g., /builds/project/000/,
    /builds/project/001/), the full VIRTUAL_ENV path changes between jobs even when
    using the same cached virtualenv. This function extracts a stable identifier
    that can be used to match site-packages paths regardless of the varying prefix.

    Configuration:
        ESM_TOOLS_VENV_IDENTIFIER: If set, use this value directly as the identifier.
            Example: ".direnv/python-3.9.2" or "venv"
        ESM_TOOLS_VENV_IDENTIFIER_DEPTH: Number of path components from the end of
            VIRTUAL_ENV to use as identifier. Default: 2 (e.g., ".direnv/python-3.9.2")

    Returns
    -------
    str or None
        The venv identifier string, or None if VIRTUAL_ENV is not set.
    """
    virtual_env_path = os.getenv("VIRTUAL_ENV")
    if not virtual_env_path:
        return None

    # Allow explicit override via environment variable
    explicit_identifier = os.getenv("ESM_TOOLS_VENV_IDENTIFIER")
    if explicit_identifier:
        logger.debug(f"Using explicit venv identifier: {explicit_identifier}")
        return explicit_identifier

    # Extract identifier from the last N components of VIRTUAL_ENV path
    # Default depth of 2 captures patterns like ".direnv/python-3.9.2" or "venv/lib"
    try:
        depth = int(os.getenv("ESM_TOOLS_VENV_IDENTIFIER_DEPTH", "2"))
    except ValueError:
        depth = 2

    path_parts = pathlib.Path(virtual_env_path).parts
    if len(path_parts) >= depth:
        identifier = str(pathlib.Path(*path_parts[-depth:]))
    else:
        # Fall back to just the last component if path is too short
        identifier = path_parts[-1] if path_parts else None

    logger.debug(f"Derived venv identifier from VIRTUAL_ENV: {identifier}")
    return identifier


def _filter_site_packages_for_venv(all_site_packages):
    """
    Filter site-packages directories to only include those belonging to the current venv.

    When running in a virtual environment, we want to search only the venv's
    site-packages, not the system site-packages. This is especially important in
    CI environments where cached venv paths may have a different prefix than the
    current working directory.

    Parameters
    ----------
    all_site_packages : list of str
        All discovered site-packages directories.

    Returns
    -------
    list of str
        Filtered list containing only venv site-packages, or the original list
        if no venv is active or no matches found.
    """
    venv_identifier = _get_venv_identifier()
    if not venv_identifier:
        logger.debug("No virtual environment detected, using all site-packages")
        return all_site_packages

    logger.debug(f"Filtering site-packages for venv identifier: {venv_identifier}")

    # Filter to site-packages paths that contain the venv identifier
    filtered = [
        site_dir for site_dir in all_site_packages
        if venv_identifier in site_dir
    ]

    if filtered:
        logger.debug(f"Filtered site-packages: {filtered}")
        return filtered

    # No matches found - this shouldn't happen in normal usage
    logger.warning(
        f"No site-packages matched venv identifier '{venv_identifier}'. "
        f"Searched: {all_site_packages}. Falling back to all site-packages."
    )
    return all_site_packages


@caller_wrapper
def _get_real_dir_from_pth_file(subfolder):
    """
    Resolve the actual filesystem path for package data in an editable install.

    When esm_tools is installed in editable/develop mode (pip install -e),
    package data like configs and namelists are not copied to site-packages.
    Instead, an .egg-link file points to the source directory. This function
    reads that link and resolves the actual path to the requested subfolder.

    Parameters
    ----------
    subfolder : str
        The package data subfolder to locate (e.g., 'configs', 'namelists').

    Returns
    -------
    pathlib.Path
        Resolved absolute path to the subfolder.

    Raises
    ------
    FileNotFoundError
        If the subfolder cannot be located in any site-packages directory.
    """
    logger.debug(f"Trying to resolve: {subfolder}")
    if subfolder.startswith("/"):
        logger.warning(f"Subfolder '{subfolder}' starts with '/' - this is unusual")

    # Gather all site-packages directories
    all_site_packages = functools.reduce(
        operator.iconcat,
        _transform([site.getusersitepackages(), site.getsitepackages()]),
        [],
    )
    logger.debug(f"All site-packages: {all_site_packages}")

    # Filter to venv site-packages if running in a virtual environment
    site_packages_to_search = _filter_site_packages_for_venv(all_site_packages)

    # Search for esm-tools.egg-link in each site-packages directory
    for site_packages_dir in site_packages_to_search:
        logger.debug(f"Searching in: {site_packages_dir}")

        egg_link_path = pathlib.Path(site_packages_dir) / "esm-tools.egg-link"
        if not egg_link_path.exists():
            continue

        # Parse the .egg-link file (contains source path and optional relative path)
        with open(egg_link_path, "r") as f:
            egg_link_lines = [line.strip() for line in f.readlines()]

        logger.debug(f"egg-link contents: {egg_link_lines}")

        # Try to construct the package data path
        if len(egg_link_lines) >= 2:
            source_dir = egg_link_lines[0]
            relative_path = egg_link_lines[1]
            candidate_path = pathlib.Path(source_dir) / relative_path / subfolder
        else:
            source_dir = egg_link_lines[0]
            candidate_path = pathlib.Path(source_dir) / subfolder

        resolved_path = candidate_path.resolve()
        logger.debug(f"Candidate path: {candidate_path} -> resolved: {resolved_path}")

        if resolved_path.exists():
            logger.debug(f"Found package data at: {resolved_path}")
            return resolved_path

        # Fallback: if relative_path looks like an absolute path, try it directly
        if len(egg_link_lines) >= 2 and egg_link_lines[1].startswith("/"):
            logger.debug(f"Trying {egg_link_lines[1]} as absolute path")
            fallback_path = (pathlib.Path(egg_link_lines[1]) / subfolder).resolve()
            if fallback_path.exists():
                logger.debug(f"Found package data at fallback: {fallback_path}")
                return fallback_path

    raise FileNotFoundError(
        f"Could not locate '{subfolder}' in esm-tools installation. "
        f"Searched site-packages: {site_packages_to_search}"
    )


def _get_namelist_filepath_standard_install(namelist):
    return pkg_resources.resource_filename("esm_tools.namelists", namelist)


def _get_namelist_filepath_editable_install(namelist):
    return f"{_get_real_dir_from_pth_file('namelists')}/{namelist}"


def _get_config_filepath_standard_install(config):
    return pkg_resources.resource_filename("esm_tools.configs", config)


def _get_config_filepath_editable_install(config):
    return f"{_get_real_dir_from_pth_file('configs')}/{config}"


def _get_runscript_filepath_standard_install(runscript):
    return pkg_resources.resource_filename("esm_tools.runscripts", runscript)


def _get_runscript_filepath_editable_install(runscript):
    return f"{_get_real_dir_from_pth_file('runscripts')}/{runscript}"


def _get_coupling_filepath_standard_install(coupling):
    return pkg_resources.resource_filename("esm_tools.couplings", coupling)


def _get_coupling_filepath_editable_install(coupling):
    return f"{_get_real_dir_from_pth_file('couplings')}/{coupling}"


def get_config_as_str(config):
    return pkg_resources.resource_string("esm_tools.configs", config)


def _list_config_dir_standard_install(dir_path):
    return pkg_resources.resource_listdir("esm_tools.configs", dir_path)


def _list_config_dir_editable_install(dir_path):
    return os.listdir(f"{_get_real_dir_from_pth_file('configs')}/{dir_path}")


# For more information on how this works, see here:
# https://stackoverflow.com/questions/62550952/including-package-data-python-from-top-level-when-package-is-in-subdirectory/62552188#62552188
def _read_config_standard_install(config):
    """
    Reads a config file for the case that you have done a standard pip install.

    Parameters
    ----------
    config : str
        The configuration to read, e.g. "machines/ollie.yaml"

    Returns
    -------
    dict :
        A dictionary representation of the configuration
    """
    if not config.endswith(".yaml"):
        config += ".yaml"
    configstr = pkg_resources.resource_string("esm_tools.configs", config)
    # configstr = pkg_resources.resource_string("configs", config)
    configdict = yaml.load(configstr, Loader=yaml.FullLoader)
    return configdict


def _read_namelist_standard_install(nml):
    """
    Reads a namelist file for the case that you have done a standard pip install.

    Parameters
    ----------
    nml : str
        The nml to read, e.g. "echam/6.3.04p2/PALEO/namelist.echam"

    Returns
    -------
    str :
        A string representation of the namelist file. This should later be
        passed to the f90nml package.
    """
    return pkg_resources.resource_string("esm_tools.namelists", nml)


def _read_config_editable_install(config):
    """
    Reads a config file for the case you have done an editable/develop install
    (e.g. pip -e).

    Parameters
    ----------
    dict :
        A dictionary representation of the configuration
    """
    if not config.endswith(".yaml"):
        config += ".yaml"
    # Note the only difference here is apparently swapping out
    # esm_tools.configs for just configs. Not sure how that works, but it seems
    # to be fine...
    with open(_get_config_filepath_editable_install(config), "r") as cfg:
        configstr = cfg.read()
    configdict = yaml.load(configstr, Loader=yaml.FullLoader)
    return configdict


def _copy_config_folder_standard_install(dest_path):
    src_path = pkg_resources.resource_filename("esm_tools.configs", ".")
    return shutil.copytree(src_path, dest_path)


def _copy_config_folder_editable_install(dest_path):
    src_path = _get_config_filepath_editable_install("")
    return shutil.copytree(src_path, dest_path)


def _copy_namelist_folder_standard_install(dest_path):
    src_path = pkg_resources.resource_filename("esm_tools.namelists", ".")
    return shutil.copytree(src_path, dest_path)


def _copy_namelist_folder_editable_install(dest_path):
    src_path = _get_namelist_filepath_editable_install("")
    return shutil.copytree(src_path, dest_path)


def _copy_runscript_folder_standard_install(dest_path):
    src_path = pkg_resources.resource_filename("esm_tools.runscripts", ".")
    return shutil.copytree(src_path, dest_path)


def _copy_runscript_folder_editable_install(dest_path):
    src_path = _get_runscript_filepath_editable_install("")
    return shutil.copytree(src_path, dest_path)


def _copy_coupling_folder_standard_install(dest_path):
    src_path = pkg_resources.resource_filename("esm_tools.couplings", ".")
    return shutil.copytree(src_path, dest_path)


def _copy_coupling_folder_editable_install(dest_path):
    src_path = _get_coupling_filepath_editable_install("")
    return shutil.copytree(src_path, dest_path)


def _read_namelist_editable_install(nml):
    """
    Reads a namelist file for the case that you have done an editable/develop install.

    Parameters
    ----------
    nml : str
        The nml to read, e.g. "echam/6.3.04p2/PALEO/namelist.echam"

    Returns
    -------
    str :
        A string representation of the namelist file. This should later be
        passed to the f90nml package.
    """
    with open(_get_namelist_filepath_editable_install("namelists", nml), "r") as nml:
        return nml.read()


# PG: Blatant theft:
# https://stackoverflow.com/questions/42582801/check-whether-a-python-package-has-been-installed-in-editable-egg-link-mode
def _dist_is_editable(dist):
    """Is distribution an editable install?"""
    for path_item in sys.path:
        egg_link = os.path.join(path_item, dist.replace("_", "-") + ".egg-link")
        if os.path.isfile(egg_link):
            return True
    return False


EDITABLE_INSTALL = _dist_is_editable("esm_tools")
"""
bool : Shows if the installation is installed in editable mode or not.
"""


# Create a function that "does the right thing", depending on how this module
# is installed
@caller_wrapper
def read_config_file(config):
    """
    Reads a configuration file, which should be seperated by "/". For example,
    "machines/ollie" will retrieve the (unparsed) configuration of the Ollie
    supercomputer.

    Parameters
    ----------
    config : str
        Configuration to get, e.g. machines/ollie.yaml, or echam/echam. You may
        omit the ".yaml" ending if you want, it will be appended automatically
        if not already there.

    Returns
    -------
    dict :
        A dictionary representation of the config.
    """
    if EDITABLE_INSTALL:
        return _read_config_editable_install(config)
    return _read_config_standard_install(config)


@caller_wrapper
def list_config_dir(dirpath):
    if EDITABLE_INSTALL:
        return _list_config_dir_editable_install(dirpath)
    return _list_config_dir_standard_install(dirpath)


@caller_wrapper
def copy_config_folder(dest_path):
    if EDITABLE_INSTALL:
        return _copy_config_folder_editable_install(dest_path)
    return _copy_config_folder_standard_install(dest_path)


@caller_wrapper
def copy_namelist_folder(dest_path):
    if EDITABLE_INSTALL:
        return _copy_namelist_folder_editable_install(dest_path)
    return _copy_namelist_folder_standard_install(dest_path)


@caller_wrapper
def copy_runscript_folder(dest_path):
    if EDITABLE_INSTALL:
        return _copy_runscript_folder_editable_install(dest_path)
    return _copy_runscript_folder_standard_install(dest_path)


@caller_wrapper
def get_namelist_filepath(namelist=""):
    if EDITABLE_INSTALL:
        return _get_namelist_filepath_editable_install(namelist)
    return _get_namelist_filepath_standard_install(namelist)


@caller_wrapper
def get_config_filepath(config=""):
    if EDITABLE_INSTALL:
        cpath = _get_config_filepath_editable_install(config)
    else:
        cpath = _get_config_filepath_standard_install(config)
    logger.debug(cpath)
    return cpath


@caller_wrapper
def get_runscript_filepath(runscript=""):
    if EDITABLE_INSTALL:
        return _get_runscript_filepath_editable_install(runscript)
    return _get_runscript_filepath_standard_install(runscript)


@caller_wrapper
def get_coupling_filepath(coupling=""):
    if EDITABLE_INSTALL:
        return _get_coupling_filepath_editable_install(coupling)
    return _get_coupling_filepath_standard_install(coupling)


@caller_wrapper
def read_namelist_file(nml):
    """Reads a namelist file from a path, seperated by "/". Similar to ``read_config_file``

    Parameters
    ----------
    nml : str
        The namelist to load

    Returns
    -------
    str :
        A string of the namelist file
    """
    if EDITABLE_INSTALL:
        return _read_namelist_editable_install(nml)
    return _read_namelist_standard_install(nml)
