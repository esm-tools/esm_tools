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
__version__ = "6.0.4"

import functools
import inspect
import operator
import os
import pathlib
import shutil
import site
import sys

from loguru import logger
import pkg_resources
import yaml


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


@caller_wrapper
def _get_real_dir_from_pth_file(subfolder):
    logger.debug(f"Trying to resolve: {subfolder}")
    if subfolder.startswith("/"):
        logger.warning("Subfolder is strange!")
        logger.warning(subfolder)
    site_packages_dirs = functools.reduce(
        operator.iconcat,
        _transform([site.getusersitepackages(), site.getsitepackages()]),
        [],
    )
    logger.debug(site_packages_dirs)
    for site_package_dir in site_packages_dirs:
        logger.debug(f"Working on {site_package_dir}")
        # Read the pth file:
        if pathlib.Path(f"{site_package_dir}/esm-tools.egg-link").exists():
            with open(f"{site_package_dir}/esm-tools.egg-link", "r") as f:
                paths = [p.strip() for p in f.readlines()]
            # NOTE(PG): a pathlib.Path has a method resolve, which removes
            # things like "foo/baz/../bar" in the path to "foo/bar"
            logger.debug(f"paths={paths}")
            logger.debug(f"subfolder={subfolder}")
            actual_package_data_dir = pathlib.Path(
                f"{paths[0]}/{paths[1]}/{subfolder}/"
            )
            logger.debug("Before resolve:")
            logger.debug(actual_package_data_dir)
            logger.debug("After resolve:")
            actual_package_data_dir = actual_package_data_dir.resolve()
            logger.debug(actual_package_data_dir)
            try:
                assert actual_package_data_dir.exists()
            # NOTE(PG): there is probably a better way of doing that than with assert.
            except AssertionError as e:
                logger.debug(
                    f"Assumed path {actual_package_data_dir} did not exist! We tried:"
                )
                logger.debug(f"paths[0]={paths[0]}")
                logger.debug(f"paths[1]={paths[1]}")
                if paths[1].startswith("/"):
                    logger.debug(
                        f"{paths[1]} starts with a slash, assuming absolute path!"
                    )
                    actual_package_data_dir = pathlib.Path(
                        f"{paths[1]}/{subfolder}/"
                    ).resolve()
                    try:
                        assert actual_package_data_dir.exists()
                    except AssertionError as e:
                        logger.error("Could not determine path!")
                        break  # Break out of the for loop
            logger.debug(f"actual_package_data_dir={actual_package_data_dir}")
            return actual_package_data_dir
    raise FileNotFoundError(
        f"Could not determine where {subfolder}'s path is inside the esm-tools installation! These were searched for info: {site_packages_dirs}"
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
def get_namelist_filepath(namelist):
    if EDITABLE_INSTALL:
        return _get_namelist_filepath_editable_install(namelist)
    return _get_namelist_filepath_standard_install(namelist)


@caller_wrapper
def get_config_filepath(config):
    if EDITABLE_INSTALL:
        cpath = _get_config_filepath_editable_install(config)
    else:
        cpath = _get_config_filepath_standard_install(config)
    return cpath


@caller_wrapper
def get_runscript_filepath(runscript):
    if EDITABLE_INSTALL:
        return _get_runscript_filepath_editable_install(runscript)
    return _get_runscript_filepath_standard_install(runscript)


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
