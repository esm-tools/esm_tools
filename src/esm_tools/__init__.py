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

    >>> ollie_config = read_config_file("machines/ollie")

Important note here is that the configuration file **has not yet been parsed**,
so it's just the dictionary representation of the YAML.
"""

__author__ = """Dirk Barbi, Paul Gierz"""
__email__ = "dirk.barbi@awi.de"
__version__ = "2.1.23"

import os
import pathlib
import shutil
import site
import sys

import pkg_resources
import yaml


def _get_real_dir_from_pth_file(package):
    site_packages_dirs = [site.getusersitepackages(), site.getsitepackages()]
    for site_package_dir in site_packages_dirs:
        # Read the pth file:
        if pathlib.Path(f"{site_package_dir}/esm-tools.egg-link").exists():
            with open(f"{site_package_dir}/esm-tools.egg-link", "r") as f:
                paths = [p.strip() for p in f.readlines()]
            actual_package_data_dir = pathlib.Path(f"{paths[0]}/{paths[1]}/{package}/")
            return actual_package_data_dir.resolve()


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


def list_config_dir(dirpath):
    if EDITABLE_INSTALL:
        return _list_config_dir_editable_install(dirpath)
    return _list_config_dir_standard_install(dirpath)


def copy_config_folder(dest_path):
    if EDITABLE_INSTALL:
        return _copy_config_folder_editable_install(dest_path)
    return _copy_config_folder_standard_install(dest_path)


def copy_namelist_folder(dest_path):
    if EDITABLE_INSTALL:
        return _copy_namelist_folder_editable_install(dest_path)
    return _copy_namelist_folder_standard_install(dest_path)


def copy_runscript_folder(dest_path):
    if EDITABLE_INSTALL:
        return _copy_runscript_folder_editable_install(dest_path)
    return _copy_runscript_folder_standard_install(dest_path)


def get_namelist_filepath(namelist):
    if EDITABLE_INSTALL:
        return _get_namelist_filepath_editable_install(namelist)
    return _get_namelist_filepath_standard_install(namelist)


def get_config_filepath(config):
    if EDITABLE_INSTALL:
        cpath =  _get_config_filepath_editable_install(config)
    else:
        cpath = _get_config_filepath_standard_install(config)
    return cpath


def get_runscript_filepath(runscript):
    if EDITABLE_INSTALL:
        return _get_runscript_filepath_editable_install(runscript)
    return _get_runscript_filepath_standard_install(runscript)


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
