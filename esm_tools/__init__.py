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
__version__ = "4.1.6"

import os
import sys

import pkg_resources
import yaml


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
    configdict = yaml.load(configstr, Loader=yaml.FullLoader)
    return configdict


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
    configstr = pkg_resources.resource_string("configs", config)
    configdict = yaml.load(configstr, Loader=yaml.FullLoader)
    return configdict


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
