"""
When run from either the command line or in library mode (note **not** as an
ESM Plugin), ``esm_archiving`` can be configured to how it looks for specific
files. The configuration file is called ``esm_archiving_config``, should be
written in YAML, and have the following format::

    echam:  # The model name
        archive: # archive seperator **required**
            # Frequency specification (how often
            # a datestamp is generated to look for)
            frequency: "1M"
            # Date format specification
            date_format: "%Y%m"


By default, ``esm_archive`` looks in the following locations:

    1. Current working directory
    2. Any files in the XDG Standard:
        https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html


If nothing is found, the program reverts to the hard-coded defaults, found in
``esm_archiving/esm_archiving/config.py``

.. note::

    In future, it might be changed that the program will look for an experiment
    specific configuration based upon the path it is given during the
    ``create`` or ``upload`` step.

Generating a configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~

You can use the command line switches ``--write_local_config`` and
``--write_config`` to generate configuration files either in the current
working directory, or in the global directory for your user account defined by
the XDG standard (typically ~/.config/esm_archiving)::

    $ esm_archive --write_local_config
    Writing local (experiment) configuration...

    $ esm_archive --write_config
    Writing global (user) configuration...

"""

import logging
import os

import yaml
from xdgenvpy import XDGPedanticPackage

# Add XDG Standard
xdg = XDGPedanticPackage("esm_archiving")
config_dirs = xdg.XDG_CONFIG_DIRS.split(":")
config_dirs = [l + "/esm_archiving" for l in config_dirs]
config_dirs.insert(0, os.curdir)

# Defaults:
CONFIG_FNAME = "esm_archiving_config"
DEFAULT_CONFIG = {
    "echam": {"archive": {"frequency": "1M", "date_format": "%Y%m"}},
    "jsbach": {"archive": {"frequency": "1M", "date_format": "%Y%m"}},
    "hdmodel": {"archive": {"frequency": "1M", "date_format": "%Y%m"}},
    "fesom": {"archive": {"frequency": "1Y", "date_format": "%Y%m"}},
    "oasis3mct": {"archive": {"frequency": "1M", "date_format": "%Y%m"}},
}


def load_config(path=None):
    """
    Loads the configuration.

    If ``path`` is given, that file is loaded directly (used by the
    ``--config`` flag to select an alternate profile, e.g. a COSMOS profile
    whose component keys would otherwise collide with the default one). If it is
    not given, the default configuration directories are searched in order
    (CWD, then the XDG config dirs). If nothing can be found, the hard-coded
    default configuration is returned.

    Parameters
    ----------
    path : str, optional
        Explicit path to an ``esm_archiving_config`` file to load.

    Returns
    -------
    dict
        A representation of the configuration used for archiving.
    """
    if path is not None:
        with open(os.path.expanduser(path)) as source:
            return yaml.load(source, Loader=yaml.FullLoader)
    for loc in config_dirs:
        read_config_fname = CONFIG_FNAME
        try:
            with open(os.path.join(loc, read_config_fname)) as source:
                config = yaml.load(source, Loader=yaml.FullLoader)
                return config
        except IOError:
            pass
    return DEFAULT_CONFIG


def write_config_yaml(path=None):
    if not path:
        path = xdg.XDG_CONFIG_DIRS.split(":")[0] + "/esm_archiving"
    # [FIXME] PG: At some point, I'd like all paths to be ``Path``
    # [PROJECT] PG: path-refactor
    logging.debug(f"Opening {os.path.join(path, CONFIG_FNAME)} for writing...")
    with open(os.path.join(path, CONFIG_FNAME), "w") as config_file:
        logging.debug("...dumping...")
        yaml.dump(DEFAULT_CONFIG, config_file)
        logging.debug("...done!")
