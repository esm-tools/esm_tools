#!/usr/bin/env python
import pathlib

import dpath
from loguru import logger


def create_conda_env(config):
    build_location = dpath.util.get(config, "general/build_location")
    if build_location == "experiment":
        logger.info("Building conda in experiment folder")
        conda_prefix = pathlib.Path(dpath.util.get(config, "general/"))
    elif build_location == "home_folder":
        logger.info("Building in home folder")
    else:
        raise ValueError("Invalid build location")
