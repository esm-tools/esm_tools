#!/usr/bin/env python3
# NOTE(PG): I hate how much duplication there is in this....

import os
import sys
import pkg_resources

import yaml


def _dist_is_editable(dist):
    """Is distribution an editable install?"""
    for path_item in sys.path:
        egg_link = os.path.join(path_item, dist.replace("_", "-") + ".egg-link")
        if os.path.isfile(egg_link):
            return True
    return False


EDITABLE_INSTALL = _dist_is_editable("esm_tests")
"""
bool : Shows if the installation is installed in editable mode or not.
"""


# State YAML:
def get_state_yaml():
    if EDITABLE_INSTALL:
        return yaml.safe_load(pkg_resources.resource_string("resources", "state.yaml"))
    return yaml.safe_load(
        pkg_resources.resource_string("esm_tests.resources", "state.yaml")
    )


# State YAML path:
def get_state_yaml_path():
    if EDITABLE_INSTALL:
        return pkg_resources.resource_filename("resources", f"state.yaml")
    return yaml.safe_load(
        pkg_resources.resource_filename("esm_tests.resources", "state.yaml")
    )


# Ignore Compare YAML:
def get_ignore_compare_yaml():
    if EDITABLE_INSTALL:
        return yaml.safe_load(
            pkg_resources.resource_string("resources", "ignore_compare.yaml")
        )
    return yaml.safe_load(
        pkg_resources.resource_string("esm_tests.resources", "ignore_compare.yaml")
    )


# Last Tested:
def get_last_tested(f):
    if EDITABLE_INSTALL:
        return pkg_resources.resource_string("resources", f"last_tested/{f}")
    return pkg_resources.resource_string("esm_tests.resources", f"last_tested/{f}")


# Last Tested dir:
def get_last_tested_dir():
    if EDITABLE_INSTALL:
        return pkg_resources.resource_filename("resources", f"last_tested/")
    return pkg_resources.resource_filename("esm_tests.resources", f"last_tested/")


# Runscripts:
def get_runscripts(f):
    if EDITABLE_INSTALL:
        return pkg_resources.resource_string("resources", f"runscripts/{f}")
    return pkg_resources.resource_string("esm_tests.resources", f"runscripts/{f}")


# Runscripts dir:
def get_runscripts_dir():
    if EDITABLE_INSTALL:
        return pkg_resources.resource_filename("resources", f"runscripts/")
    return pkg_resources.resource_filename("esm_tests.resources", f"runscripts/")
