"""
Unit tests for the choose functionality
"""

import os

import pytest
from fixtures_choose import (
    choose_overwrites_default,
    conflict_choose_config,
    simple_choose_config,
)

from esm_parser import DictWithProvenance, yaml_file_to_dict
from esm_runscripts import resolve_some_choose_blocks
from utils import Capturing

ESM_PARSER_TESTS_DIR = os.path.dirname(__file__)


def prepare_config(config):
    if not isinstance(config, DictWithProvenance):
        config = DictWithProvenance(config, {})

    config._blackdict = config.get("_blackdict", {})
    if "_blackdict" in config:
        del config["_blackdict"]

    return config


def test_simple_choose(simple_choose_config):
    """
    Test the most basic choose functionality
    """
    config = prepare_config(simple_choose_config)

    resolve_some_choose_blocks(config)

    assert config["general"]["major_version"] == 3.1


def test_choose_overwrites_default(choose_overwrites_default):
    """
    Test the choose functionality overwriting default values
    """
    config = prepare_config(choose_overwrites_default)

    resolve_some_choose_blocks(config)

    assert config["general"]["version"] == 3.2


def test_simple_choose_with_from_choose(simple_choose_config):
    """
    Test the most basic choose functionality with from_choose in the provenance
    """
    config = prepare_config(simple_choose_config)

    resolve_some_choose_blocks(config)

    assert config["general"]["major_version"].provenance[-1]["from_choose"] == {
        "choose_key": "choose_general.version",
        "choice": "3.1.1",
    }


def test_detect_conflict_in_choose(conflict_choose_config):
    """
    Test  the detection of conflicts in choose blocks
    """
    config = prepare_config(conflict_choose_config)

    error = None
    with Capturing() as output:
        try:
            resolve_some_choose_blocks(config)
        except SystemExit as e:
            error = e

    assert isinstance(error, SystemExit)
    assert any(["ERROR: Choose conflict" in line for line in output])


# ---------------------------------
# REGRESSION TESTS
# ---------------------------------


def test_reg_choose_1():
    """
    Choose regresion test 1

    model: awicm-1.0
    type: choose block
    issue: -
    description: conflict between nested `choose_execution_mode` and
        `choose_computer.execution_mode`
    """
    config = yaml_file_to_dict(f"{ESM_PARSER_TESTS_DIR}/data/reg_choose_1.yaml")
    expected_config = yaml_file_to_dict(
        f"{ESM_PARSER_TESTS_DIR}/data/expected_reg_choose_1.yaml"
    )
    config = prepare_config(config)

    resolve_some_choose_blocks(config)

    assert config == expected_config


# TODO:

# Test in comp-AWICM1-recom: NETCDF_CXX_ vars are removed from all
# models except for fesom because there is an specific one in fesom.
# Expected behaviour: keep the one from levante in the other models
