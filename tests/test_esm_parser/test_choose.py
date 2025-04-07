"""
Unit tests for the choose functionality
"""
import os
import pytest

from esm_parser import yaml_file_to_dict
from esm_runscripts import resolve_some_choose_blocks
from fixtures_choose import simple_choose_config, conflict_choose_config
from utils import Capturing


ESM_PARSER_TESTS_DIR = os.path.dirname(__file__)

class FakeConfig(dict):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._blackdict = self.get("_blackdict", {})
        if "_blackdict" in self:
            del self["_blackdict"]

def prepare_config(config):
    config = FakeConfig(config)

    return config


def test_simple_choose(simple_choose_config):
    """
    Test the most basic choose functionality
    """
    config = prepare_config(simple_choose_config)

    resolve_some_choose_blocks(config)

    assert config["general"]["version"] == "3.1.1"

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
    expected_config = yaml_file_to_dict(f"{ESM_PARSER_TESTS_DIR}/data/expected_reg_choose_1.yaml")
    config = prepare_config(config)

    resolve_some_choose_blocks(config)

    assert config == expected_config

# TODO:

# Test in comp-AWICM1-recom: NETCDF_CXX_ vars are removed from all
# models except for fesom because there is an specific one in fesom.
# Expected behaviour: keep the one from levante in the other models
