#!/usr/bin/env python

""" Tests for ``esm_environment``"""
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
import yaml

import esm_tools

from copy import deepcopy
from io import StringIO

from esm_parser.provenance import Provenance, DictWithProvenance, ListWithProvenance
from esm_environment import EnvironmentInfos
from utils import Capturing

SIMPLE_EXP_VARS = """
merge_component_envs:
    compile: False
    run: True
export_vars:
    VAR1: "1"
    VAR2: "2"
"""
SIMPLE_EXP_VARS = yaml.safe_load(SIMPLE_EXP_VARS)

EXP_VARS_WITH_DIFF_PROV = """
merge_component_envs:
    compile: False
    run: True
export_vars:
    VAR1: "1"
    VAR2: "2"
    VAR3: "3"
"""
EXP_VARS_WITH_DIFF_PROV = DictWithProvenance(
    yaml.safe_load(EXP_VARS_WITH_DIFF_PROV), {}
)
echam_provenance = Provenance(
    {
        "category": "components",
        "yaml_file": "/dummy/path",
        "col": 1,
        "line": 6,
        "subcategory": "echam",
    }
)
fesom_provenance = Provenance(
    {
        "category": "components",
        "yaml_file": "/dummy/path",
        "col": 20,
        "line": 41,
        "subcategory": "fesom",
    }
)
awiesm_provenance = Provenance(
    {
        "category": "setups",
        "yaml_file": "/dummy/path",
        "col": 45,
        "line": 9,
        "subcategory": "awiesm",
    }
)
EXP_VARS_WITH_DIFF_PROV["export_vars"]["VAR1"].provenance = echam_provenance
EXP_VARS_WITH_DIFF_PROV["export_vars"]["VAR2"].provenance = fesom_provenance
EXP_VARS_WITH_DIFF_PROV["export_vars"]["VAR3"].provenance = awiesm_provenance

MODULE_ACTIONS_WITH_DIFF_PROV = """
merge_component_envs:
    compile: False
    run: True
module_actions:
    - load intel-oneapi-compiler/2021.1.2
    - load netcdf/4.7.4
    - load python/3.8.5
"""
MODULE_ACTIONS_WITH_DIFF_PROV = DictWithProvenance(
    yaml.safe_load(MODULE_ACTIONS_WITH_DIFF_PROV), {}
)
MODULE_ACTIONS_WITH_DIFF_PROV["module_actions"][0].provenance = echam_provenance
MODULE_ACTIONS_WITH_DIFF_PROV["module_actions"][1].provenance = fesom_provenance
MODULE_ACTIONS_WITH_DIFF_PROV["module_actions"][2].provenance = awiesm_provenance

EXP_VARS_WITH_USER_SPEC = """
merge_component_envs:
    compile: False
    run: True
export_vars:
    VAR1: "1"
    VAR2:
        _value: "2"
        _execution_mode: "compile"
        _component: "echam"
    VAR3:
        _value: "3"
        _component: "fesom"
"""
EXP_VARS_WITH_USER_SPEC = DictWithProvenance(
    yaml.safe_load(EXP_VARS_WITH_USER_SPEC), {}
)

MODULE_ACTIONS_WITH_USER_SPEC = """
merge_component_envs:
    compile: False
    run: True
module_actions:
    - _value: "load intel-oneapi-compiler/2021.1.2"
      _execution_mode: "compile"
      _component: "fesom"
    - "load netcdf/4.7.4"
    - "load python/3.8.5"
"""
MODULE_ACTIONS_WITH_USER_SPEC = DictWithProvenance(
    yaml.safe_load(MODULE_ACTIONS_WITH_USER_SPEC), {}
)

COMPLETE_CONFIG = """
    echam:
        include_env_from_component_files: True
"""

DEPRECATED_CONFIG_YAML = """
general:
    name: awicm
    crazy:
    - recursion:
        compiletime_environment_changes:
            this_is_deprecated: "please remove"
echam:
    environment_changes:
        this_is_deprecated: "please remove"
fesom:
    choose_computer.name:
        levante:
            compiletime_environment_changes:
                this_is_deprecated: "please remove"
recom:
    choose_computer.name:
        levante:
            runtime_environment_changes:
                this_is_deprecated: "please remove"
computer:
    name: levante
"""
DEPRECATED_CONFIG = DictWithProvenance(yaml.safe_load(DEPRECATED_CONFIG_YAML), {})
DEPRECATED_CONFIG.set_provenance(awiesm_provenance)


def config_gen(computer, config=COMPLETE_CONFIG):
    config = DictWithProvenance(yaml.safe_load(config), {})
    config.update({"computer": computer})
    return config


class FakeEnv(object):
    def __init__(self, config, execution_mode, component):
        self.config = config
        self.computer = config["computer"]
        self.execution_mode = execution_mode
        self.component = component


FakeEnv._filter_env_vars = EnvironmentInfos._filter_env_vars
FakeEnv._flatten_values_with_attrs = EnvironmentInfos._flatten_values_with_attrs
FakeEnv.report_deprecated_environment_changes = (
    EnvironmentInfos.report_deprecated_environment_changes
)

SIMPLE_EXP_VARS_OBJ = FakeEnv(config_gen(SIMPLE_EXP_VARS), "compile", "echam")
EXP_VARS_WITH_DIFF_PROV_OBJ = FakeEnv(
    config_gen(EXP_VARS_WITH_DIFF_PROV), "compile", "echam"
)
MODULE_ACTIONS_WITH_DIFF_PROV_OBJ = FakeEnv(
    config_gen(MODULE_ACTIONS_WITH_DIFF_PROV), "compile", "echam"
)
EXP_VARS_WITH_USER_SPEC_OBJ = FakeEnv(
    config_gen(EXP_VARS_WITH_USER_SPEC), "compile", "echam"
)
MODULE_ACTIONS_WITH_USER_SPEC_OBJ = FakeEnv(
    config_gen(MODULE_ACTIONS_WITH_USER_SPEC), "compile", "echam"
)
DEPRECATED_CONFIG_OBJ = FakeEnv(DEPRECATED_CONFIG, "compile", "awiesm")


class TestEnvironment(unittest.TestCase):
    """Tests for ``esm_environment``"""

    def setUp(self):
        """Prepare the test environment"""
        pass

    def test_simple_export_vars(self):
        """Test to check the simple export vars"""
        env = deepcopy(SIMPLE_EXP_VARS_OBJ)
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "export_vars")
        EnvironmentInfos.remove_env_vars_from_component_files(env, "export_vars")
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "export_vars")

        assert env.computer["export_vars"] == SIMPLE_EXP_VARS["export_vars"]

    def test_component_specific_envs(self):
        """Test to check the ignore of component-specific environments"""
        env = deepcopy(EXP_VARS_WITH_DIFF_PROV_OBJ)
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "export_vars")
        EnvironmentInfos.remove_env_vars_from_component_files(env, "export_vars")
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "export_vars")

        assert env.computer["export_vars"] == {
            "VAR1": "1",
            "VAR3": "3",
        }

    def test_merge_component_envs(self):
        """Test to check the merging of component-specific environments during compilation"""
        env = deepcopy(EXP_VARS_WITH_DIFF_PROV_OBJ)
        env.computer["merge_component_envs"]["compile"] = True
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "export_vars")
        EnvironmentInfos.remove_env_vars_from_component_files(env, "export_vars")
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "export_vars")

        assert env.computer["export_vars"] == {
            "VAR1": "1",
            "VAR2": "2",
            "VAR3": "3",
        }

    def test_ignore_the_env_from_a_component_file_in_export_vars(self):
        """Test to check the ignore of a component-specific environment"""
        env = deepcopy(EXP_VARS_WITH_DIFF_PROV_OBJ)
        env.computer["merge_component_envs"]["compile"] = True
        env.config["echam"]["include_env_from_component_files"] = False
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "export_vars")
        EnvironmentInfos.remove_env_vars_from_component_files(env, "export_vars")
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "export_vars")

        assert env.computer["export_vars"] == {
            "VAR2": "2",
            "VAR3": "3",
        }

    def test_ignore_the_env_from_a_component_file_in_module_actions(self):
        """Test to check the ignore of a component-specific environment"""
        env = deepcopy(MODULE_ACTIONS_WITH_DIFF_PROV_OBJ)
        env.computer["merge_component_envs"]["compile"] = True
        env.config["echam"]["include_env_from_component_files"] = False
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "module_actions")
        EnvironmentInfos.remove_env_vars_from_component_files(env, "module_actions")
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "module_actions")

        assert env.computer["module_actions"] == [
            "load netcdf/4.7.4",
            "load python/3.8.5",
        ]

    def test_ignore_the_env_from_all_component_files(self):
        """Test to check the ignore of all component-specific environments"""
        env = deepcopy(EXP_VARS_WITH_DIFF_PROV_OBJ)
        env.computer["merge_component_envs"]["compile"] = True
        del env.config["echam"]["include_env_from_component_files"]
        env.computer["include_env_from_component_files"] = False
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "export_vars")
        EnvironmentInfos.remove_env_vars_from_component_files(env, "export_vars")
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "export_vars")

        assert env.computer["export_vars"] == {
            "VAR3": "3",
        }

    def test_user_specified_env_attributes_in_export_vars(self):
        """Test to check the selection of component-specific environments for compiling based on user specification"""
        env = deepcopy(EXP_VARS_WITH_USER_SPEC_OBJ)
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "export_vars")
        EnvironmentInfos.remove_env_vars_from_component_files(env, "export_vars")
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "export_vars")

        assert env.computer["export_vars"] == {
            "VAR1": "1",
            "VAR2": "2",
        }

    def test_user_specified_env_attributes_in_module_actions(self):
        """Test to check the selection of component-specific environments for compiling based on user specification"""
        env = deepcopy(MODULE_ACTIONS_WITH_USER_SPEC_OBJ)
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "module_actions")
        EnvironmentInfos.remove_env_vars_from_component_files(env, "module_actions")
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "module_actions")

        assert env.computer["module_actions"] == [
            "load netcdf/4.7.4",
            "load python/3.8.5",
        ]

    def test_component_specific_environment_for_run_not_supported(self):
        """Test to check the component-specific environment for run not supported"""
        env = deepcopy(SIMPLE_EXP_VARS_OBJ)
        env.execution_mode = "run"
        env.computer["merge_component_envs"]["run"] = False

        error = []
        with Capturing() as output:
            try:
                EnvironmentInfos.select_env_vars_based_on_provenance(env, "export_vars")
            except SystemExit as e:
                error = e

        assert isinstance(error, SystemExit)
        assert any(["during run is not supported yet" in line for line in output])

    # TODO: Sorting testing

    def test_report_deprecated_environment_changes(self):
        """Test to check the reporting of deprecated environment changes"""
        with Capturing() as output:
            try:
                DEPRECATED_CONFIG_OBJ.report_deprecated_environment_changes(
                    DEPRECATED_CONFIG_OBJ.config
                )
                error = None
            except SystemExit as e:
                error = e

        expected_error_line = (
            " - \x1b[31m/dummy/path\x1b[0m,line:\x1b[31m9\x1b[0m,col:\x1b[31m45\x1b[0m"
        )

        assert isinstance(error, SystemExit)
        assert output.count(expected_error_line) == 4
