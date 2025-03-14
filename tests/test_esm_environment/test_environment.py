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


SIMPLE_EXP_VARS = """
merge_component_envs:
    compiletime: False
    runtime: True
export_vars:
    VAR1: "1"
    VAR2: "2"
"""
SIMPLE_EXP_VARS = yaml.safe_load(SIMPLE_EXP_VARS)

EXP_VARS_WITH_DIFF_PROV = """
merge_component_envs:
    compiletime: False
    runtime: True
export_vars:
    VAR1: "1"
    VAR2: "2"
    VAR3: "3"
"""
EXP_VARS_WITH_DIFF_PROV = DictWithProvenance(yaml.safe_load(EXP_VARS_WITH_DIFF_PROV), {})
echam_provenance = Provenance({
    "category": "components",
    "yaml_file": "/dummy/path",
    "col": 1,
    "line": 6,
    "subcategory": "echam",
})
fesom_provenance = Provenance({
    "category": "components",
    "yaml_file": "/dummy/path",
    "col": 20,
    "line": 41,
    "subcategory": "fesom",
})
awiesm_provenance = Provenance({
    "category": "setups",
    "yaml_file": "/dummy/path",
    "col": 45,
    "line": 9,
    "subcategory": "awiesm",
})
EXP_VARS_WITH_DIFF_PROV["export_vars"]["VAR1"].provenance = echam_provenance
EXP_VARS_WITH_DIFF_PROV["export_vars"]["VAR2"].provenance = fesom_provenance
EXP_VARS_WITH_DIFF_PROV["export_vars"]["VAR3"].provenance = awiesm_provenance

MODULE_ACTIONS_WITH_DIFF_PROV = """
merge_component_envs:
    compiletime: False
    runtime: True
module_actions:
    - load intel-oneapi-compiler/2021.1.2
    - load netcdf/4.7.4
    - load python/3.8.5
"""
MODULE_ACTIONS_WITH_DIFF_PROV = DictWithProvenance(yaml.safe_load(MODULE_ACTIONS_WITH_DIFF_PROV), {})
MODULE_ACTIONS_WITH_DIFF_PROV["module_actions"][0].provenance = echam_provenance
MODULE_ACTIONS_WITH_DIFF_PROV["module_actions"][1].provenance = fesom_provenance
MODULE_ACTIONS_WITH_DIFF_PROV["module_actions"][2].provenance = awiesm_provenance

EXP_VARS_WITH_USER_SPEC = """
merge_component_envs:
    compiletime: False
    runtime: True
export_vars:
    VAR1: "1"
    VAR2:
        _value: "2"
        _run_or_compile: "compiletime"
        _component: "echam"
    VAR3:
        _value: "3"
        _component: "fesom"
"""

MODULE_ACTIONS_WITH_USER_SPEC = """
merge_component_envs:
    compiletime: False
    runtime: True
module_actions:
    - _value: "load intel-oneapi-compiler/2021.1.2"
      _run_or_compile: "compiletime"
      _component: "echam"
    - "load netcdf/4.7.4"
    - "load python/3.8.5"
"""

COMPLETE_CONFIG = """
    echam:
        include_env_from_component_files: True
"""

class FakeEnv(object):
    def __init__(self, env, run_or_compile, complete_config, model):
        self.config = env
        self.run_or_compile = run_or_compile
        self.complete_config = yaml.safe_load(complete_config)
        self.model = model

SIMPLE_EXP_VARS_OBJ = FakeEnv(SIMPLE_EXP_VARS, "compiletime", COMPLETE_CONFIG, "echam")
EXP_VARS_WITH_DIFF_PROV_OBJ = FakeEnv(EXP_VARS_WITH_DIFF_PROV, "compiletime", COMPLETE_CONFIG, "echam")
MODULE_ACTIONS_WITH_DIFF_PROV_OBJ = FakeEnv(MODULE_ACTIONS_WITH_DIFF_PROV, "compiletime", COMPLETE_CONFIG, "echam")
EXP_VARS_WITH_USER_SPEC_OBJ = FakeEnv(EXP_VARS_WITH_USER_SPEC, "compiletime", COMPLETE_CONFIG, "echam")

class Capturing(list):
    """Taken from https://stackoverflow.com/questions/16571150/how-to-capture-stdout-output-from-a-python-function-call"""

    def __enter__(self):
        self._stdout = sys.stdout
        sys.stdout = self._stringio = StringIO()
        return self

    def __exit__(self, *args):
        self.extend(self._stringio.getvalue().splitlines())
        del self._stringio  # free up some memory
        sys.stdout = self._stdout

class TestEnvironment(unittest.TestCase):
    """Tests for ``esm_environment``"""

    def setUp(self):
        """Prepare the test environment"""
        pass

    def test_simple_export_vars(self):
        """Test to check the simple export vars"""
        env = deepcopy(SIMPLE_EXP_VARS_OBJ)
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "export_vars")
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "export_vars")

        assert(env.config["export_vars"] == SIMPLE_EXP_VARS["export_vars"])

    def test_component_specific_envs(self):
        """Test to check the ignore of component-specific environments"""
        env = deepcopy(EXP_VARS_WITH_DIFF_PROV_OBJ)
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "export_vars")
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "export_vars")

        assert(env.config["export_vars"] == {
            "VAR1": "1",
            "VAR3": "3",
        })

    def test_merge_component_envs(self):
        """Test to check the merging of component-specific environments during compilation"""
        env = deepcopy(EXP_VARS_WITH_DIFF_PROV_OBJ)
        env.config["merge_component_envs"]["compiletime"] = True
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "export_vars")
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "export_vars")

        assert(env.config["export_vars"] == {
            "VAR1": "1",
            "VAR2": "2",
            "VAR3": "3",
        })

    def test_ignore_the_env_from_a_component_file_in_export_vars(self):
        """Test to check the ignore of a component-specific environment"""
        env = deepcopy(EXP_VARS_WITH_DIFF_PROV_OBJ)
        env.config["merge_component_envs"]["compiletime"] = True
        env.complete_config["echam"]["include_env_from_component_files"] = False
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "export_vars")
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "export_vars")

        assert(env.config["export_vars"] == {
            "VAR2": "2",
            "VAR3": "3",
        })

    def test_ignore_the_env_from_a_component_file_in_module_actions(self):
        """Test to check the ignore of a component-specific environment"""
        env = deepcopy(MODULE_ACTIONS_WITH_DIFF_PROV_OBJ)
        env.config["merge_component_envs"]["compiletime"] = True
        env.complete_config["echam"]["include_env_from_component_files"] = False
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "module_actions")
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "module_actions")

        assert(env.config["module_actions"] == [
            "load netcdf/4.7.4",
            "load python/3.8.5",
        ])

    def test_ignore_the_env_from_all_component_files(self):
        """Test to check the ignore of all component-specific environments"""
        env = deepcopy(EXP_VARS_WITH_DIFF_PROV_OBJ)
        env.config["merge_component_envs"]["compiletime"] = True
        env.complete_config["echam"]["include_env_from_component_files"] = False
        env.config["include_env_from_component_files"] = False
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "export_vars")
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "export_vars")

        assert(env.config["export_vars"] == {
            "VAR3": "3",
        })

    def test_user_specified_env_attributes_in_export_vars(self):
        """Test to check the selection of component-specific environments for compiling based on user specification"""
        env = deepcopy(EXP_VARS_WITH_USER_SPEC_OBJ)
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "export_vars")
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "export_vars")

        assert(env.config["export_vars"] == {
            "VAR1": "1",
            "VAR2": "2",
        })

    def test_user_specified_env_attributes_in_module_actions(self):
        """Test to check the selection of component-specific environments for compiling based on user specification"""
        env = deepcopy(MODULE_ACTIONS_WITH_USER_SPEC_OBJ)
        EnvironmentInfos.select_env_vars_based_on_provenance(env, "module_actions")
        EnvironmentInfos.select_env_vars_based_on_var_attributes(env, "module_actions")

        assert(env.config["module_actions"] == [
            "load intel-oneapi-compiler/2021.1.2",
            "load python/3.8.5",
        ])

    def tests_component_specific_environment_for_runtime_not_supported(self):
        """Test to check the component-specific environment for runtime not supported"""
        env = deepcopy(SIMPLE_EXP_VARS_OBJ)
        env.run_or_compile = "runtime"
        env.config["merge_component_envs"]["runtime"] = False

        error = []
        with Capturing() as output:
            try:
                EnvironmentInfos.select_env_vars_based_on_provenance(env, "export_vars")
            except SystemExit as e:
                error = e

        assert isinstance(error, SystemExit)
        assert any(["during runtime is not supported yet" in line for line in output])

# Sorting testing
