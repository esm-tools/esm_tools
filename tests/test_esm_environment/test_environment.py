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

from esm_parser.provenance import Provenance, DictWithProvenance, ListWithProvenance


SIMPLE_EXP_VARS_YAML = """
export_vars:
    VAR1: "1"
    VAR2: "2"
"""

EXP_VARS_WITH_DIFF_PROV = """
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
    "component": "echam",
})
fesom_provenance = Provenance({
    "category": "components",
    "yaml_file": "/dummy/path",
    "col": 20,
    "line": 41,
    "component": "fesom",
})
awiesm_provenance = Provenance({
    "category": "components",
    "yaml_file": "/dummy/path",
    "col": 45,
    "line": 9,
    "component": "awiesm",
})
EXP_VARS_WITH_DIFF_PROV["export_vars"]["VAR1"].provenance = echam_provenance
EXP_VARS_WITH_DIFF_PROV["export_vars"]["VAR2"].provenance = fesom_provenance
EXP_VARS_WITH_DIFF_PROV["export_vars"]["VAR3"].provenance = awiesm_provenance

MODULE_ACTIONS_WITH_DIFF_PROV = """
module_actions:
    - load intel-oneapi-compiler/2021.1.2
    - load netcdf/4.7.4
    - load python/3.8.5
"""
MODULE_ACTIONS_WITH_DIFF_PROV = ListWithProvenance(yaml.safe_load(MODULE_ACTIONS_WITH_DIFF_PROV), {})
MODULE_ACTIONS_WITH_DIFF_PROV["module_actions"][0].provenance = echam_provenance
MODULE_ACTIONS_WITH_DIFF_PROV["module_actions"][1].provenance = fesom_provenance
MODULE_ACTIONS_WITH_DIFF_PROV["module_actions"][2].provenance = awiesm_provenance

EXP_VARS_WITH_USER_SPEC = """
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

# Test to check the ignore of component-specific environments

# Test to check the selection of component-specific environments for compiling based on provenance

# Test to check the selection of component-specific environments for compiling based on user specification

import ipdb; ipdb.set_trace()
