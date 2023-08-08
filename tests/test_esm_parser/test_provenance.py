"""
Unit tests for the new provenance feature
"""
import os
import pathlib
import sys

import pytest
from ruamel.yaml import YAML

import esm_parser
import esm_parser.provenance as provenance
from esm_parser import yaml_to_dict


@pytest.fixture
def yaml():
    yaml = YAML()
    return yaml


@pytest.fixture
def config():
    config_dict = {
        "echam": {
            "type": "atmosphere",
            "files": {
                "greenhouse": {
                    "kind": "input",
                    "path_in_computer": "/my/path/in/computer",
                }
            },
        }
    }

    my_provenance = {
        "from_file": None,
        "type": None,
    }
    config = provenance.DictWithProvenance(config_dict, my_provenance)
    return config


# Test 1 (should give you a provenance of None for the key ["fesom"]["asd"])
def test_get_provenance_1(config):
    config["fesom"] = {"asd": 0}
    check_provenance = {
        "echam": {
            "type": {"from_file": None, "type": None},
            "files": {
                "greenhouse": {
                    "kind": {"from_file": None, "type": None},
                    "path_in_computer": {"from_file": None, "type": None},
                }
            },
        },
        "fesom": {"asd": None},
    }
    assert config.get_provenance() == check_provenance


# Test 2 (should give you a provenance of None for the key "computer")
def test_get_provenance_2(config):
    config["computer"] = 0
    check_provenance = {
        "echam": {
            "type": {"from_file": None, "type": None},
            "files": {
                "greenhouse": {
                    "kind": {"from_file": None, "type": None},
                    "path_in_computer": {"from_file": None, "type": None},
                }
            },
        },
        "fesom": {"asd": None},
        "computer": None,
    }
    assert config.get_provenance() == check_provenance


# Test 3 (should give you a provenance of 2 for the leaf keys inside "fesom")
def test_get_provenance_3(config):
    config_fesom = provenance.DictWithProvenance(
        {"fesom": {"asd": 0, "model": "ocean"}}, 2
    )
    config.update(config_fesom)
    check_provenance = {
        "echam": {
            "type": {"from_file": None, "type": None},
            "files": {
                "greenhouse": {
                    "kind": {"from_file": None, "type": None},
                    "path_in_computer": {"from_file": None, "type": None},
                }
            },
        },
        "fesom": {"asd": 2, "model": 2},
        "computer": None,
    }
    assert config.get_provenance() == check_provenance


# Test 4 (should give you a provenance of None for the key True)
def test_get_provenance_4(config):
    config[True] = "boolean"
    check_provenance = {
        "echam": {
            "type": {"from_file": None, "type": None},
            "files": {
                "greenhouse": {
                    "kind": {"from_file": None, "type": None},
                    "path_in_computer": {"from_file": None, "type": None},
                }
            },
        },
        "fesom": {"asd": 2, "model": 2},
        "computer": None,
        True: None,
    }
    assert config.get_provenance() == check_provenance


# Test 5 (reset the provenance of all ``echam`` leaves to "a_string")
def test_get_provenance_5(config):
    config["echam"].set_provenance("a_string")
    check_provenance = {
        "echam": {
            "type": "a_string",
            "files": {
                "greenhouse": {"kind": "a_string", "path_in_computer": "a_string"}
            },
        },
        "fesom": {"asd": 2, "model": 2},
        "computer": None,
        True: None,
    }
    assert config.get_provenance() == check_provenance


# Test 6 (reset the provenance of a leaf)
def test_get_provenance_6(config):
    config["echam"]["files"]["greenhouse"].provenance["kind"] = "a_new_string"
    check_provenance = {
        "echam": {
            "type": "a_string",
            "files": {
                "greenhouse": {"kind": "a_new_string", "path_in_computer": "a_string"}
            },
        },
        "fesom": {"asd": 2, "model": 2},
        "computer": None,
        True: None,
    }
    assert config.get_provenance() == check_provenance


def test_extract_dict():
    esm_tools_loader = yaml_to_dict.EsmToolsLoader()
    file_path = pathlib.Path("example.yaml")
    config = {
        "person": {
            "name": "Paul Gierz",
            "username": "nwieters",
            "a_string": " hello world I am here to make your life impossible ",
            "my_var": "MY_VAR",
            "my_other_var": ["a", "b", "c"],
            "my_other_list": ["a", "b", "c"],
            "my_bolean": True,
            "my_int": 12.1,
            "my_int2": 42,
            "list_with_dict_inside": [
                1,
                2,
                {"my_dict": {"foo": [1, 2, {"my_dict": {"foo": "bar"}}]}},
            ],
        }
    }

    with open(file_path, "r") as file:
        esm_tools_loader.set_filename(file_path)
        data, data2 = esm_tools_loader.load(file)

    assert data == config


def test_extract_provenance():
    esm_tools_loader = yaml_to_dict.EsmToolsLoader()
    file_path = pathlib.Path("example.yaml")
    file_path = os.path.abspath(file_path)
    provenance = {
        "person": {
            "name": {"line": 1, "col": 8, "yaml_file": file_path, "category": "None"},
            "username": {
                "line": 2,
                "col": 12,
                "yaml_file": file_path,
                "category": "None",
            },
            "a_string": {
                "line": 3,
                "col": 12,
                "yaml_file": file_path,
                "category": "None",
            },
            "my_var": {
                "line": 8,
                "col": 10,
                "yaml_file": file_path,
                "category": "None",
            },
            "my_other_var": [
                {"line": 10, "col": 8, "yaml_file": file_path, "category": "None"},
                {"line": 12, "col": 8, "yaml_file": file_path, "category": "None"},
                {"line": 13, "col": 8, "yaml_file": file_path, "category": "None"},
            ],
            "my_other_list": [
                {"line": 14, "col": 18, "yaml_file": file_path, "category": "None"},
                {"line": 14, "col": 21, "yaml_file": file_path, "category": "None"},
                {"line": 14, "col": 24, "yaml_file": file_path, "category": "None"},
            ],
            "my_bolean": {
                "line": 16,
                "col": 13,
                "yaml_file": file_path,
                "category": "None",
            },
            "my_int": {
                "line": 17,
                "col": 10,
                "yaml_file": file_path,
                "category": "None",
            },
            "my_int2": {
                "line": 18,
                "col": 11,
                "yaml_file": file_path,
                "category": "None",
            },
            "list_with_dict_inside": [
                {"line": 20, "col": 4, "yaml_file": file_path, "category": "None"},
                {"line": 21, "col": 4, "yaml_file": file_path, "category": "None"},
                {
                    "my_dict": {
                        "foo": [
                            {
                                "line": 24,
                                "col": 10,
                                "yaml_file": file_path,
                                "category": "None",
                            },
                            {
                                "line": 25,
                                "col": 10,
                                "yaml_file": file_path,
                                "category": "None",
                            },
                            {
                                "my_dict": {
                                    "foo": {
                                        "line": 27,
                                        "col": 17,
                                        "yaml_file": file_path,
                                        "category": "None",
                                    }
                                }
                            },
                        ]
                    }
                },
            ],
        }
    }

    with open(file_path, "r") as file:
        esm_tools_loader.set_filename(file_path)
        data, data2 = esm_tools_loader.load(file)

    assert data2 == provenance


# Test 7 (reset the provenance of a leaf)
def test_get_provenance_7(config):
    config_fesom = provenance.DictWithProvenance(
        {"fesom": {"update_test": True}}, "new_provenance"
    )

    config["fesom"].update(config_fesom["fesom"])

    check_provenance = {
        "echam": {
            "type": "a_string",
            "files": {
                "greenhouse": {"kind": "a_new_string", "path_in_computer": "a_string"}
            },
        },
        "fesom": {"asd": 2, "model": 2, "update_test": "new_provenance"},
        "computer": None,
        True: None,
    }
    assert config.get_provenance() == check_provenance


def test_dump_config_with_provenance(config, yaml):
    """Prints out the dictionary with comments"""
    breakpoint()

    print(yaml.dump(config.get_provenance(), sys.stdout))
