"""
Unit tests for the new provenance feature
"""
import os
import pathlib
import pytest

import esm_parser.provenance as provenance
import esm_parser

from esm_parser import yaml_to_dict


@pytest.fixture()
def example_path1():
    return f"{os.path.dirname(__file__)}/example.yaml"


@pytest.fixture()
def example_path2():
    return f"{os.path.dirname(__file__)}/example2.yaml"


@pytest.fixture()
def config(example_path2):
    return yaml_to_dict.yaml_file_to_dict(example_path2)


@pytest.fixture()
def check_provenance(example_path2):
    check_provenance = {
        "echam": {
            "type": {
                "line": 2,
                "col": 11,
                "yaml_file": example_path2,
                "category": "runscript",
            },
            "files": {
                "greenhouse": {
                    "kind": {
                        "line": 5,
                        "col": 19,
                        "yaml_file": example_path2,
                        "category": "runscript",
                    },
                    "path_in_computer": {
                        "line": 6,
                        "col": 31,
                        "yaml_file": example_path2,
                        "category": "runscript",
                    },
                    "a_list": [
                        {
                            "line": 8,
                            "col": 19,
                            "yaml_file": example_path2,
                            "category": "runscript",
                        },
                        {
                            "line": 9,
                            "col": 19,
                            "yaml_file": example_path2,
                            "category": "runscript",
                        },
                        {
                            "line": 10,
                            "col": 19,
                            "yaml_file": example_path2,
                            "category": "runscript",
                        },
                    ],
                }
            },
        },
    }

    return check_provenance


def test_get_provenance_from_yaml_to_dict(config, check_provenance):
    """
    Test 1:  Checks for correct provenance entries from example2.yaml file.
    """

    assert config.get_provenance() == check_provenance


def test_get_provenance_of_added_entry(config, check_provenance):
    """
    Test 2: Checks whether the provenance of an added config entry is None.
    """

    config["fesom"] = True
    check_provenance["fesom"] = None
    assert config.get_provenance() == check_provenance


def test_get_provenance_of_added_nested_entry(config, check_provenance):
    """
    Test 3: Checks whether the provenance of an added nested config entry is None.
    """

    config["fesom"] = {"asd": 0}
    check_provenance["fesom"] = None
    assert config.get_provenance() == check_provenance


def test_get_provenance_of_added_nested_entry_2(config, check_provenance):
    """
    Test 4: Checks whether the provenance of an added nested config entry is None.
    """

    config["echam"]["test1"] = 17.0
    check_provenance["echam"]["test1"] = None
    assert config.get_provenance() == check_provenance


def test_set_provenance_for_leaf(config, check_provenance):
    """
    Test 5: Reset the provenance of an ``echam`` leave.
    """

    new_prov = {
        "line": 2,
        "col": 11,
        "yaml_file": "someother.yaml",
        "category": "userdefined",
    }
    print(config["echam"].get_provenance())
    config["echam"].set_provenance(new_prov)
    print(config)
    check_provenance["echam"]["type"] = new_prov
    check_provenance["echam"]["files"]["greenhouse"]["kind"] = new_prov
    check_provenance["echam"]["files"]["greenhouse"]["path_in_computer"] = new_prov
    check_provenance["echam"]["files"]["greenhouse"]["a_list"] = [
        new_prov,
        new_prov,
        new_prov,
    ]
    assert config.get_provenance() == check_provenance


def test_set_provenance_for_leaf_of_new_branch(config, check_provenance):
    """
    Test 6: Reset the provenance of leaves for an later added branch ``new_branch``.
    """

    new_prov = {
        "line": 2,
        "col": 11,
        "yaml_file": "someother.yaml",
        "category": "debuginfo",
    }
    config["new_branch"] = provenance.DictWithProvenance({"loaded_from_file": None}, {})
    config["new_branch"].set_provenance(new_prov)
    check_provenance["new_branch"] = {"loaded_from_file": new_prov}
    assert config.get_provenance() == check_provenance


def test_set_provenance_for_leaf_to_a_string(config, check_provenance):
    """
    Test 7: Reset the provenance of all ``echam`` leaves to "a_string")
    """

    new_prov = "a_string"
    config["echam"].set_provenance(new_prov)
    check_provenance["echam"]["type"] = new_prov
    check_provenance["echam"]["files"]["greenhouse"]["kind"] = new_prov
    check_provenance["echam"]["files"]["greenhouse"]["path_in_computer"] = new_prov
    check_provenance["echam"]["files"]["greenhouse"]["a_list"] = [
        new_prov,
        new_prov,
        new_prov,
    ]
    assert config.get_provenance() == check_provenance


def test_set_provenance_for_a_new_leaf(config, check_provenance):
    """
    Test 8: Rest the provenanve of fesom entry.
    """

    config["fesom"] = {"asd": 0}
    new_prov = {
        "line": 2,
        "col": 11,
        "yaml_file": "someother.yaml",
        "category": "set_for_unknown_leaf",
    }
    config["fesom"] = provenance.DictWithProvenance(config["fesom"], {})
    config["fesom"].set_provenance(new_prov)
    check_provenance["fesom"] = {"asd": None}
    check_provenance["fesom"]["asd"] = new_prov
    assert config.get_provenance() == check_provenance


@pytest.fixture()
def test_provenance_update(config, check_provenance):
    """
    Test 9: Checks that the method update preserves the provenance history
    """

    new_prov = {
        "line": 2,
        "col": 11,
        "yaml_file": "someother.yaml",
        "category": "set_for_unknown_leaf",
    }
    new_config = {
        "echam": provenance.DictWithProvenance({"type": "mpi_atmosphere"}, {})
    }
    new_config["echam"].set_provenance(new_prov)

    config["echam"].update(new_config["echam"])
    check_provenance["echam"]["type"] = new_prov
    assert config.get_provenance() == check_provenance
    # Checks that update preserves provenance history
    assert config["echam"]["type"].provenance == [
        {
            "line": 2,
            "col": 11,
            "yaml_file": example_path2,
            "category": "runscript",
        },
        None,
        {
            "line": 2,
            "col": 11,
            "extended_by": "dict.update",
            "yaml_file": "someother.yaml",
            "category": "set_for_unknown_leaf",
        },
    ]


def test_set_provenance_for_a_list_leaf(config, check_provenance):
    """
    Test 9: Reset the provenance of a list")
    """

    new_prov = {
        "line": 2,
        "col": 11,
        "yaml_file": "someother.yaml",
        "category": "this_is_for_a_list",
    }
    config["fesom"] = {"asd": 0}
    config["fesom"]["list"] = [30, 19]
    config["fesom"] = provenance.DictWithProvenance(config["fesom"], {})
    config["fesom"]["list"].set_provenance(new_prov)
    check_provenance["fesom"] = {}
    check_provenance["fesom"]["list"] = [new_prov, new_prov]
    check_provenance["fesom"]["asd"] = None
    assert config.get_provenance() == check_provenance


def test_extract_dict_config(example_path1):
    """
    Test 10: Test the extraction of config for all allowed variable types.
    """

    esm_tools_loader = yaml_to_dict.EsmToolsLoader()
    os.environ["USER"] = "some_user"
    config = {
        "person": {
            "name": "Paul Gierz",
            "username": os.environ["USER"],
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

    with open(example_path1, "r") as file:
        esm_tools_loader.set_filename(example_path1)
        data, data2 = esm_tools_loader.load(file)

    assert data == config


def test_check_provenance_list(example_path1):
    """
    Test 11: Check provenance of a list entry
    """

    os.environ["USER"] = "some_user"
    esm_tools_loader = yaml_to_dict.EsmToolsLoader()
    check_prov = [
        {"line": 15, "col": 19, "yaml_file": example_path1, "category": "runscript"},
        {"line": 15, "col": 22, "yaml_file": example_path1, "category": "runscript"},
        {"line": 15, "col": 25, "yaml_file": example_path1, "category": "runscript"},
    ]

    with open(example_path1, "r") as file:
        esm_tools_loader.set_filename(file)
        data, data2 = esm_tools_loader.load(file)

    config = provenance.DictWithProvenance(data, data2)
    assert config["person"]["my_other_list"].get_provenance() == check_prov


def test_check_set_provenance_list(example_path1):
    """
    Test 12: Check set_provenance of a list entry
    """

    os.environ["USER"] = "some_user"
    esm_tools_loader = yaml_to_dict.EsmToolsLoader()
    new_prov = {
        "line": 15,
        "col": 25,
        "yaml_file": "example.yaml",
        "category": "from_a_list",
    }
    check_prov = [new_prov, new_prov, new_prov]

    with open(example_path1, "r") as file:
        esm_tools_loader.set_filename(file)
        data, data2 = esm_tools_loader.load(file)

    config = provenance.DictWithProvenance(data, data2)
    config["person"]["my_other_list"].set_provenance(new_prov)
    assert config["person"]["my_other_list"].get_provenance() == check_prov


def test_check_set_provenance_of_single_list_entry(example_path1):
    """
    Test 13: Check set_provenance of a single list entry
    """

    os.environ["USER"] = "some_user"
    esm_tools_loader = yaml_to_dict.EsmToolsLoader()
    old_prov1 = {
        "line": 15,
        "col": 19,
        "yaml_file": example_path1,
        "category": "runscript",
    }
    old_prov2 = {
        "line": 15,
        "col": 22,
        "yaml_file": example_path1,
        "category": "runscript",
    }
    new_prov = {
        "line": 15,
        "col": 25,
        "yaml_file": "example.yaml",
        "category": "from_a_second_list",
    }
    check_prov = [old_prov1, old_prov2, new_prov]

    with open(example_path1, "r") as file:
        esm_tools_loader.set_filename(file)
        data, data2 = esm_tools_loader.load(file)

    config = provenance.DictWithProvenance(data, data2)
    config["person"]["my_other_list"][2].provenance = provenance.Provenance(new_prov)
    assert config["person"]["my_other_list"].get_provenance() == check_prov


def test_check_set_provenance_of_single_list_entry(example_path1):
    """
    Test 14: Check get_provenance raises the correct error when the provenance of an
    item is not of the type provenance.Provenance
    """

    os.environ["USER"] = "some_user"
    esm_tools_loader = yaml_to_dict.EsmToolsLoader()
    new_prov = {
        "line": 15,
        "col": 25,
        "yaml_file": "example.yaml",
        "category": "from_a_list",
    }

    with open(example_path1, "r") as file:
        esm_tools_loader.set_filename(file)
        data, data2 = esm_tools_loader.load(file)

    config = provenance.DictWithProvenance(data, data2)

    with pytest.raises(
        ValueError,
        match="Provenance must be an instance of the provenance.Provenance class!",
    ):
        config["person"]["my_other_list"][2].provenance = new_prov


@pytest.fixture()
def test_keep_provenance_in_recursive_function(config):
    """
    Test 15: Test that provenance is not modified in a method when it has the
    keep_provenance_in_recursive_function decorator
    """
    @provenance.keep_provenance_in_recursive_function
    def change_elem(tree, rhs):
        return provenance.wrapper_with_provenance_factory("new_val", {"modified": True})

    tree = []

    check_provenance1 = [
        {
            "line": 2,
            "col": 11,
            "yaml_file": example_path2,
            "category": "runscript",
        },
        {
            "modified": True,
            "extended_by": "<function test_keep_provenance.<locals>.change_elem at 0x11433f8b0>",
        },
    ]
    check_provenance2 = [
        {
            "line": 9,
            "col": 19,
            "yaml_file": example_path2,
            "category": "runscript",
        },
        {
            "modified": True,
            "extended_by": "<function test_keep_provenance.<locals>.change_elem at 0x11433f8b0>",
        },
    ]

    rhs1 = change_elem(tree, config["echam"]["type"])
    rhs2 = change_elem(tree, config["echam"]["files"]["greenhouse"]["a_list"][1])

    assert rhs1 == "new_val" and rhs1.provenance[0] == check_provenance1[0]
    assert rhs2 == "new_val" and rhs2.provenance[0] == check_provenance2[0]
