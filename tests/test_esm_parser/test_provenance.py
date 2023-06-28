"""
    Unit tests for the new provenance feature
"""
import esm_parser.provenance as provenance

config_dict = {
    "echam": {
        "type": "atmosphere",
        "files": {
            "greenhouse": {"kind": "input", "path_in_computer": "/my/path/in/computer"}
        },
    }
}

my_provenance = {
    "from_file": None,
    "type": None,
}
config = provenance.DictWithProvenance(config_dict, my_provenance)

# Test 1 (should give you a provenance of None for the key ["fesom"]["asd"])
def test_get_provenance_1():
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
def test_get_provenance_2():
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
def test_get_provenance_3():
    config_fesom = provenance.DictWithProvenance({"fesom": {"asd": 0, "model": "ocean"}}, 2)
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
def test_get_provenance_4():
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
def test_get_provenance_5():
    config["echam"].set_provenance("a_string")
    check_provenance = {
        "echam": {
            "type": "a_string",
            "files": {"greenhouse": {"kind": "a_string", "path_in_computer": "a_string"}},
        },
        "fesom": {"asd": 2, "model": 2},
        "computer": None,
        True: None,
    }
    assert config.get_provenance() == check_provenance

# Test 6 (reset the provenance of a leaf)
def test_get_provenance_6():
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

# Test 7 (reset the provenance of a leaf)
def test_get_provenance_7():
    config_fesom = provenance.DictWithProvenance({"fesom": {"update_test": True}}, "new_provenance")

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
