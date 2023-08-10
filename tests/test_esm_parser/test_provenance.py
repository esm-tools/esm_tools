"""
    Unit tests for the new provenance feature
"""
import os
import pathlib
import esm_parser.provenance as provenance
import esm_parser
from esm_parser import yaml_to_dict

# Todo: Check if 'GITHUB_WORKSPACE' is set, otherwise set to pwd.
if os.environ.get('GITHUB_WORKSPACE') is not None:
    example_file_path = f"{os.environ['GITHUB_WORKSPACE']}/tests/test_esm_parser/example2.yaml"
else:
    example_file_path = os.getcwd()

print(example_file_path)
config = yaml_to_dict.yaml_file_to_dict(str(pathlib.Path(f"{os.environ['GITHUB_WORKSPACE']}/tests/test_esm_parser/example2.yaml").resolve()))

check_provenance = {'echam':
    {'type':
        {'line': 2, 'col': 11, 'yaml_file': str(pathlib.Path(f"{os.environ['GITHUB_WORKSPACE']}/tests/test_esm_parser/example2.yaml").resolve()), 'category': 'runscript'},
        'files': {
            'greenhouse': {
                'kind': {'line': 5, 'col': 19, 'yaml_file': str(pathlib.Path(f"{os.environ['GITHUB_WORKSPACE']}/tests/test_esm_parser/example2.yaml").resolve()), 'category': 'runscript'},
                'path_in_computer': {'line': 6, 'col': 31, 'yaml_file': str(pathlib.Path(f"{os.environ['GITHUB_WORKSPACE']}/tests/test_esm_parser/example2.yaml").resolve()), 'category': 'runscript'}
            }
        }
    },
    'debug_info': {'loaded_from_file': None}
}

# Test 1:  Checks for a correct provenance entries from example yaml file.
def test_get_provenance_from_yaml_to_dict():
    assert config.get_provenance() == check_provenance


# Test 2: Checks whether the provenance of an added config entry is None.
def test_get_provenance_of_added_entry():
    config["fesom"] = True
    check_provenance["fesom"] = None
    assert config.get_provenance() == check_provenance


# Test 3: Checks whether the provenance of an added nested config entry is None.
def test_get_provenance_of_added_nested_entry():
    config["fesom"] = {"asd": 0}
    check_provenance["fesom"] = None
    assert config.get_provenance() == check_provenance


# Test 4: Checks whether the provenance of an added nested config entry is None.
def test_get_provenance_of_added_nested_entry_2():
    config["echam"]["test1"] = 17.
    check_provenance["echam"]["test1"] = None
    assert config.get_provenance() == check_provenance


# Test 5: Reset the provenance of an ``echam`` leave.
def test_set_provenance_for_leaf():
    new_prov = {'line': 2, 'col': 11, 'yaml_file': 'someother.yaml', 'category': 'userdefined'}
    config["echam"].set_provenance(new_prov)
    check_provenance["echam"]["type"] = new_prov
    check_provenance["echam"]["files"]["greenhouse"]["kind"] = new_prov
    check_provenance["echam"]["files"]["greenhouse"]["path_in_computer"] = new_prov
    check_provenance["echam"]["test1"] = new_prov
    assert config.get_provenance() == check_provenance


# Test 6: Reset the provenance of leaves for an later added branch ``debug_info``.
def test_set_provenance_for_leaf_of_new_branch():
    new_prov = {'line': 2, 'col': 11, 'yaml_file': 'someother.yaml', 'category': 'debuginfo'}
    config["debug_info"].set_provenance(new_prov)
    check_provenance["debug_info"]['loaded_from_file'] = new_prov
    print(config.get_provenance())
    assert config.get_provenance() == check_provenance


# Test 7: Reset the provenance of all ``echam`` leaves to "a_string")
def test_set_provenance_for_leaf_to_a_string():
    new_prov = "a_string"
    config["echam"].set_provenance(new_prov)
    check_provenance["echam"]["type"] = new_prov
    check_provenance["echam"]["files"]["greenhouse"]["kind"] = new_prov
    check_provenance["echam"]["files"]["greenhouse"]["path_in_computer"] = new_prov
    check_provenance["echam"]["test1"] = new_prov
    assert config.get_provenance() == check_provenance


# Test 8: Rest the provenanve of fesom entry.
def test_set_provenance_for_a_new_leaf():
    new_prov = {'line': 2, 'col': 11, 'yaml_file': 'someother.yaml', 'category': 'set_for_onknown_leaf'}
    config["fesom"] = provenance.DictWithProvenance(config["fesom"], {})
    config["fesom"].set_provenance(new_prov)
    check_provenance["fesom"] = {"asd": None}
    check_provenance["fesom"]["asd"] = new_prov
    assert config.get_provenance() == check_provenance

## Test 9: Reset the provenance of a list")
#def test_set_provenance_for_a_list_leaf():
#    new_prov = {'line': 2, 'col': 11, 'yaml_file': 'someother.yaml', 'category': 'this_is_for_a_list'}
#    config["fesom"] = provenance.DictWithProvenance(config["fesom"], {})
#    config["fesom"] = {"list": [30, 19]}
#    config["fesom"]["list"] = provenance.ListWithProvenance(config["fesom"]["list"], [None, None])
#    config["fesom"]["list"].set_provenance(new_prov)
#    #print("provenance: ",config["fesom"]["list"].get_provenance())
#    #print("provenance2: ",config.get_provenance())
#    assert config.get_provenance() == check_provenance


# Test 10: Test the extraction of config for all allowed variable types.
def test_extract_dict_config():
    esm_tools_loader = yaml_to_dict.EsmToolsLoader()
    os.environ['USER'] = "some_user"
    file_path = pathlib.Path(str(pathlib.Path(f"{os.environ['GITHUB_WORKSPACE']}/tests/test_esm_parser/example.yaml")))
    config = {
            'person': {
                'name': 'Paul Gierz',
                'username': os.environ['USER'],
                'a_string': ' hello world I am here to make your life impossible ',
                'my_var': 'MY_VAR',
                'my_other_var': ['a', 'b', 'c'],
                'my_other_list': ['a', 'b', 'c'],
                'my_bolean': True,
                'my_int': 12.1,
                'my_int2': 42,
                'list_with_dict_inside': [1, 2, {
                    'my_dict': {
                        'foo': [1, 2, {
                            'my_dict': {
                                'foo': 'bar'}
                            }]
                    }
                }]
            }
    }

    with open(file_path, "r") as file:
        esm_tools_loader.set_filename(file_path)
        data, data2 = esm_tools_loader.load(file)

    assert data == config


# Test 11: Check provenance of a list entry
def test_check_provenance_list():
    os.environ['USER'] = "some_user"
    esm_tools_loader = yaml_to_dict.EsmToolsLoader()
    file_path = pathlib.Path(str(pathlib.Path(f"{os.environ['GITHUB_WORKSPACE']}/tests/test_esm_parser/example.yaml")))
    check_prov = [{'line': 15, 'col': 19, 'yaml_file': str(pathlib.Path(f"{os.environ['GITHUB_WORKSPACE']}/tests/test_esm_parser/example.yaml").resolve()), 'category': 'runscript'}, {'line': 15, 'col': 22, 'yaml_file': str(pathlib.Path(f"{os.environ['GITHUB_WORKSPACE']}/tests/test_esm_parser/example.yaml").resolve()), 'category': 'runscript'}, {'line': 15, 'col': 25, 'yaml_file': str(pathlib.Path(f"{os.environ['GITHUB_WORKSPACE']}/tests/test_esm_parser/example.yaml").resolve()), 'category': 'runscript'}]

    with open(file_path, "r") as file:
        esm_tools_loader.set_filename(file_path)
        data, data2 = esm_tools_loader.load(file)

    config = provenance.DictWithProvenance(data, data2)
    assert config["person"]["my_other_list"].get_provenance() == check_prov


# Test 12: Check set_provenance of a list entry
def test_check_set_provenance_list():
    os.environ['USER'] = "some_user"
    esm_tools_loader = yaml_to_dict.EsmToolsLoader()
    file_path = pathlib.Path(str(pathlib.Path(f"{os.environ['GITHUB_WORKSPACE']}/tests/test_esm_parser/example.yaml")))
    new_prov = {'line': 15, 'col': 25, 'yaml_file': 'example.yaml', 'category': 'from_a_list'}
    check_prov = [new_prov, new_prov, new_prov]

    with open(file_path, "r") as file:
        esm_tools_loader.set_filename(file_path)
        data, data2 = esm_tools_loader.load(file)

    config = provenance.DictWithProvenance(data, data2)
    config["person"]["my_other_list"].set_provenance(new_prov)
    assert config["person"]["my_other_list"].get_provenance() == check_prov


# Test 13: Check set_provenance of a single list entry
def test_check_set_provenance_of_single_list_entry():
    os.environ['USER'] = "some_user"
    esm_tools_loader = yaml_to_dict.EsmToolsLoader()
    file_path = pathlib.Path(str(pathlib.Path(f"{os.environ['GITHUB_WORKSPACE']}/tests/test_esm_parser/example.yaml")))
    old_prov1 = {'line': 15, 'col': 19, 'yaml_file': str(pathlib.Path(f"{os.environ['GITHUB_WORKSPACE']}/tests/test_esm_parser/example.yaml").resolve()), 'category': 'runscript'}
    old_prov2 = {'line': 15, 'col': 22, 'yaml_file': str(pathlib.Path(f"{os.environ['GITHUB_WORKSPACE']}/tests/test_esm_parser/example.yaml").resolve()), 'category': 'runscript'}
    new_prov = {'line': 15, 'col': 25, 'yaml_file': 'example.yaml', 'category': 'from_a_second_list'}
    check_prov = [old_prov1, old_prov2, new_prov]

    with open(file_path, "r") as file:
        esm_tools_loader.set_filename(file_path)
        data, data2 = esm_tools_loader.load(file)

    config = provenance.DictWithProvenance(data, data2)
    config["person"]["my_other_list"][2].provenance = new_prov
    assert config["person"]["my_other_list"].get_provenance() == check_prov
