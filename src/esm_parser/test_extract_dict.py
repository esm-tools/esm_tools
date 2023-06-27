#!/usr/bin/env python3
import pathlib
import esm_parser
from esm_parser import yaml_to_dict

def test_extract_dict():
    esm_tools_loader = yaml_to_dict.EsmToolsLoader()
    file_path = pathlib.Path("../../ruamel-examples/example.yaml")
    config = {
            'person': {
                'name': 'Paul Gierz',
                'username': 'nwieters',
                'a_string': ' hello world I am here to make your life impossible ',
                'my_var': 'TEST',
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
