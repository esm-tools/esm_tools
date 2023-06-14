#!/usr/bin/env python3
import os
import pathlib
import sys
import warnings

import ruamel.yaml
from ruamel.yaml.nodes import ScalarNode
from ruamel.yaml.representer import RoundTripRepresenter
from yaml import warnings

YAML_SPEC_TAG = "yaml.org,2002"
"""
str: Defines the YAML specification version used for tag replacements.
"""


class EnvironmentConstructor(ruamel.yaml.RoundTripConstructor):
    """This class is used to replace the !ENV tag with the value of the environment variable."""

    def construct_scalar(self, node):
        if isinstance(node, ScalarNode) and node.tag == "!ENV":
            env_variable = node.value
            if env_variable in os.environ:
                # NOTE(PG): This will **always** give you back a string. Have a look in the handbook:
                # https://docs.python.org/3/library/os.html#os.environ
                rval = os.environ[env_variable]
                return rval
            else:
                raise ValueError(f"Environment variable {env_variable} not found")
        rval = super().construct_scalar(node)
        return rval


class EnvironmentRepresenter(RoundTripRepresenter):
    """When dumping, this class is used to remove the !ENV tag from a particular value."""

    def represent_scalar(self, tag, value, style=None, anchor=None):
        type_tags = {
            "int": f"tag:{YAML_SPEC_TAG}:int",
            "float": f"tag:{YAML_SPEC_TAG}:float",
            "bool": f"tag:{YAML_SPEC_TAG}:bool",
            "str": f"tag:{YAML_SPEC_TAG}:str",
        }
        if tag == "!ENV":
            type_tag = type(value).__name__
            actual_type = type_tags[type_tag]
            return super().represent_scalar(actual_type, value, style, anchor=anchor)
        return super().represent_scalar(tag, value, style, anchor=anchor)


class EsmToolsLoader(ruamel.yaml.YAML):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.filename = None
        self.add_comments = True
        self.Constructor = EnvironmentConstructor
        self.Constructor.add_constructor(None, EnvironmentConstructor.construct_scalar)
        self.Representer = EnvironmentRepresenter

    def get_filename(self):
        return self.filename

    def set_filename(self, filename):
        self.filename = filename

    def load(self, stream):
        self.set_filename(stream.name)
        return super().load(stream)

    def _add_origin_comments(self, data, comment=None, key=None):
        if isinstance(data, dict):
            for key, value in data.items():
                self._add_origin_comments(
                    value,
                    comment,
                    key,
                )
        elif isinstance(data, list):
            for value in data:
                self._add_origin_comments(
                    value,
                    comment,
                    key,
                )
        try:
            if comment is None:
                comment = {
                    "Source": self.filename,
                    "line": data.lc.line,
                    "col": data.lc.col,
                }
            if hasattr(data, "yaml_set_start_comment"):
                print("Adding comment to data", data, comment)
                data.yaml_set_start_comment(comment)
            else:
                # warnings.warn("Cannot add comment to data", data, comment)
                print("Cannot add comment to data", data, comment)
        except:
            print("nope")

    def dump(self, data, stream=None, **kw):
        if not self.add_comments:
            return super().dump(data, stream, **kw)
        self._add_origin_comments(data)
        return super().dump(data, stream, **kw)


def main():
    esm_tools_loader = EsmToolsLoader()

    # Load the YAML file
    file_path = pathlib.Path("example.yaml")
    with open(file_path, "r") as file:
        esm_tools_loader.set_filename(file_path)
        data = esm_tools_loader.load(file)

    # Access the parsed data
    print(data)
    breakpoint()
    # Dump the file back to YAML
    with open("parsed_output.yaml", "w") as file:
        esm_tools_loader.dump(data, file)


if __name__ == "__main__":
    main()
