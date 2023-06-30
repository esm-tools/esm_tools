#!/usr/bin/env python3
import os
import re
import pathlib
import warnings

import ruamel.yaml
from ruamel.yaml import RoundTripConstructor
from ruamel.yaml.nodes import ScalarNode

YAML_SPEC_TAG = "yaml.org,2002"
"""
str: Defines the YAML specification version used for tag replacements.
"""


class EnvironmentConstructor(RoundTripConstructor):
    """This class is used to replace the !ENV tag with the value of the environment variable."""

    #def __init__(self, *args, **kwargs):
    #    self.env_variables = []

    def construct_scalar(self, node):
        print(type(self))
        tag = "!ENV"
        pattern_envvar = re.compile("\${(\w+)}")
        pattern_envtag = re.compile("""^[^\#]*\!ENV[ \t]+['|"]?\$\{\w+\}['|"]?""")
        if isinstance(node, ScalarNode) and node.tag == "!ENV":
            env_variable = node.value
            envvar_matches = pattern_envvar.findall(env_variable)
            if envvar_matches:
                full_value = env_variable
                super().env_variables.append(full_value)
                for env_var in envvar_matches:
                    # first check if the variable exists in the shell environment
                    if env_var in os.environ:
                        rval = full_value.replace(f"${{{env_var}}}", os.getenv(env_var))
                        return rval
                    else:
                        esm_parser.user_error(
                            f"{env_var} is not defined",
                            f"{env_var} is not an environment variable. Exiting",
                        )
                return rval
            else:
                raise ValueError(f"Environment variable {env_variable} not found")
        rval = super().construct_scalar(node)

        return rval


class ProvenanceConstructor(EnvironmentConstructor):
    """
    Subclasses the ``EnvironmentConstructor`` to, instead of returning only a ``data``
    returning a ``tuple`` where the element 0 is the ``data`` itself and the following
    elements are the provenance values (1 -> line number, 2 -> column). The resulting
    dictionary contains keys that are ``tuples`` and ``values`` that are tuples. This
    can then be separated into two equivalent dictionaries, one with the "real" values
    and another one with the values of the provenance.
    """

    def construct_object(self, node, *args, **kwargs):
        """
        Parameters
        ----------
        node : node object
            The node containing all the information about the yaml element

        Returns
        -------
        data : any
            The "real" value as interpreted from the parent yaml constructor
        provenance : tuple
            provenance[0]: line number
            provenance[1]: column number
            provenance[2]: file name
            provenance[3]: file category
        """

        data = super().construct_object(node, *args, **kwargs)

        yaml_file_path = os.path.abspath(node.start_mark.name)

        categories = ["runscripts", "components", "couplings", "defaults", "esm_software", "machines", "other_software", "setups", "spack_envs"]
        for category in categories:
            if category in yaml_file_path:
                category = category
            else:
                category = None
        # Todo: How to find out if category is a user runscript?
        #       If None -> always user runscript?

        provenance = (
            node.start_mark.line,
            node.start_mark.column,
            yaml_file_path,
            category,
        )

        return (data, provenance)


class EnvironmentRepresenter(ruamel.yaml.RoundTripRepresenter):
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
        self.env_variables = ['hallo']
        self.Constructor = ProvenanceConstructor
        self.Constructor.add_constructor(None, ProvenanceConstructor.construct_scalar)
        self.Constructor.add_constructor(
            "tag:yaml.org,2002:bool", ProvenanceConstructor.construct_yaml_bool
        )
        self.Representer = EnvironmentRepresenter

    def get_filename(self):
        return self.filename

    def set_filename(self, filename):
        self.filename = filename

    def load(self, stream):
        self.set_filename(stream.name)
        mapping_with_tuple_prov = super().load(stream)[0]

        def _extract_dict(mapping_with_prov):
            config = { }
            config_prov = { }
            for (key,key_prov), (value,value_prov) in mapping_with_prov.items():
                if isinstance(value, dict):
                    config[key], config_prov[key] = _extract_dict(value)
                elif isinstance(value, list):
                    config[key] = []
                    config_prov[key] = []
                    for (elem,elem_prov) in value:
                        prov = { }
                        prov["line"], prov["col"], prov["yaml_file"], prov["category"] = elem_prov
                        if isinstance(elem, dict):
                            config[key].append(_extract_dict(elem)[0])
                            config_prov[key].append(_extract_dict(elem)[1])
                        else:
                            config_prov[key].append(prov)
                            config[key].append(elem)
                else:
                    prov = { }
                    prov["line"], prov["col"], prov["yaml_file"], prov["category"] = value_prov
                    config_prov[key] = prov
                    config[key] = value
            return (config, config_prov)


        config, provenance = _extract_dict(mapping_with_tuple_prov)

        return (config, provenance)

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
                    "Defined For": key,
                    "Type": type(data).__name__,
                }
            if hasattr(data, "yaml_set_start_comment"):
                print("Adding comment to data", data, comment, type(data))
                data.yaml_set_start_comment(str(comment))
            else:
                # warnings.warn("Cannot add comment to data", data, comment)
                print("Cannot add comment to data", data, comment)
        except:
            pass
            #print("nope")

    def dump(self, data, stream=None, **kw):
        if not self.add_comments:
            return super().dump(data, stream, **kw)
        self._add_origin_comments(data)
        return super().dump(data, stream, **kw)


def main():
    esm_tools_loader = EsmToolsLoader()

    # Load the YAML file
    file_path = pathlib.Path(
        #"../../ruamel-examples/example.yaml"
       # "/home/nwieters/Modelling/esm_tools_sprint/esm_tools/configs/components/fesom/fesom-2.0.yaml"
        "/home/nwieters/Modelling/esm_tools_sprint/esm_tools/runscripts/awicm/awicm-ollie-initial-monthly.yaml"
        #"/home/nwieters/Modelling/esm_tools_sprint/esm_tools/configs/components/echam/echam.yaml"
        # "/Users/pgierz/Code/github.com/esm-tools/esm_tools/ruamel-examples/configs/components/echam/echam.yaml"
        # "/Users/mandresm/Codes/esm_tools/configs/components/echam/echam.yaml"
    )
    with open(file_path, "r") as file:
        esm_tools_loader.set_filename(file_path)
        data, data2 = esm_tools_loader.load(file)

    # Access the parsed data
#    print(data)
#    print(data2)
    # breakpoint()
    # Dump the file back to YAML

    #    comment_index = self.find_comments()
    #
    #    subprovenance = {}
    #    line = subdata.lc.line + 1
    #    source = self.filename
    #    for c, (key, val) in enumerate(subdata.items()):
    #        print(key, val, c)
    #        if not isinstance(val, list):
    #            subprovenance[key] = {"line": line+c, source}

    with open("parsed_output.yaml", "w") as file:
        esm_tools_loader.dump(data, file)

    with open("parsed_output_with_prov.yaml", "w") as file2:
        esm_tools_loader.dump(data2, file2)


if __name__ == "__main__":
    main()
