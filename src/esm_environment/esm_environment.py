#!/usr/bin/env python
"""
Main module for EsmEnvironment.
"""

import copy
import os
import re
import sys
import warnings

import esm_parser
from esm_tools import user_error, user_note

ENVIRONMENT_VARIABLES = [
    "module_actions",
    "add_module_actions",
    "spack_actions",
    "add_spack_actions",
    "export_vars",
    "add_export_vars",
    "unset_vars",
    "add_unset_vars",
    "general_actions",
    "add_general_actions",
]

######################################################################################
########################### class "environment_infos" ################################
######################################################################################


class EnvironmentInfos:
    """
    The package ``esm_environment`` takes care of generating the environments for the
    different HPCs supported by `ESM-Tools`. This is done through the use of the
    ``EnvironmentInfos`` class inside the different `ESM-Tools` packages.

    For the correct definition of an environment for an HPC a `yaml` file for that
    system needs to be included inside the ``esm_tools`` package inside the
    ``configs/machines/`` folder (e.g. ``ollie.yaml``). This file should contain all
    the required preset variables for that system and the environment variables
    ``module_actions``, ``export_vars`` and ``unset_vars``.

    By instancing the ``EnvironmentInfos`` class, the environment information for
    the specified component or coupled setup is compiled and stored in
    ``self.commands``.
    """

    def __init__(self, config, execution_mode, component=None):
        """
        Initializes the environment object and default attributes.

        Parameters
        ----------
        config : dict
            Dictionary containing all the compiled information from the `yaml` files
            needed for the current `ESM-Tools` operation.
        execution_mode : str
            A string indicating whether ``EnvironmentInfos`` was instanced from a
            compilation operation (``compile``) or a run operarion (``run``).
        component : str, optional
            Component for which the environment is required.
        """
        # Ensure local copy of config to avoid mutating it... (facepalm)
        self.config = copy.deepcopy(config)
        self.execution_mode = execution_mode
        self.component = component
        # Load computer dictionary
        self.computer = self.config.get("computer", {})
        if not self.computer:
            logger.error("No computer dictionary found in config")
            raise ValueError("No computer dictionary found in config")

        # [TODO] add this to defaults yaml when that is merged
        self.computer["merge_component_envs"] = {
            "compile": self.computer.get("merge_component_envs", {}).get(
                "compile", False
            ),
            "run": self.config.get("merge_component_envs", {}).get("run", True),
        }
        self.computer["include_env_from_component_files"] = True

        # Check for deprecated environment commands
        self.report_deprecated_environment_changes(self.config)

        # Add the ENVIRONMENT_SET_BY_ESMTOOLS into the exports
        self.add_esm_var()

        # Define the environment commands for the script
        self.commands = self.get_shell_commands()

    def add_esm_var(self):
        """
        Adds the ENVIRONMENT_SET_BY_ESMTOOLS=TRUE to the config, for later
        dumping to the shell script.
        """

        if "export_vars" in self.computer:
            self.computer["export_vars"]["ENVIRONMENT_SET_BY_ESMTOOLS"] = "TRUE"
        else:
            self.computer["export_vars"] = {"ENVIRONMENT_SET_BY_ESMTOOLS": "TRUE"}

    def report_deprecated_environment_changes(
        self, mapping={}, deprecation_list=[], is_recursion=False
    ):
        """
        Report deprecated environment changes in the configuration (e.g.
        ``environment_changes``, ``compiletime_environment_changes``,
        ``runtime_environment_changes``).

        Parameters
        ----------
        mapping : dict or list
            The mapping to check for deprecated keys.
        deprecation_list : list
            A list to store deprecated keys through recursions.
        is_recursion : bool
            A flag to indicate if the function is being called recursively.

        Raises
        ------
        esm_tools.user_error
            If deprecated keys are found, an error is raised with hints.
        """
        env_changes_keys = [
            "environment_changes",
            "compiletime_environment_changes",
            "runtime_environment_changes",
        ]
        if isinstance(mapping, dict):
            for key, value in mapping.items():
                if key in env_changes_keys:
                    deprecation_list.append(value)
                self.report_deprecated_environment_changes(
                    mapping=value, deprecation_list=deprecation_list, is_recursion=True
                )
        elif isinstance(mapping, list):
            for item in mapping:
                self.report_deprecated_environment_changes(
                    mapping=item, deprecation_list=deprecation_list, is_recursion=True
                )

        # After recursion, check if any deprecated keys were found and report them
        # with an error
        if not is_recursion and len(deprecation_list) > 0:
            hints = []
            for item in deprecation_list:
                hints.append(
                    {
                        "type": "prov",
                        "object": item,
                        "text": "@HINT@",
                    }
                )
            hint_list = ""
            for indx, _ in enumerate(deprecation_list):
                hint_list += f"\n - @HINT_{indx}@"
            user_error(
                "Deprecated environment_changes",
                "The environment variables ``environment_changes``, "
                "``compiletime_environment_changes``, and "
                "``runtime_environment_changes`` keys are deprecated. Please "
                "define the ``add_module_actions``, ``add_export_vars``, and "
                "``add_unset_vars`` inside the ``computer`` section of the yaml file. "
                f"This affects the following lines in the yaml files:{hint_list}",
                hints=hints,
            )

    def get_shell_commands(self):
        """
        Gathers module actions and export variables from the computer into a list,
        prepending appropriate shell command words (e.g. module and export).

        If the ``export_vars`` dictionary contains variables with indexes (``[(int)]``)
        or ``[(list)]``, those are removed before returning the command list.

        Returns
        -------
        environment : list
            A list of the environment operations, to be used in the compilation and run
            scripts.
        """

        environment = []
        # Fix for seb-wahl's hack via source
        if self.computer.get("general_actions") is not None:
            self.process_env_vars("general_actions")
            for action in self.computer["general_actions"]:
                environment.append(action)
        # Write module actions
        if self.computer.get("module_actions") is not None:
            self.process_env_vars("module_actions")
            for action in self.computer["module_actions"]:
                # seb-wahl: workaround to allow source ... to be added to the batch header
                # until a proper solution is available. Required with FOCI
                if action.startswith("source"):
                    environment.append(action)
                else:
                    environment.append(f"module {action}")
        # Write Spack actions
        if self.computer.get("spack_actions") is not None:
            self.process_env_vars("spack_actions")
            for action in self.computer["spack_actions"]:
                environment.append(f"spack {action}")
        # Add an empty string as a newline:
        environment.append("")
        if self.computer.get("export_vars") is not None:
            self.process_env_vars("export_vars")
            for var in self.computer["export_vars"]:
                # If export_vars is a dictionary
                if isinstance(self.computer["export_vars"], dict):
                    # If the variable is a dictionary itself (e.g. "AWI_FESOM_YAML"
                    # in fesom-1.4) add the contents of the dictionary as the value of
                    # the exported variable inside '""'
                    if isinstance(self.computer["export_vars"][var], dict):
                        key = var
                        value = self.computer["export_vars"][key]
                        environment.append(f"export {key}='{str(value)}'")
                    # If the value of the variable is not a dictionary
                    else:
                        key = var
                        value = self.computer["export_vars"][key]
                        # Define the pattern for indexes [(int)]
                        ipattern = "\[+\(\d+\)+\]$"
                        # If the variable was added as a list produce the correct string
                        if key.endswith("[(list)]"):
                            key = key.replace("[(list)]", "")
                            environment.append(f"export {value}")
                        # If the variable contained a repetition index, remove it
                        elif re.search(ipattern, key):
                            environment.append(
                                f"export {re.sub(ipattern, '', key)}={str(value)}"
                            )
                        # It it is a normal variable return the export command
                        else:
                            environment.append(f"export {key}={str(value)}")
                # If export_vars is a list append the export command (this should not
                # happen anymore as the export_vars in the machine files should be all
                # defined now as dictionaries
                else:
                    environment.append("export {str(var)}")
        environment.append("")
        # Write the unset commands
        if self.computer.get("unset_vars") is not None:
            self.process_env_vars("unset_vars")
            for var in self.computer["unset_vars"]:
                environment.append(f"unset {var}")

        return environment

    def process_env_vars(self, env_var_key):
        """
        Processes the environment variables for the given key ``env_var_key`` in the
        ``self.computer`` dictionary. This function handles:
        - selecting environment variables based on their attributes (if any)
        - removing environment variables from component files if specified
        - selecting environment variables based on their provenance of the files
          (if required by the user)
        - sorting the environment variables based on their original order in the
          configuration files

        Parameters
        ----------
        env_var_key : str
            The name of the set of environment variables to process (e.g.
            ``module_actions``, ``export_vars``, ``unset_vars``).

        Mutates
        -------
        self.computer : dict
            The EnvironmentInfo ``computer`` dictionary is modified specifically for
            ``env_var_key``, with the selected and sorted environment variables.
        """
        self.select_env_vars_based_on_var_attributes(env_var_key)
        self.remove_env_vars_from_component_files(env_var_key)
        self.select_env_vars_based_on_provenance(env_var_key)
        self.sort_env_vars(env_var_key, esm_parser.CATEGORY_HIERARCHY)

    def _filter_env_vars(self, env_vars, condition_fn):
        """
        Helper function to filter environment variables based on a condition.

        Parameters
        ----------
        env_vars : dict or list
            The environment variables to filter.
        condition_fn : function
            A function that takes a value and returns ``True`` if the value should be
            included in the filtered result.

        Returns
        -------
        filtered_env_vars : dict or list
            The filtered environment variables.
        """
        if isinstance(env_vars, dict):
            filtered_env_vars = esm_parser.DictWithProvenance({}, None)
            for key, value in env_vars.items():
                if condition_fn(value):
                    filtered_env_vars[key] = value
        elif isinstance(env_vars, list):
            filtered_env_vars = esm_parser.ListWithProvenance([], None)
            for value in env_vars:
                if condition_fn(value):
                    filtered_env_vars.append(value)
        else:
            raise ValueError("env_vars must be an instance of dict or list")

        return filtered_env_vars

    def _flatten_values_with_attrs(self, env_vars):
        """
        Helper function to flatten environment variables with attributes. Loops through
        the given ``env_vars`` dictionary (``export_vars``, ``module_actions``, or
        ``unset_vars``) and flattens the values of their nested dictionaries and lists
        recursively, if those values are dictionaries containing the ``_value`` key.

        For example:

        .. code-block::python3

           export_vars = {
               "OIFS_OASIS_BASE": {"_value": "/a/path"},
           }

        would be flattened to:

        .. code-block::python3

           export_vars = {
               "OIFS_OASIS_BASE": "/a/path",
           }

        Parameters
        ----------
        env_vars : dict or list
            The environment variables to flatten.

        Returns
        -------
        new_env_vars : dict or list
            The flattened environment variables.
        """
        if isinstance(env_vars, dict):
            new_env_vars = esm_parser.DictWithProvenance({}, None)
            for key, value in env_vars.items():
                if isinstance(value, dict) and "_value" in value:
                    new_env_vars[key] = value["_value"]
                else:
                    new_env_vars[key] = value
        elif isinstance(env_vars, list):
            new_env_vars = esm_parser.ListWithProvenance([], None)
            for value in env_vars:
                if isinstance(value, dict) and "_value" in value:
                    new_env_vars.append(value["_value"])
                else:
                    new_env_vars.append(value)
        else:
            raise ValueError("env_vars must be an instance of dict or list")

        return new_env_vars

    def select_env_vars_based_on_var_attributes(self, env_var_key):
        """
        Selects environment variables based on their attributes (if any) and the
        current execution mode and component. The attributes are not truly class attributes,
        but rather keys in the environment variable dictionaries that are defined in the
        config yaml files, for example as:

        .. code-block::yaml

            export_vars:
                OIFS_OASIS_BASE:
                    _value: /a/path
                    _execution_mode: compile
                    _component: oifs

        Each environment variable can have the following keys (attributes):
        - ``_value``: The value of the environment variable once flattened
        - ``_execution_mode``: The execution mode for which the variable will take
          the value defined in ``_value``. If not defined, the variable will take
          the value defined in ``_value`` for all execution modes.
        - ``_component``: The component for which the variable will take the value
          defined in ``_value``. If not defined, the variable will take the value
          defined in ``_value`` for all components.
        - ``_old_value``: The value of the environment variable before it was
          overwritten with a env_var with attributes. This is set by
          ``esm_parser.dict_merge`` if a given dictionary is been merged with another
          and has an ``_old_value`` key.

        If the ``_component`` and/or ``_execution_mode`` keys are not matched to the
        current component and execution mode, the variable will take the value defined in
        ``_old_value`` (if it exists) instead of the value defined in ``_value``.

        Parameters
        ----------
        env_var_key : str
            The environment variable to be filtered (e.g. ``export_vars``).

        Mutates
        -------
        self.computer : dict
            The EnvironmentInfo ``computer`` dictionary is modified specifically for
            ``env_var_key``, with the selected environment variables.
        """
        env_vars = self.computer[env_var_key]
        component, execution_mode = self.component, self.execution_mode

        def condition_fn(value):
            if isinstance(value, dict) and "_value" in value:
                if (
                    value.get("_execution_mode", execution_mode) == execution_mode
                    and value.get("_component", component) == component
                ):
                    return True
                elif "_old_value" in value:
                    # Delete the _value because Provenance will block its reassignment
                    # otherwise, in some cases
                    del value["_value"]
                    value["_value"] = value["_old_value"]
                    return True
                else:
                    return False
            return True

        env_vars = self._filter_env_vars(env_vars, condition_fn)

        self.computer[env_var_key] = self._flatten_values_with_attrs(env_vars)

    def select_env_vars_based_on_provenance(self, env_var_key):
        """
        If ``merge_component_envs`` is set to ``False``, filters out environment
        variables that do not match the current component using the provenance information.

        Parameters
        ----------
        env_var_key : str
            The environment variable to be filtered (e.g. ``export_vars``).

        Mutates
        -------
        self.computer : dict
            The EnvironmentInfo ``computer`` dictionary is modified specifically for
            ``env_var_key``, with the selected environment variables.
        """
        if self.execution_mode == "run" and not self.computer[
            "merge_component_envs"
        ].get("run", True):
            user_error(
                "Not supported",
                "Selection of component-specific environment during run is not "
                "supported yet.",
            )

        env_vars = self.computer[env_var_key]
        component = self.component
        merge_component_envs = self.computer["merge_component_envs"][
            self.execution_mode
        ]

        if merge_component_envs:
            return

        def condition_fn(value):
            provenance = (
                value.provenance[-1]
                if hasattr(value, "provenance") and value.provenance[-1]
                else None
            )
            return (
                provenance is None
                or provenance["category"] != "components"
                or provenance["subcategory"] == component
            )

        self.computer[env_var_key] = self._filter_env_vars(env_vars, condition_fn)

    def remove_env_vars_from_component_files(self, env_var_key):
        """
        If ``include_env_from_component_files`` is set to ``False``, filters out
        environment variables defined in files in the ``config/components`` directory
        that do not match the current component, using the provenance information.

        Parameters
        ----------
        env_var_key : str
            The environment variable to be filtered (e.g. ``export_vars``).

        Mutates
        -------
        self.computer : dict
            The EnvironmentInfo ``computer`` dictionary is modified specifically for
            ``env_var_key``, with the selected environment variables.
        """
        include_component_env_from_computer = self.computer.get(
            "include_env_from_component_files", True
        )
        env_vars = self.computer[env_var_key]

        def condition_fn(value):
            provenance = (
                value.provenance[-1]
                if hasattr(value, "provenance") and value.provenance[-1]
                else None
            )
            if provenance is None:
                return True
            include_component_env = self.config.get(provenance["subcategory"], {}).get(
                "include_env_from_component_files", include_component_env_from_computer
            )
            return provenance["category"] != "components" or include_component_env

        self.computer[env_var_key] = self._filter_env_vars(env_vars, condition_fn)

    def sort_env_vars(self, env_var_key, category_order):
        """
        Sorts environment variables based upon their order in the original configuration
        files, respecting their provenance.

        Parameters
        ----------
        env_var_key : str
            The environment variable to be sorted (e.g. ``LD_LIBRARY_PATH``)
        category_order : list
            The category priorities to respect, i.e. ``["computer", "component", "setup"].

        Mutates
        -------
        self.computer : dict
           The EnvironmentInfo ``computer`` dictionary is modified specifically for
           ``env_var_key``, with the resorted, provenance-aware order.
        """

        env_vars = self.computer[env_var_key]

        if isinstance(env_vars, dict):
            items = env_vars.items()
            env_vars_type = "dict"
            new_env_vars = esm_parser.DictWithProvenance({}, None)
        elif isinstance(env_vars, list):
            items = enumerate(env_vars)
            env_vars_type = "list"
            new_env_vars = esm_parser.ListWithProvenance([], None)
        else:
            raise ValueError("env_vars must be an instance of dict or list")

        ordering_dict = {}
        for key, value in items:
            category_found = False
            if hasattr(value, "provenance"):
                for category in category_order:
                    for prov in value.provenance:
                        if prov is not None and category == prov["category"]:
                            ordering_dict[category] = ordering_dict.get(category, [])
                            ordering_dict[category].append(
                                (prov["line"], prov["col"], key, value)
                            )
                            category_found = True
                            break
                    if category_found:
                        break

            if not category_found:
                ordering_dict["backend"] = ordering_dict.get("backend", [])
                ordering_dict["backend"].append((None, None, key, value))

        for category in category_order:
            if category not in ordering_dict:
                continue
            ii, jj, keys, values = zip(
                *sorted(ordering_dict[category], key=lambda x: (x[0], x[1]))
            )
            if env_vars_type == "dict":
                for key, value in zip(keys, values):
                    new_env_vars[key] = value
            elif env_vars_type == "list":
                for value in values:
                    new_env_vars.append(value)

        self.computer[env_var_key] = new_env_vars

    def write_dummy_script(self, include_set_e=True):
        """
        Writes a dummy script containing only the header information, module
        commands, and export variables. The actual compile/configure commands
        are added later.

        Parameters
        ----------
        include_set_e : bool
            Default to True, whether or not to include a ``set -e`` at the
            beginning of the script. This causes the shell to stop as soon as
            an error is encountered.
        """
        # Check for sh_interpreter
        if "sh_interpreter" not in self.computer:
            print('WARNING: "sh_interpreter" not defined in the machine yaml')
        with open("dummy_script.sh", "w") as script_file:
            # Write the file headings
            script_file.write(
                f'#!{self.computer.get("sh_interpreter", "/bin/bash")} -l\n'
            )
            script_file.write(
                "# Dummy script generated by esm-tools, to be removed later: \n"
            )
            if include_set_e:
                script_file.write("set -e\n")

            # Write the module and export commands
            for command in self.commands:
                script_file.write(f"{command}\n")
            script_file.write("\n")

    @staticmethod
    def cleanup_dummy_script():
        """
        Removes the ``dummy_script.sh`` if it exists.
        """
        try:
            os.remove("dummy_script.sh")
        except OSError:
            print("No file dummy_script.sh there; nothing to do...")

    @staticmethod
    def add_commands(commands, name):
        """
        Writes all commands in a list to a file named ``<name>_script.sh``,
        located in the current working directory. The header from this script
        is read from ``dummy_script.sh``, also in the current working
        directory.

        Parameters
        ----------
        commands : list of str
            List of the commands to write to the file after the header
        name : str
            Name of the script, generally something like ``comp_echam-6.3.05``

        Returns
        -------
        str :
            ``name`` + "_script.sh"
        """
        if commands:
            with open(f"{name}_script.sh", "w") as newfile:
                with open("dummy_script.sh", "r") as dummy_file:
                    newfile.write(dummy_file.read())
                for command in commands:
                    newfile.write(f"{command}\n")
        return f"{name}_script.sh"

    def output(self):
        self.computer.yaml_dump()


def turn_export_vars_into_dict(config):
    """
    Turns the given ``entry`` in ``componentconfig`` (normally ``add_export_vars``) into
    a dictionary, if it is not a dictionary yet. This function is necessary for
    retro-compatibility of configuration files having ``add_export_vars`` defined as
    list of strings, instead of as dictionaries.

    Parameters
    ----------
    componentconfig : dict
        Information compiled from the `yaml` files for this specific component.
    entry : str
        The environment variable (originally developed for ``add_export_vars``) to
        be turned into a dictionary.
    """
    computer = config.get("computer", {})
    if not computer:
        return

    # Find the variables whose names contains the entry (e.g. add_export_vars)
    path_sep = ","
    entry_paths = esm_parser.find_key(
        computer,
        "export_vars",
        paths2finds=[],
        sep=path_sep,
    )
    # Loop through the variables
    for entry_path in entry_paths:
        # Split the path and define the export_dict dictionary that links to the
        # current entry. Later, if the content of export_dict is a list it will be
        # turned into a dictionary itself
        path_to_var = entry_path.split(path_sep)
        path_to_var = [esm_parser.convert(leaf) for leaf in path_to_var]
        if len(path_to_var) > 1:
            export_dict = esm_parser.find_value_for_nested_key(
                computer,
                path_to_var[-2],
                path_to_var[:-2],
            )
        else:
            export_dict = computer
        # Get the value of export_dict
        export_vars = export_dict[path_to_var[-1]]

        # If export_vars is a list transform it into a dictionary
        if isinstance(export_vars, list):
            user_note(
                "environment behavior deprecated",
                "The ``export_vars`` been a list is deprecated and it won't be "
                "supported in the future. You'll need to turn ``export_vars`` in "
                "@HINT_0@ into a ``dict``. ",
                hints=[
                    {
                        "type": "prov",
                        "object": export_vars,
                        "text": "@HINT@",
                    }
                ],
            )
            env_list_to_dict(export_dict, path_to_var[-1])


def env_list_to_dict(export_dict, key):
    """
    Transforms lists in ``export_dict`` in dictionaries. This allows to add lists of
    ``export_vars`` to the machine-defined ``export_vars`` that should always be a
    dictionary. Note that lists are always added at the end of the ``export_vars``,
    if you want to edit variables of an already existing dictionary make your
    ``export_var`` be a dictionary.

    Avoids destroying repetitions of elements by adding indexes to the keys of the
    newly transformed dictionary, for example:

    .. code-block::yaml
       your_component:
           environment_changes:
               add_export_vars:
                   - 'SOMETHING=dummy'
                   - 'somethingelse=dummy'
                   - 'SOMETHING=dummy'

    The ``export_dict[key]`` (where ``key = add_export_vars``) will be transformed
    in this function from being a list to be the following dictionary:

    .. code-block::yaml
       'SOMETHING=dummy[(0)][(list)]': 'SOMETHING=dummy'
       'somethingelse=dummy[(0)][(list)]': 'somethingelse=dummy'
       'SOMETHING=dummy[(1)][(list)]': "SOMETHING=dummy'

    Note that, once all the environments are resolved, and before writing the
    exports in the bash files, the ``export_vars`` dictionary is transformed again
    into a list and the indexes and ``[(list)]`` strings are removed.

    Parameters
    ----------
    export_dict : dict
        ``export_var`` dictionary which value is a list. This list is transformed
        into a dictionary.
    key : str
        The key to the value.
    """
    # Load the value
    export_vars = export_dict[key]
    # Check if the value is a list TODO: logging
    if not isinstance(export_vars, list):
        print(
            f"The only reason to use this function is if {key} is a list, and it "
            + "is not in this case..."
        )
        sys.exit(1)

    # Loop through the elements of the list
    new_export_vars = esm_parser.DictWithProvenance({}, {})
    for var in export_vars:
        # Initialize index
        index = 0
        while True:
            # If the key with the current index already exists move the move the
            # index forward
            if var + f"[({index})][(list)]" in new_export_vars:
                index += 1
            # If the key with the current index does not exist yet, add the element
            # to the dictionary
            else:
                new_export_vars[f"{var}[({index})][(list)]"] = var
                break

    # Redefined the transformed dictionary
    export_dict[key] = new_export_vars
