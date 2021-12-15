#!/usr/bin/env python
"""
Main module for EsmEnvironment.
"""

import copy
import os
import warnings
import re
import sys

import esm_parser
from esm_rcfile import FUNCTION_PATH

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
    ``module_actions`` and ``export_vars``.

    By instancing the ``EnvironmentInfos`` class, the environment information for
    the specified model or coupled setup is compiled and stored in
    ``self.commands``. If there are environment variables inside the ``general``
    section, ``__init__`` will ignore the environment variables from the standalone
    component files, and it will define the ``general.environment_changes`` for
    each component of the setup.

    Parameters
    ----------
    run_or_compile : str
        A string indicating whether ``EnvironmentInfos`` was instanced from a
        compilation operation (``compiletime``) or a run (``runtime``).
    complete_config : dict
        Dictionary containing all the compiled information from the `yaml` files
        needed for the current `ESM-Tools` operation.
    model : string
        Model for which the environment is required. If not defined, this method
        will loop through all the available keys in ``complete_config``.
    """

    def __init__(self, run_or_compile, complete_config=None, model=None):
        # Ensure local copy of complete config to avoid mutating it... (facepalm)
        complete_config = copy.deepcopy(complete_config)
        # Load computer dictionary or initialize it from the correct machine file
        if complete_config and "computer" in complete_config:
            self.config = complete_config["computer"]
        else:
            self.machine_file = esm_parser.determine_computer_from_hostname()
            self.config = esm_parser.yaml_file_to_dict(self.machine_file)
            esm_parser.basic_choose_blocks(self.config, self.config)
            esm_parser.recursive_run_function(
                [],
                self.config,
                "atomic",
                esm_parser.find_variable,
                self.config,
                [],
                True,
            )

        # Add_s can only be inside choose_ blocks in the machine file
        for entry in ["add_module_actions", "add_export_vars", "add_unset_vars"]:
            if entry in self.config:
                del self.config[entry]

        # Load the general environments if any
        self.general_environment(complete_config, run_or_compile)

        # If the model is defined during the instantiation of the class (e.g.
        # during esm_master with a coupled setup), get the environment for that
        # model. Otherwise, loop through all the keys of the complete_config dictionary
        if model:
            self.apply_config_changes(run_or_compile, complete_config, model)
        else:
            for model in complete_config:
                self.apply_config_changes(run_or_compile, complete_config, model)

        # Add the ENVIRONMENT_SET_BY_ESMTOOLS into the exports
        self.add_esm_var()
        # Define the environment commands for the script
        self.commands = self.get_shell_commands()

    def add_esm_var(self):
        """
        Adds the ENVIRONMENT_SET_BY_ESMTOOLS=TRUE to the config, for later
        dumping to the shell script.
        """

        if "export_vars" in self.config:
            self.config["export_vars"]["ENVIRONMENT_SET_BY_ESMTOOLS"] = "TRUE"
        else:
            self.config["export_vars"] = {"ENVIRONMENT_SET_BY_ESMTOOLS": "TRUE"}

    def apply_config_changes(self, run_or_compile, config, model):
        """
        Calls ``apply_model_changes`` with the selected configuration for the
        ``model``.
        """

        self.apply_model_changes(
            model, run_or_compile=run_or_compile, modelconfig=config[model]
        )

    def apply_model_changes(self, model, run_or_compile="runtime", modelconfig=None):
        """
        Applies the ``environment_changes``, ``compiletime_environment_changes``,
        and/or ``runtime_environment_changes`` to the environment configuration of the
        ``model`` component. Note that ``model`` can be either a component (e.g.
        ``fesom``) or ``general``.

        Parameters
        ----------
        model : str
            Name of the component for which changes will apply.
        run_or_compile : str
            A string indicating whether ``EnvironmentInfos`` was instanced from a
            compilation operation (``compiletime``) or a run (``runtime``).
        modelconfig : dict
            Information compiled from the `yaml` files for this specific component.
        """

        if not modelconfig:
            print("Should not happen anymore...")
            modelconfig = esm_parser.yaml_file_to_dict(
                FUNCTION_PATH + "/" + model + "/" + model
            )

        # Merge whatever is relevant to this environment operation (either compile or
        # run) to ``environment_changes``, taking care of solving possible ``choose_``
        # blocks
        thesechanges = run_or_compile + "_environment_changes"
        if thesechanges in modelconfig:
            # kh 16.09.20 the machine name is already handled here
            # additionally handle different versions of the model (i.e.
            # choose_version...) for each machine if this is possible here in a more
            # generic way, it can be refactored
            if "choose_version" in modelconfig[thesechanges]:
                if "version" in modelconfig:
                    if (
                        modelconfig["version"]
                        in modelconfig[thesechanges]["choose_version"]
                    ):
                        for k, v in modelconfig[thesechanges]["choose_version"][
                            modelconfig["version"]
                        ].items():
                            # kh 16.09.20 move up one level and replace default
                            modelconfig[thesechanges][k] = v
                del modelconfig[thesechanges]["choose_version"]

            # Perform the merging of the environment dictionaries
            if "environment_changes" in modelconfig:
                modelconfig["environment_changes"].update(modelconfig[thesechanges])
            else:
                modelconfig["environment_changes"] = modelconfig[thesechanges]

        if "environment_changes" in modelconfig:
            for entry in ["add_module_actions", "add_export_vars", "add_unset_vars"]:
                # Initialize the environment variables
                if not entry in self.config:
                    if entry in ["add_module_actions", "add_unset_vars"]:
                        self.config[entry] = []
                    elif entry == "add_export_vars":
                        self.config[entry] = {}

                if entry == "add_export_vars":
                    # Transform any list whose name contains add_export_vars into a
                    # dictionary (machine-file export_vars are from now on always a
                    # dictionary but add_export_vars of components and setups are
                    # allowed to be lists for retro-compatibility)
                    self.turn_add_export_vars_to_dict(modelconfig, entry)

            # Merge the ``environment_changes`` into the general ``config``
            self.config.update(modelconfig["environment_changes"])
            # Change any ``choose_computer.*`` block in ``config`` to ``choose_*``
            self.remove_computer_from_choose(self.config)

            # Resolve ``choose_`` blocks
            esm_parser.basic_choose_blocks(self.config, self.config)

            # Remove the environment variables from the config
            for entry in ["add_module_actions", "add_export_vars", "add_unset_vars"]:
                if entry in self.config:
                    del self.config[entry]

    def turn_add_export_vars_to_dict(self, modelconfig, entry):
        """
        Turns the given ``entry`` in ``modelconfig`` (normally ``add_export_vars``) into
        a dictionary, if it is not a dictionary yet. This function is necessary for
        retro-compatibility of configuration files having ``add_export_vars`` defined as
        list of strings, instead of as dictionaries.

        Parameters
        ----------
        modelconfig : dict
            Information compiled from the `yaml` files for this specific component.
        entry : str
            The environment variable (originally developed for ``add_export_vars``) to
            be turned into a dictionary.
        """

        # Find the variables whose names contains the entry (e.g. add_export_vars)
        path_sep = ","
        entry_paths = esm_parser.find_key(
            modelconfig["environment_changes"],
            entry,
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
                    modelconfig["environment_changes"],
                    path_to_var[-2],
                    path_to_var[:-2],
                )
            else:
                export_dict = modelconfig["environment_changes"]
            # Get the value of export_dict
            export_vars = export_dict[path_to_var[-1]]

            # If export_vars is a list transform it into a dictionary
            if isinstance(export_vars, list):
                self.env_list_to_dict(export_dict, path_to_var[-1])

    def env_list_to_dict(self, export_dict, key):
        """
        Transforms lists in ``export_dict`` in dictionaries. This allows to add lists of
        ``export_vars`` to the machine-defined ``export_vars`` that should always be a
        dictionary. Note that lists are always added at the end of the ``export_vars``,
        if you want to edit variables of an already existing dictionary make your
        ``export_var`` be a dictionary.

        Avoids destroying repetitions of elements by adding indexes to the keys of the
        newly transformed dictionary, for example:

        .. code-block::yaml
           your_model:
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
        new_export_vars = {}
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

    def general_environment(self, complete_config, run_or_compile):
        """
        Checks if there are ``environment_changes`` inside the ``general`` section, and
        if that is the case, ignore the changes loaded from the component files.

        Parameters
        ----------
        complete_config : dict
            Dictionary containing all the compiled information from the `yaml` files
            needed for the current `ESM-Tools` operation.
        run_or_compile : str
            A string indicating whether ``EnvironmentInfos`` was instanced from a
            compilation operation (``compiletime``) or a run (``runtime``).
        """

        # If the general section exists load the general environments
        general_env = False
        if "general" in complete_config:
            # Is it a coupled setup?
            coupled_setup = complete_config["general"].get("coupled_setup", False)

            # Check if a general setup environment exists that will overwrite the
            # component setups
            if coupled_setup and (
                "compiletime_environment_changes" in complete_config["general"]
                or "runtime_environment_changes" in complete_config["general"]
                or "environment_changes" in complete_config["general"]
            ):  # TODO: do this if the model include other models and the environment is
                # labelled as priority over the other models environment (OIFS case)
                general_env = True
                self.apply_config_changes(run_or_compile, complete_config, "general")

        # If there is a general environment remove all the model specific environments
        # defined in the model files and preserve only the model specific environments
        # that are explicitly defined in the setup file
        if general_env:
            self.load_component_env_changes_only_in_setup(complete_config)

    def load_component_env_changes_only_in_setup(self, complete_config):
        """
        Removes all the model specific environments defined in the component files and
        preserve only the component-specific environments that are explicitly defined in
        the setup file.

        Parameters
        ----------
        complete_config : dict
            Dictionary containing all the compiled information from the `yaml` files
            needed for the current `ESM-Tools` operation.
        """

        # Get necessary variables
        setup = complete_config.get("general", {}).get("model", None)
        version = str(complete_config.get("general", {}).get("version", None))
        models = complete_config.get("general", {}).get("models", None)
        # Check for errors TODO: logging
        if not models:
            print(
                "Use the EnvironmentInfos.load_component_env_changes_only_in_setup "
                + "method only if complete_config has a general chapter that includes "
                + "a models list"
            )
            sys.exit(1)

        # Find the setup file
        include_path, needs_load = esm_parser.look_for_file(
            setup,
            setup + "-" + version,
        )
        # If setup file not found throw and error TODO: logging
        if not include_path:
            print(f"File for {setup}-{version} not found")
            sys.exit(1)
        # Load the file TODO: logging
        if needs_load:
            setup_config = esm_parser.yaml_file_to_dict(include_path)
        else:
            print(f"A setup needs to load a file so this line shouldn't be reached")
            sys.exit(1)

        # Add the attachment files (e.g. the environment variables can be in a
        # further_reading file)
        for attachment in esm_parser.CONFIGS_TO_ALWAYS_ATTACH_AND_REMOVE:
            # Add the attachment file chapters (e.g. there is a further_reading chapter
            # at the same level of general and the components)
            esm_parser.attach_to_config_and_remove(setup_config, attachment)
            # Add the attachment files in each chapter (i.e. in general, components,
            # etc.)
            for component in list(setup_config):
                esm_parser.attach_to_config_and_remove(
                    setup_config[component],
                    attachment,
                )

        # Define the possible environment variables
        environment_vars = [
            "environment_changes",
            "compiletime_environment_changes",
            "runtime_environment_changes",
        ]
        # Loop through the models
        for model in models:
            # Sanity check TODO: logging
            if model not in complete_config:
                print(f"The chapter {model} does not exist in complete_config")
                sys.exit(1)
            # Load the configuration of this model
            model_config = complete_config[model]
            # Loop through the possible environment variables
            for env_var in environment_vars:
                # If the environment variable exists replace it with the one defined in
                # the setup file for that model:
                # 1. Delete the variable
                if env_var in model_config:
                    del model_config[env_var]
                # 2. Redefine the variable
                if env_var in setup_config.get(model, {}):
                    # Solve any unresolved variables in the reloaded setup environment
                    # TODO: change this to  be out of the loop using the method
                    # ``model_config.finalize()``, currently not working due to
                    # a problem with the dates
                    esm_parser.recursive_run_function(
                        [],
                        setup_config[model][env_var],
                        "atomic",
                        esm_parser.find_variable,
                        complete_config,
                        {},
                        {},
                    )
                    # Actually redefine the variable
                    model_config[env_var] = setup_config[model][env_var]

    def replace_model_dir(self, model_dir):
        """
        Replaces any instances of ${model_dir} in the config section
        "export_vars" with the argument

        Parameters
        ----------
        model_dir : str
            The replacement string for ${model_dir}
        """
        for entry in ["export_vars"]:
            if entry in self.config:
                newlist = []
                for line in self.config[entry]:
                    newline = line.replace("${model_dir}", model_dir)
                    newlist.append(newline)
                self.config[entry] = newlist

    def get_shell_commands(self):
        """
        Gathers module actions and export variables from the config to a list,
        prepending appropriate shell command words (e.g. module and export).

        If the ``export_vars`` dictionary contains variables with repetition
        indexes (``[(int)]``) or ``[(list)]``, those are removed before returning the
        command list.

        Returns
        -------
        environment : list
            A list of the environment operations, to be used in the compilation and run
            scripts.
        """

        environment = []
        # Write module actions
        if "module_actions" in self.config:
            for action in self.config["module_actions"]:
                # seb-wahl: workaround to allow source ... to be added to the batch header
                # until a proper solution is available. Required with FOCI
                if action.startswith("source"):
                    environment.append(action)
                else:
                    environment.append(f"module {action}")
        # Add an empty string as a newline:
        environment.append("")
        if "export_vars" in self.config:
            for var in self.config["export_vars"]:
                # If export_vars is a dictionary
                if isinstance(self.config["export_vars"], dict):
                    # If the variable is a dictionary itself (e.g. "AWI_FESOM_YAML"
                    # in fesom-1.4) add the contents of the dictionary as the value of
                    # the exported variable inside '""'
                    if isinstance(self.config["export_vars"][var], dict):
                        key = var
                        value = self.config["export_vars"][key]
                        environment.append(f"export {key}='{str(value)}'")
                    # If the value of the variable is not a dictionary
                    else:
                        key = var
                        value = self.config["export_vars"][key]
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
        if "unset_vars" in self.config:
            for var in self.config["unset_vars"]:
                environment.append(f"unset {var}")

        return environment

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
        if "sh_interpreter" not in self.config:
            print('WARNING: "sh_interpreter" not defined in the machine yaml')
        with open("dummy_script.sh", "w") as script_file:
            # Write the file headings
            script_file.write(
                f'#!{self.config.get("sh_interpreter", "/bin/bash")} -l\n'
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

    def remove_computer_from_choose(self, chapter):
        """
        Recursively remove ``computer.`` from all the `choose_` keys.

        Parameters
        ----------
        chapter : dict
            Dictionary to search for ``choose_computer.`` blocks.
        """
        all_keys = list(chapter.keys())
        for key in all_keys:
            if isinstance(key, str) and "choose_computer." in key:
                newkey = key.replace("computer.", "")
                chapter[newkey] = chapter[key]
                del chapter[key]
                key = newkey
            if isinstance(chapter[key], dict):
                self.remove_computer_from_choose(chapter[key])

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
        esm_parser.pprint_config(self.config)


class environment_infos(EnvironmentInfos):
    def __init__(self, *args, **kwargs):
        warnings.warn(
            "Please change your code to use EnvironmentInfos!",
            DeprecationWarning,
            stacklevel=2,
        )
        super(environment_infos, self).__init__(*args, **kwargs)
