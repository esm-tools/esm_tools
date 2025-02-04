""" ESM Framework for organizing python code in plugins / yaml recipes """

import os
import subprocess
import sys

import pkg_resources
from loguru import logger

import esm_parser
import esm_profile
from esm_parser import yaml_file_to_dict


def read_recipe(recipe, additional_dict, needs_parse=True):
    if needs_parse:
        recipe = yaml_file_to_dict(recipe)
    recipe.update(additional_dict)
    esm_parser.basic_choose_blocks(recipe, recipe)
    esm_parser.recursive_run_function(
        [], recipe, "atomic", esm_parser.find_variable, recipe, [], True
    )

    return recipe


def read_plugin_information(plugins_bare, recipe, needs_parse=True):
    plugins = find_installed_plugins(category="core")
    plugins.update(find_installed_plugins(category="external"))
    return plugins


def find_installed_plugins(category="external"):
    if category == "external":
        discovered_plugins = {
            entry_point.name: {
                "plugin_name": entry_point.module_name.split(".")[0],
                "callable": entry_point.load(),
                "type": "installed",
            }
            for entry_point in pkg_resources.iter_entry_points("esm_tools.plugins")
        }
        return discovered_plugins
    if category == "core":
        discovered_plugins = {
            entry_point.name: {
                "module": entry_point.module_name.split(".")[0],
                "submodule": entry_point.module_name.split(".")[1],
                "callable": entry_point.load(),
                "type": "core",
            }
            for entry_point in pkg_resources.iter_entry_points("esm_tools.core_plugins")
        }
        return discovered_plugins
    raise ValueError("Please provide a category of 'external' or 'core'")


def check_plugin_availability(plugins):
    something_missing = False
    for workitem in list(plugins.keys()):
        if plugins[workitem]["type"] == "core":
            pass
        elif plugins[workitem]["type"] == "installed":
            pass
        else:
            logger.info(
                "Checking if function "
                + plugins[workitem]["module"]
                + "."
                + plugins[workitem]["submodule"]
                + "."
                + workitem
                + " can be imported..."
            )
            try:
                if sys.version_info >= (3, 5):
                    import importlib.util

                    spec = importlib.util.spec_from_file_location(
                        plugins[workitem]["module"],
                        plugins[workitem]["location"]
                        + "/"
                        + plugins[workitem]["module"]
                        + ".py",
                    )
                    thismodule = importlib.util.module_from_spec(spec)
                    spec.loader.exec_module(thismodule)
            except:
                logger.error(
                    "Couldn't import "
                    + plugins[workitem]["module"]
                    + " from "
                    + plugins[workitem]["location"]
                )
                something_missing = True
    if something_missing:
        sys.exit(1)


def work_through_recipe(recipe, plugins, config):
    if config.get("general", {}).get("debug_recipe", False):
        import pdb

        pdb.set_trace()
    recipes = recipe["recipe"]
    recipe_name = recipe["job_type"]
    for index, workitem in enumerate(recipes, start=1):
        if config["general"].get("verbose", False):
            # diagnostic message of which recipe step is being executed
            message = (
                f"::: Executing the step:  {workitem}    "
                f"(step [{index}/{len(recipes)}] of the job:  "
                f'{recipe["job_type"]})'
            )

            logger.info("")
            logger.info("=" * len(message))
            logger.info(message)
            logger.info("=" * len(message))
        if plugins[workitem]["type"] in ["core", "installed"]:
            workitem_callable = plugins[workitem]["callable"]
            if config["general"].get("profile", False):
                timed_workitem_callable = esm_profile.timing(
                    workitem_callable, recipe_name
                )
                config = timed_workitem_callable(config)
            else:
                config = workitem_callable(config)
        else:
            if sys.version_info >= (3, 5):
                import importlib.util

                spec = importlib.util.spec_from_file_location(
                    plugins[workitem]["module"],
                    plugins[workitem]["location"]
                    + "/"
                    + plugins[workitem]["module"]
                    + ".py",
                )
                thismodule = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(thismodule)
                if config["general"].get("profile", False):
                    workitem_callable = getattr(thismodule, workitem)
                    timed_workitem_callable = esm_profile.timing(
                        workitem_callable, recipe_name
                    )
                    config = timed_workitem_callable(config)
                else:
                    config = getattr(thismodule, workitem)(config)
    return config


def install(package: str) -> None:
    """
    Checks if a package is already installed in the system and if it's not, then it
    installs it.

    Parameters
    ----------
    package : str
        Name of the package or get operation. Can be a package name (e.g.
        ``numpy``) or a full pip address (e.g.
        ``git@https://github.com/esm-tools/esm_tools.git``)

    Returns
    -------
    None
    """
    package_name = package.split("/")[-1].replace(".git", "")
    installed_entry_points = find_installed_plugins()
    installed_plugins = []
    for entry_point in installed_entry_points:
        this_plugin = installed_entry_points[entry_point]["plugin_name"]
        if this_plugin not in installed_plugins:
            installed_plugins.append(this_plugin)
    arg_list = [sys.executable, "-m", "pip", "install", "--user", package]
    if os.environ.get("VIRTUAL_ENV"):
        arg_list.remove("--user")
    if (
        package_name not in installed_entry_points
        and package_name not in installed_plugins
    ):
        try:
            subprocess.check_call(arg_list)
        except (
            OSError,
            subprocess.CalledProcessError,
        ):  # PermissionDeniedError would be nicer...
            subprocess.check_call(arg_list)


def install_missing_plugins(config: esm_parser.ConfigSetup) -> esm_parser.ConfigSetup:
    """
    Loop through the components and install the missing plugins. This method can be
    called from a recipe.

    Parameters
    ----------
    config : esm_parser.ConfigSetup
        ConfigSetup object containing the information of the current simulation.

    Returns
    -------
    config : esm_parser.ConfigSetup
        ConfigSetup object containing the information of the current simulation.
    """
    if config.get("general", {}).get("install_missing_plugins", True):
        for component in config:
            for plugin in config[component].get("required_plugins", []):
                install(plugin)

    return config
