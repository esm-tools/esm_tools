""" ESM Framework for organizing python code in plugins / yaml recipes """

import sys
import esm_parser
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
    """
    Reads in plugin information from the pluginfile = esm_plugins.yaml

    Parameters
    ----------
    plugins_bare : dict
        Dictionary as it is read in by function ``read_recipe``
    recipe : dict
        Dictionary of all workitems of a recipe
    needs_parse : bool
        True (default) or False

    Returns
    -------
    plugins : dict
        Dictionary that has information for each workitem of the recipe:
        - module: e.g. esm_runscripts
        - submodule: e.g. prepare (this is the Python file where the workitem function is defined.
        - type: e.g. core
    """
    if needs_parse:
        plugins_bare = yaml_file_to_dict(plugins_bare)
    extra_info = ["location", "git-url"]
    plugins = {}
    # loop over all recipe entries
    # tries to find workitem in 'plugins_bare'
    for workitem in recipe["recipe"]:
        found = False
        for module_type in ["core", "plugins"]:
            if module_type in plugins_bare:
                for module in plugins_bare[module_type]:
                    for submodule in plugins_bare[module_type][module]:
                        if submodule in extra_info:
                            continue
                        # functionlist is a list of workitems (Python function names)
                        functionlist = plugins_bare[module_type][module][submodule]
                        # if the workitem of the recipe is found in this list
                        # the dictionary plugins will be filled with fields for
                        # - 'module' (e.g. esm_runscirpts)
                        # - 'submodule' (e.g. prepare, this is basically the name
                        #               of the python file this function is defined in)
                        # - 'type' (core of plugin)
                        if workitem in functionlist:
                            plugins[workitem] = {
                                "module": module,
                                "submodule": submodule,
                                "type": module_type,
                            }
                            # add extra info ["location", "git-url"] if found in plugins_bare dict
                            # is there a use case for this?
                            for extra in extra_info:
                                if extra in plugins_bare[module_type][module]:
                                    plugins[workitem].update(
                                        {
                                            extra: plugins_bare[module_type][module][
                                                extra
                                            ]
                                        }
                                    )
                            # if workitem is found, all loops including loop over module_type can be aborted.
                            found = True
                            break
                    if found:
                        break
                if found:
                    break
            if found:
                break

    attach_installed_plugins_to_all(plugins)
    return plugins


def find_installed_plugins():
    import pkg_resources

    discovered_plugins = {
        entry_point.name: {"callable": entry_point.load(), "type": "installed"}
        for entry_point in pkg_resources.iter_entry_points("esm_tools.plugins")
    }
    return discovered_plugins


def attach_installed_plugins_to_all(plugins):
    plugins.update(find_installed_plugins())


def check_plugin_availability(plugins):
    something_missing = False
    for workitem in list(plugins.keys()):
        if plugins[workitem]["type"] == "core":
            pass
        elif plugins[workitem]["type"] == "installed":
            pass
        else:
            print(
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
                print(
                    "Couldn't import "
                    + plugins[workitem]["module"]
                    + " from "
                    + plugins[workitem]["location"]
                )
                something_missing = True
    if something_missing:
        sys.exit(-1)


def work_through_recipe(recipe, plugins, config):
    """
    Works through the esm_runscripts recipes and plugin recipes.

    Arguments:
        recipe -- dictionary            # What is in these two dictionaries? Where do the entries are comming from?
        plugins -- dictionary
        config -- dictionary

    Returns:
        config
    """
    if config.get("general", {}).get("debug_recipe", False):
        import pdb

        pdb.set_trace()
    recipes = recipe["recipe"]
    # Loop over the recipe
    for index, workitem in enumerate(recipes, start=1):
        if config["general"].get("verbose", False):
            # diagnostic message of which recipe step is being executed
            message = (
                f"::: Executing the step:  {workitem}    "
                f"(step [{index}/{len(recipes)}] of the job:  "
                f'{recipe["job_type"]})'
            )

            print()
            print("=" * len(message))
            print(message)
            print("=" * len(message))
        if plugins[workitem]["type"] == "core":
            thismodule = __import__(plugins[workitem]["module"])
            submodule = getattr(thismodule, plugins[workitem]["submodule"])
            config = getattr(submodule, workitem)(config)
        elif plugins[workitem]["type"] == "installed":
            # print("Installed plugin will be run: ", workitem)
            config = plugins[workitem]["callable"](config)
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
                config = getattr(thismodule, workitem)(config)
    return config
