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
    # pluginfile = esm_plugins.yaml
    if needs_parse:
        plugins_bare = yaml_file_to_dict(plugins_bare)
    extra_info = ["location", "git-url"]
    plugins = {}
    for workitem in recipe["recipe"]:
        found = False
        for module_type in ["core", "plugins"]:
            if module_type in plugins_bare:
                for module in plugins_bare[module_type]:
                    for submodule in plugins_bare[module_type][module]:
                        if submodule in extra_info:
                            continue
                        functionlist = plugins_bare[module_type][module][submodule]
                        if workitem in functionlist:
                            plugins[workitem] = {
                                "module": module,
                                "submodule": submodule,
                                "type": module_type,
                            }
                            for extra in extra_info:
                                if extra in plugins_bare[module_type][module]:
                                    plugins[workitem].update(
                                        {
                                            extra: plugins_bare[module_type][module][
                                                extra
                                            ]
                                        }
                                    )
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
    if config.get("general", {}).get("debug_recipe", False):
        import pdb

        pdb.set_trace()
    recipes = recipe["recipe"]
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
