"""
Functionality for listing plugins via the command line.

Example::

    $ esm_plugins ls external

    Plugin 1
    --------
    Docstring of plugin 1

    Plugin 2
    --------
    Docstring of plugin 2

"""
import sys

import click

from esm_plugin_manager import find_installed_plugins


@click.group
def main():
    """The main plugin CLI"""


@main.command()
@click.argument("category")
def ls(category):
    """List plugins installed"""
    if category in ["core", "external"]:
        discovered_plugins = find_installed_plugins(category=category)
        print(f"The following plugins are installed and available ({category}):")
        for plugin_name in discovered_plugins:
            plugin_code = discovered_plugins[plugin_name]["callable"]
            print(plugin_name)
            doc = plugin_code.__doc__
            if doc:
                print("-" * len(plugin_name))
                print(doc)
        sys.exit(0)
    else:
        print("Please use esm_plugins ls [external, core]!")
        sys.exit(1)
