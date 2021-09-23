"""
Functionality for listing plugins via the command line.

Example::

    $ esm_plugins

    Plugin 1
    --------
    Docstring of plugin 1

    Plugin 2
    --------
    Docstring of plugin 2

"""
from esm_plugin_manager import find_installed_plugins


def main():
    """The main plugin CLI"""
    discovered_plugins = find_installed_plugins()
    print("The following plugins are installed and available:")
    for plugin_name in discovered_plugins:
        plugin_code = discovered_plugins[plugin_name]["callable"]
        print(plugin_name)
        doc = plugin_code.__doc__
        if doc:
            print("-" * len(plugin_name))
            print(doc)
