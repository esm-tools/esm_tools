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

import rich_click as click

from esm_plugin_manager import find_installed_plugins


@click.command()
def main():
    """List all installed plugins."""
    discovered_plugins = find_installed_plugins()
    click.echo("The following plugins are installed and available:")
    for plugin_name in discovered_plugins:
        plugin_code = discovered_plugins[plugin_name]["callable"]
        click.echo(plugin_name)
        doc = plugin_code.__doc__
        if doc:
            click.echo("-" * len(plugin_name))
            click.echo(doc)
