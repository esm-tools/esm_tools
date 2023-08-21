"""
Functionality for displaying the version number
"""
import shutil
import sys

import click

import esm_tests
import esm_tools

# click.version_option read the PKG_INFO which contains the wrong version
# number. Get it directly from __init__.py
version = esm_tools.__version__


@click.group()
@click.version_option(version=version)
def main():
    pass


@main.command()
def test_state():
    """Prints the state of the last tested experiments."""

    esm_tests.test_utilities.print_state_online()

    return 0


@main.command()
@click.option(
    "-t",
    "--type",
    type=click.Choice(["component", "setup", "machine"], case_sensitive=False),
    help="Creates either a new component (default) or a new setup",
    default="component",
    show_default=True,
)
@click.argument("name", nargs=1)
def create_new_config(name, type):
    """Opens your $EDITOR and creates a new file for NAME"""
    click.echo(f"Creating a new {type} configuration for {name}")
    template_file = esm_tools.get_config_filepath(
        config=f"templates/{type}_template.yaml"
    )
    shutil.copy(template_file, f"{name}.yaml")
    new_config_file = f"{name}.yaml"
    # TODO(PG): Currently this lands in the current working directory.
    # Would be nice if this landed already in an git-controlled
    # editable version and prepared a commit for you:
    click.edit(filename=new_config_file)
    click.echo(
        "Thank you! The new configuration has been saved. Please commit it (and get in touch with the"
    )
    click.echo("esm-tools team if you need help)!")
    return 0


if __name__ == "__main__":
    sys.exit(main())
