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


# TODO(PG): This doesn't belong in the cli.py but whatever...
@main.command()
@click.argument("file_name", nargs=1, required=False)
@click.argument("current_year", nargs=1, required=False)
def check_transient_forcing(file_name=None, current_year=None):
    import pandas
    import questionary

    file_name = file_name or questionary.filepath("Select the file to read").ask()
    df = pandas.read_csv(
        file_name,
        sep=";",
        index_col=0,
        header=None,
    )
    current_year = (
        current_year
        or questionary.text(
            "Enter a value for year that you know is in the table"
        ).ask()
    )
    # current_year = int(current_year)
    co2, n2o, ch4, cecc, cobld, clonp = df.loc[current_year]
    print(f"Forcing table was OK to read at year {current_year}")
    print(f"CO2: {co2}")
    print(f"N2O: {n2o}")
    print(f"CH4: {ch4}")
    print(f"CECC: {cecc}")
    print(f"COBLD: {cobld}")
    print(f"CLONP: {clonp}")


if __name__ == "__main__":
    sys.exit(main())
