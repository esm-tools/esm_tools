"""
Functionality for displaying the version number
"""
import sys
import click
import esm_tools
import esm_tests

# click.version_option read the PKG_INFO which contains the wrong version
# number. Get it directly from __init__.py
version = esm_tools.__version__

@click.version_option(version=version)
@click.option("--test-state", is_flag=True, help="Prints the state of the last tested experiments.")
@click.command()
def main(test_state):
    """Console script for esm_tools"""

    if test_state:
        esm_tests.test_utilities.print_state_online()

    return 0


if __name__ == "__main__":
    sys.exit(main())
