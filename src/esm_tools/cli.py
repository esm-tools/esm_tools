"""
Functionality for displaying the version number
"""
import sys
import click
import esm_tools

# click.version_option read the PKG_INFO which contains the wrong version 
# number. Get it directly from __init__.py
version = esm_tools.__version__

@click.version_option(version=version)
@click.group()
def main(args=None):
    """Console script for esm_tools"""
    return 0


if __name__ == "__main__":
    sys.exit(main())
