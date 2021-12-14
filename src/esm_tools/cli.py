"""
Functionality for displaying the version number
"""
import sys
import click


@click.version_option()
@click.group()
def main(args=None):
    """Console script for esm_tools"""
    return 0


if __name__ == "__main__":
    sys.exit(main())
