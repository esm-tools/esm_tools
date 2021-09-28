"""Console script for esm_utilities."""
import sys
import click

from . import utils


@click.group()
def main(args=None):
    """Console script for esm_utilities."""
    click.echo(
        "Replace this message by putting your code into " "esm_utilities.cli.main"
    )
    click.echo("See click documentation at https://click.palletsprojects.com/")
    return 0


@main.command()
@click.argument("f")
def logfile_stats(f):
    utils.logfile_stats(f)


if __name__ == "__main__":
    sys.exit(main())  # pragma: no cover
