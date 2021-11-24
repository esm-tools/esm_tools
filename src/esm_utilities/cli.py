"""Console script for esm_utilities."""
import sys
import click

from . import utils
import esm_utilities.bugs as model_bugs

@click.group()
def main(args=None):
    """Console script for esm_utilities."""
    #FIXME(PG): How to get this guy to only show up if no other bug is given...
    click.echo("Please use the command logfile-stats or bugs. With -h or --help, you can get some more info")
    return 0

@main.command()
@click.argument("f")
def logfile_stats(f):
    utils.logfile_stats(f)

@main.command()
@click.argument("bug_name")
@click.option("--help", is_flag=True)
@click.option("--check", is_flag=True)
def bugs(bug_name, help=False, check=False):
    available_bugs = [bug.replace("fix_for_", "") for bug in dir(model_bugs) if bug.startswith("fix_for")]
    if bug_name not in available_bugs:
        click.echo(f"Sorry, {bug_name} does not exist. Please use:")
        [click.echo(f"* {bug}") for bug in available_bugs]
        return 1
    if help:
        docstring = getattr(model_bugs, f"check_for_{bug_name}").__doc__
        click.echo(docstring)
        return 0
    if check:
        checker = getattr(model_bugs, f"check_for_{bug_name}")
        return checker.main()
    else:
        checker = getattr(model_bugs, f"fix_for_{bug_name}")
        return checker.main()


if __name__ == "__main__":
    sys.exit(main())  # pragma: no cover
