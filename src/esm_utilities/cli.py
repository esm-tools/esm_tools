"""Console script for esm_utilities."""
import sys

import click
from click_loguru import ClickLoguru

from . import __version__, pool_sync, utils

VERSION = __version__
NAME = "esm_utilities"

click_loguru = ClickLoguru(NAME, VERSION)

@click_loguru.logging_options
@click.group()
@click.version_option(version=VERSION, prog_name=NAME)
def main(verbose, quiet, logfile, profile_mem,):
    """Console script for esm_utilities."""
    unused_str = (
        f"verbose: {verbose} quiet: {quiet}"
        + f" logfile: {logfile} profile_mem: {profile_mem}"
    )
    return 0


@main.command()
@click.argument("f")
def logfile_stats(f):
    utils.logfile_stats(f)

@main.group()
def pool(args=None):
    return 0

@pool.command()
@click_loguru.init_logger()
@click.argument("machine")
def get_pool_dir(machine):
    print(pool_sync.get_pool_for_machine(machine))
    return 0


@pool.command()
@click_loguru.init_logger()
@click.argument("machine")
def get_broken_symlinks(machine):
    pool_dir = pool_sync.get_pool_for_machine(machine)
    pool_sync.get_broken_symlinks(pool_dir)
    return 0

@pool.command()
@click_loguru.init_logger()
@click.argument("machine")
@click.argument("other_machine")
def fix_broken_symlinks(machine, other_machine):
    pool_dir = pool_sync.get_pool_for_machine(machine)
    pool_sync.fix_broken_symlinks(pool_dir, other_machine)
    return 0

if __name__ == "__main__":
    sys.exit(main())  # pragma: no cover
