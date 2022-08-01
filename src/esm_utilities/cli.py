"""Console script for esm_utilities."""
# NOTE(PG): The docstring formatting for click-decorated functions is
# non-standard to make it work better with click.
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
    """Standalone utility functions that may be useful for interacting with esm-tools runs"""
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
    """Utilities to interact with the HPC Pools"""
    return 0

@pool.command()
@click_loguru.init_logger()
@click.argument("machine")
def get_pool_dir(machine):
    """Displays the pool directory.

    Shows which directory the pool_dir (root of the pool is) for MACHINE. If
    nothing is returned, the HPC specified at MACHINE does not have a
    `pool_dir` entry in it's `computer` yaml.
    """
    pool = pool_sync.get_pool_for_machine(machine)
    if pool:
        click.echo(f"{machine}'s pool is at {pool}")
        return 0
    return 1


@pool.command()
@click_loguru.init_logger()
@click.argument("machine")
def get_broken_symlinks(machine):
    """Displays broken symlinks.

    SUMMARY

    Displays which symlinks in the pool for MACHINE point to non-existing
    locations.

    ARGUMENTS

        MACHINE The supercomputer to fix links on

    USAGE

    $ esm_utilities pool get-broken-symlinks ollie
    """
    pool_dir = pool_sync.get_pool_for_machine(machine)
    pool_sync.get_broken_symlinks(pool_dir)
    return 0

@pool.command()
@click_loguru.init_logger()
@click.argument("machine")
@click.argument("other_machine") 
def fix_broken_symlinks(machine, other_machine):
    """Fix links between machines.

    SUMMARY

    Replace potentially problematic links prefixed by the pool for MACHINE
    with the prefix for OTHER_MACHINE. This would mean that on MACHINE,
    you could have a prefix /work/ollie/pool, and on OTHER_MACHINE, you
    would have a prefix /work/data/pool. Any incorrectly labeled links
    would swap out the beginning of their link path from OTHER_MACHINE to
    the correct version in MACHINE.

    ARGUMENTS

        MACHINE The supercomputer to fix links on

        OTHER_MACHINE The supercomputer where potentially broken links
                      originate from

    USAGE

    $ esm_utilities pool fix-broken-symlinks ollie levante
    """
    pool_dir = pool_sync.get_pool_for_machine(machine)
    pool_sync.fix_broken_symlinks(pool_dir, other_machine)
    return 0

if __name__ == "__main__":
    sys.exit(main())  # pragma: no cover
