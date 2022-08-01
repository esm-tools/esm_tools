"""
Syncs pools between two supercomputers
"""
import pathlib
import subprocess

from esm_tools import read_config_file
from loguru import logger


def get_pool_for_machine(machine):
    """Determines the pool directory for a particular HPC.

    Returns
    -------
    Pathlib.path or None :
        The pool directory
    """
    machine_config = read_config_file(machine)
    try:
        return pathlib.Path(machine_config["pool"])
    except KeyError:
        logger.warning(f"No pool defined for {machine}")
        return None


def links_in_pool(pool_dir):
    """Recursively yields all links in the pool.

    Yields
    ------
    Pathlib.path :
        Symlinks in the pool_dir
    """
    for path in pool_dir.iterdir():
        if path.is_symlink():
            yield path
        elif path.is_dir():
            yield from links_in_pool(path)


def determine_broken_symlinks(pool_dir):
    for link in links_in_pool(pool_dir):
        source = link.readlink()
        if not source.exists():
            logger.warning(f"{link} points to non-existing {source}")


def main():
    """Main function to sync between two computers"""


if __name__ == "__main__":
    main()
