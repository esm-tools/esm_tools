"""
Syncs pools between two supercomputers
"""
import pathlib
import subprocess

from loguru import logger

from esm_tools import read_config_file


def get_pool_for_machine(machine):
    """Determines the pool directory for a particular HPC.

    Returns
    -------
    Pathlib.path or None :
        The pool directory
    """
    machine_config = read_config_file(f"machines/{machine}")
    try:
        return pathlib.Path(machine_config["pool_dir"])
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


def get_broken_symlinks(pool_dir):
    for link in links_in_pool(pool_dir):
        if not link.exists():
            logger.warning(f"{link} points to non-existing {link.readlink()}")
            yield link

def fix_broken_symlinks(pool_dir, other_machine):
    other_pool_dir = get_pool_for_machine(other_machine)
    for broken_symlink in get_broken_symlinks(pool_dir):
        old_target = broken_symlink.readlink()
        new_target = pathlib.Path(str(old_target).replace(str(other_pool_dir), str(pool_dir)))
        if new_target.exists():
            broken_symlink.unlink()
            broken_symlink.symlink_to(new_target)
            logger.success(f"Relinked {old_target} to {new_target}.")
        else:
            logger.error(f"Unable to determine {new_target} location!")
            logger.error("User interaction is required!")
            break


def main():
    """Main function to sync between two computers"""


if __name__ == "__main__":
    main()
