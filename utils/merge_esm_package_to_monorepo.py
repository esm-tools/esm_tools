#!/usr/bin/env python3

import os
import questionary
from loguru import logger


def ask_which_package():
    user_confirms = False
    while not user_confirms:
        esm_tools_packages = os.listdir("../src")
        package_to_update = questionary.select(
            "Which package would you like to update?", choices=esm_tools_packages
        ).ask()
        package_branch = questionary.text("Which branch?").ask()
        user_confirms = questionary.confirm(
            f"You have: {package_to_update}@{package_branch}. Are you sure?",
            default=False,
            auto_enter=False,
        ).ask()
    return package_to_update, package_branch


def clone_package(package_to_update, package_branch):
    logger.info(f"Cloning {package_to_update}@{package_branch} to temporary directory!")
    # NOTE(PG): There are very probably better ways of doing this...
    os.system(
        f"git clone -b {package_branch} {package_to_update} /tmp/{package_to_update}"
    )


def determine_package_differences(package_to_update):
    # Something like: diff package_to_update /tmp/package_to_update
    pass


def resolve_differences(diffs):
    pass


def commit_results(package_to_update):
    pass


def main():
    package_to_update, package_branch = ask_which_package()
    clone_package(package_to_update, package_branch)
    diffs = determine_package_differences(package_to_update)
    resolve_differences(diffs)
    commit_results(package_to_update)


if __name__ == "__main__":
    main()
