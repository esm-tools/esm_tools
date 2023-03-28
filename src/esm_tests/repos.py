import os
import questionary

from loguru import logger

from .test_utilities import sh
from esm_parser import user_error


def update_resources_submodule(info, verbose=True):
    """
    Initiates and updates the module ``esm_tests_info``.

    Parameters
    ----------
    info : esm_tests.Info
        Dictionary with the general info about the tests.
    verbose : bool
        Verbose option.
    """

    check_resources(info, verbose)
    resources_branch = info["resources_branch"]

    if info["repo_update"]:
        text = f"Updating the resources repo with '{resources_branch}' branch"

        logger.info(text)
        logger.info("-" * len(text))

        # Initiate the subodule
        os.chdir(info["script_dir"])
        sh("git submodule init", verbose=verbose)
        sh("git submodule update", verbose=verbose)
        os.chdir(f"{info['script_dir']}/resources")

        # Pull the desired branch of resources
        try:
            sh(f"git checkout {resources_branch}")
            sh("git pull")
        except:
            logger.error(f"Pull of {resources_branch} branch not possible")
            sys.exit(1)


def check_resources(info, verbose=True):
    """
    Check if the submodule ``resources`` (``esm_tests_info``) is installed yet, and if
    not, asks the user whether it needs to install it or not. If the user wants to
    install it sets ``info["repo_update"] = True``.

    Parameters
    ----------
    info : esm_tests.Info
        Dictionary with the general info about the tests.
    verbose : bool
        Verbose option.

    Notes
    -----
    Missing resources for ESM-Tests error : esm_parser.user_error
        Returns an ``esm_parser.user_error`` if the user decides not to install the
        submodule.
    """
    resources_folder = f"{info['script_dir']}/resources/"
    dir_resources = os.listdir(resources_folder)
    if not (
        "last_tested" in dir_resources
        and "runscripts" in dir_resources
        and not info.get("repo_update", False)
    ):
        install_resources = questionary.confirm(
            "The resources submodule for esm_tests is not installed yet (i.e. the "
            f"{resources_folder} folder does not include the last_tested or the "
            "runscripts folder). Would you like to install the submodule now?",
        ).ask()
        if install_resources:
            info["repo_update"] = True
        else:
            user_error(
                "Missing resources for ESM-Tests",
                "ESM-Tests needs the esm_tests_info submodule. Please, run "
                "``esm_tests -u`` or accept the installation of the submodule in the "
                "previous questionary.",
            )
        print()


def info_repo():
    """
    Return a git object for the esm_tests_info repo
    """
    pass


def esm_tools_repo():
    """
    Return a git object for the esm_tools repo
    """
    pass


def checkout_info():
    """
    Checks out the release branch in the resources repo and pulls
    """
    pass


def commit_info_changes():
    """
    commits
    """
