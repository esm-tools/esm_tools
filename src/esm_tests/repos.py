import os

from loguru import logger

from .test_utilities import sh


def update_resources_submodule(info, verbose=True):
    """
    Initiates and updates the module ``esm_tests_info``.
    """
    if info["repo_update"]:
        resources_branch = "release"
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
