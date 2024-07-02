import sys
import urllib
import urllib.request
from time import sleep

import esm_parser
import esm_utilities
import yaml

import esm_tools


class MessageOfTheDayError(Exception):
    """Raise this error when Message of The Day has a problem"""


class MessageOfTheDayHandler:
    """
    Message of the day class. An object from this class loads the MOTD information
    stored in the repository, and includes the methods necessary to decide which MOTD
    to display, based on the version information.
    """

    def __init__(self):
        """
        Object initialization method in charge of loading the MOTD information from the
        repository.
        """
        url = "https://raw.githubusercontent.com/esm-tools/esm_tools/release/motd/motd.yaml"
        try:
            self.motdfile = urllib.request.urlopen(url)
        except (urllib.error.HTTPError, urllib.error.URLError):
            timeout = 1  # seconds to wait
            # print(f"HTTP Error: Connection to file {url} containing update messages could not be established")
            # print("    Please check the URL by manually...")
            # print(f"    Program will proceed in {timeout} seconds\n")
            # sleep(timeout)
            self.database_connected = False
            self.message_dict = {}
            return
        self.database_connected = True
        self.message_dict = yaml.load(self.motdfile, Loader=yaml.FullLoader)

    def action_finder(self, action):
        """
        Interprets the actions specified in the MOTD file.

        Parameters
        ----------
        action : str
            Name of the action.

        Returns
        -------
        (action, option) : tuple
            A tuple containing the name of the action and an option.
        """
        action = action.upper()
        if action.startswith("DELAY"):
            waittime = int(
                action.replace("DELAY", "").replace("(", "").replace(")", "").strip()
            )
            return ("sleep", waittime)
        if action.startswith("ERROR"):
            return ("error", 0)

    def action_handler(self, action, time, package, version):
        """
        Executes the specified action. ``sleep`` and ``error`` actions supported.

        Parameters
        ----------
        action : str
            Name of the action to be performed (``sleep`` or ``error`` supported).
        time : int
            Number of seconds to ``sleep``.
        package : str
            Package name.
        version : str
            Version number.
        """
        if action:
            if action == "sleep":
                sleep(time)
            elif action == "error":
                esm_parser.user_error(
                    "Version",
                    (
                        f"Version {version} of '{package}' package has been tagged as "
                        + "problematic. Please, change version. For more information, "
                        + "please, read the message above."
                    ),
                )

    def motd_handler(self, mypackage, myversion):
        """
        Method to print the required MOTD messages.

        Parameters
        ----------
        mypackage : str
            Name of the ESM package to be checked. Currently only ``esm-tools`` after
            the monorepo rework.
        myversion : str
            Current version of the package in the format ``X.Y.Z``.
        """
        if not self.database_connected:
            return
        action = None
        time = -1
        # Loop through messages
        for message in self.message_dict:
            # If the package for this message is the same as ``mypackage`` and the
            # the version condition is met display the MOTD
            version_range = self.message_dict[message]["versions"]
            version = version_range.strip("<>=!")
            if self.message_dict[message][
                "package"
            ] == mypackage and esm_utilities.check_valid_version(
                version_range, version=myversion
            ):
                print(
                    "************************************************************************************"
                )
                print(f"Message found for package {mypackage} version {myversion}:")
                print()
                print(self.message_dict[message]["message"])
                if mypackage == "esm_tools":
                    esm_tools_path = esm_tools._get_real_dir_from_pth_file("")
                    print(
                        f"Upgrade ESM-Tools to the version contianing this fix (\x1b[96m{version}\x1b[0m) by:\n"
                        f"\x1b[96m1.\x1b[0m \x1b[35mcd {esm_tools_path}\x1b[0m\n"
                        "\x1b[96m2.\x1b[0m Make sure that your git repo is clean (\x1b[35mgit status\x1b[0m)\n"
                        "\x1b[96m3.\x1b[0m \x1b[35mgit checkout release\x1b[0m\n"
                        "\x1b[96m4.\x1b[0m \x1b[35mgit pull\x1b[0m\n"
                    )
                # Deprecated after monorepo rework, rewrite if we ever get to have
                # more than one package again.
                else:
                    print(
                        f"Upgrade this package by typing:              esm_versions upgrade {mypackage}"
                    )
                    print(
                        f"Upgrade all packages by typing:              esm_versions upgrade"
                    )
                print(
                    "************************************************************************************"
                )
                # Perform action
                thisaction, thistime = self.action_finder(
                    self.message_dict[message]["action"]
                )
                if thisaction == "error" or thistime > time:
                    action = thisaction
                    time = thistime
                    self.action_handler(action, time, mypackage, myversion)


def check_all_esm_packages():
    """
    Method called by ``esm_master`` and ``esm_runscripts`` (through their ``cli.py``
    files, to check for the message of the day.
    """
    # Instance the ``motd`` object
    motd = MessageOfTheDayHandler()
    # Check current version of ESM-Tools
    motd.motd_handler("esm_tools", esm_tools.__version__)


def check_esm_package_with_version_and_local_options(
    package, myversion="1.0.0", local=False
):
    """
    Method to check the message of the day for a specific package, version and with an
    option to use a local MOTD file. To be called from the ``esm_tools`` CLI.

    Parameters
    ----------
    package : str
        Name of the package to check the MOTD for.
    myversion : str
        Version of the package to check the MOTD for.
    local : bool
        Option to use the local MOTD file. The default is ``False``.
    """
    mypackage = "esm_tools"

    motd = MessageOfTheDayHandler()
    # For testing using the local motd.yaml
    if local:
        import os

        local_motd = f"{os.path.dirname(__file__)}/../../esm_tools/motd/motd.yaml"
        with open(local_motd, "r") as motdfile:
            motd.message_dict = yaml.load(motdfile, Loader=yaml.FullLoader)
    motd.motd_handler(mypackage, myversion)
    sys.exit(0)
