import urllib.request
import urllib
import sys
import yaml
from time import sleep

import esm_tools
import esm_parser

class motd_handler:
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
        url = 'https://raw.githubusercontent.com/esm-tools/esm_tools/release/motd/motd.yaml'
        try:
            self.motdfile = urllib.request.urlopen(url)
        except urllib.error.HTTPError:
            timeout = 1   # seconds to wait
            #print(f"HTTP Error: Connection to file {url} containing update messages could not be established")
            #print("    Please check the URL by manually...")
            #print(f"    Program will proceed in {timeout} seconds\n")
            #sleep(timeout)
            self.database_connected = False
            self.message_dict = {}
            return
        self.database_connected = True
        self.message_dict = yaml.load(self.motdfile, Loader=yaml.FullLoader)

    def toint(self, versionstring):
        """
        Transforms the version number ``X.Y.Z`` into an integer
        (``X*1000000+Y*1000+Z``) in order to be able to compare version numbers.

        Parameters
        ----------
        versionstring : str
            A string containing version information in the format ``X.Y.Z``.

        Returns
        -------
        number : int
            Integer containing version information in the format
            ``X*1000000+Y*1000+Z``.
        """
        versionparts=[]
        versionparts = versionstring.split(".")
        # Add missing parts
        if len(versionparts) == 1:
            versionparts.append("0")
        if len(versionparts) == 2:
            versionparts.append("0")
        # Transform into integer
        number = int(versionparts[2]) + int(versionparts[1])*1000 + int(versionparts[0])*1000000
        return number

    def check_valid_version(self, version, versionrange):
        """
        Returns ``True`` if the ``version`` provided matches the condition of
        ``versionrange``.

        Parameters
        ----------
        version : str
            String specifying the version number with the format ``X.Y.Z``.
        versionrange : str
            Condition for the version range, expressed as a comparison operator
            followed by a version number in the format ``X.Y.Z``.

        Returns
        -------
        True, False : bool
            ``True`` if the condition is met, ``False`` if not.
        """
        if versionrange.startswith("<="):
            lowerbound = -1
            upperbound = self.toint(versionrange.replace("<=", "").strip())
        if versionrange.startswith("<"):
            lowerbound = -1
            upperbound = self.toint(versionrange.replace("<", "").strip()) - 1
        elif versionrange.startswith(">="):
            lowerbound = self.toint(versionrange.replace(">=", "").strip()) + 1
            upperbound = 1000000000
        elif versionrange.startswith(">"):
            lowerbound = self.toint(versionrange.replace(">", "").strip())
            upperbound = 1000000000
        else:
            upperbound = lowerbound = self.toint(versionrange.replace(">", "").replace("<", "").replace("=", "").strip())

        testnumber = self.toint(version)
        if testnumber > lowerbound and testnumber < upperbound:
            return True
        else:
            return False

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
            waittime = int(action.replace("DELAY", "").replace("(", "").replace(")", "").strip())
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
                    )
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
            if (
                self.message_dict[message]["package"] == mypackage and
                self.check_valid_version(myversion, self.message_dict[message]["versions"])
            ):
                    print("************************************************************************************")
                    print(f"Message found for package {mypackage} version {myversion}:")
                    print()
                    print(self.message_dict[message]["message"])
                    if mypackage=="esm_tools":
                        print("Upgrade ESM-Tools by:")
                        print(f"    cd {esm_tools._get_real_dir_from_pth_file('')}")
                        print("    git pull")
                    # Deprecated after monorepo rework, rewrite if we ever get to have
                    # more than one package again.
                    else:
                        print(f"Upgrade this package by typing:              esm_versions upgrade {mypackage}")
                        print(f"Upgrade all packages by typing:              esm_versions upgrade")
                    print("************************************************************************************")
                    # Perform action
                    thisaction, thistime = self.action_finder(self.message_dict[message]["action"])
                    if thisaction == "error" or thistime > time:
                        action = thisaction
                        time = thistime
                        self.action_handler(action, time, mypackage, myversion)
        return


def check_all_esm_packages():
    """
    Method called by ``esm_master`` and ``esm_runscripts`` (through their ``cli.py``
    files, to check for the message of the day.
    """
    # Instance the ``motd`` object
    motd = motd_handler()
    # Check current version of ESM-Tools
    installed_packages_and_versions = get_version_numbers()
    # Display the MOTD for each package
    for (package, version) in installed_packages_and_versions:
        motd.motd_handler(package, version)
    return


def get_version_numbers():
    """
    Gets the version number of the ESM-Tools packages.
    """
    import importlib
    import pkg_resources

    # Only one package to check after the monorepo changes
    esm_tools_modules = ["esm_tools"]

    installed_packages_and_versions = []
    for tool in esm_tools_modules:
        version = "unknown"
        try:
            tool_mod = importlib.import_module(tool)
            import_successful = True
        except ImportError:
            import_successful = False
        if import_successful:
            try:
                version = tool_mod.__version__
            except AttributeError:
                try:
                    version = pkg_resources.get_distribution(tool).version
                except:
                    raise
        if not version == "unknown":
            installed_packages_and_versions.append((tool, version))
    return installed_packages_and_versions


if __name__ == '__main__':
    mypackage = 'esm_tools'
    myversion = '1.0'

    motd = motd_handler()
    # Uncomment the following lines For testing using the local motd.yaml
    #local_motd = f"{esm_tools._get_real_dir_from_pth_file('')}/motd/motd.yaml"
    #with open(local_motd, "r") as motdfile:
    #    motd.message_dict = yaml.load(motdfile, Loader=yaml.FullLoader)
    motd.motd_handler(mypackage, myversion)
    sys.exit(0)

