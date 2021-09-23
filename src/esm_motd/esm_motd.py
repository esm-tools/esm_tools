import urllib.request
import urllib
import sys
import yaml
from time import sleep

class motd_handler:
    def __init__(self):
        url = 'https://raw.githubusercontent.com/esm-tools/esm_tools/release/esm_tools/motd/motd.yaml'
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
        versionparts=[]
        versionparts = versionstring.split(".")
        if len(versionparts) == 1:
            versionparts.append("0")
        if len(versionparts) == 2:
            versionparts.append("0")
        number = int(versionparts[2]) + int(versionparts[1])*1000 + int(versionparts[0])*1000000
        return number

    def check_valid_version(self, my, versionrange):
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

        testnumber = self.toint(my)
        if testnumber > lowerbound and testnumber < upperbound:
            return True
        else:
            return False

    def action_finder(self, action):
        action = action.upper()
        if action.startswith("DELAY"):
            waittime = int(action.replace("DELAY", "").replace("(", "").replace(")", "").strip())
            return ("sleep", waittime)
        if action.startswith("ERROR"):
            return ("error", 0)


    def action_handler(self, action, time):
        if action:
            if action == "sleep":
                sleep(time)
            elif action == "error":
                print("Can't work under these circumstances, exiting...")
                sys.exit(-1)


    def motd_handler(self, mypackage, myversion):
        if not self.database_connected:
            return
        action = None
        time = -1
        for message in self.message_dict:
            if self.message_dict[message]["package"] == mypackage and \
               self.check_valid_version(myversion, self.message_dict[message]["versions"]):
                   print ("************************************************************************************")
                   print(f"Message found for package {mypackage} version {myversion}:")
                   print()
                   print (self.message_dict[message]["message"])
                   print (f"Upgrade this package by typing:              esm_versions upgrade {mypackage}")
                   print (f"Upgrade all packages by typing:              esm_versions upgrade")
                   print ("************************************************************************************")
                   thisaction, thistime = self.action_finder(self.message_dict[message]["action"])
                   if thisaction == "error" or thistime > time: 
                       action = thisaction
                       time = thistime
        self.action_handler(action, time)
        return


def check_all_esm_packages():
    motd = motd_handler()
    installed_packages_and_versions = get_version_numbers()
    for (package, version) in installed_packages_and_versions:
        motd.motd_handler(package, version)
    return


def get_version_numbers():
    import importlib
    import pkg_resources
    from esm_version_checker import get_esm_packages  

    # get the list of ESM-Tools as a list of strings
    esm_tools_modules = get_esm_packages()
    
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
    mypackage = 'esm_motd'
    myversion = '1.0'

    motd = motd_handler()
    motd.motd_handler(mypackage, myversion)
    sys.exit(0)

