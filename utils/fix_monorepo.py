"""
Script that cleans the multirepo packages of ESM-Tools (<v6.0.0). Run this script
if you suspect that you are having a conflict between a newly installed ESM-Tools
version >5.1.9 and some older packages::

    python3 utils/fix_monorepo.py

This script is based on the script used to upgrade from 5.1.9 yo 6.0.0:
https://github.com/esm-tools/esm_version_checker/blob/release/esm_version_checker/monorepo.py
"""

import colorama
import glob
import os
import pkg_resources
import questionary
import shutil
import site
import subprocess
import sys

import regex as re


def install_monorepo(esm_tools, version):
    """
    Does all the magic for successfully installing the monorepo if the user has the
    multirepo already installed.
    """
    _, columns = os.popen("stty size", "r").read().split()
    columns = int(columns)

    # Packages from multirepo
    packages = [
        "esm_calendar",
        "esm_database",
        "esm_environment",
        "esm_master",
        "esm_motd",
        "esm_parser",
        "esm_plugin",
        "esm_profile",
        "esm_rcfile",
        "esm_runscripts",
        "esm_tools",
        "esm_version",
    ]

    tools_dir, bin_dir, lib_dirs = find_dir_to_remove(packages)
    os.chdir(tools_dir)

    steps = {
        "uninstall": "Uninstall packages (``pip uninstall esm_<package>``)",
        "esm_tools": f"Cleanup ``{tools_dir}/esm_tools`` folder",
        "esm_tools.egg-info": f"Cleanup ``{tools_dir}/esm_tools.egg-info`` folder",
        "rm_libs": ["Remove esm_<packages> from python libraries in:"] + lib_dirs,
        "rm_bins": f"Remove esm_<packages> in the bin folder (``rm {bin_dir}/esm_<package>``)",
        "rm_easy": ["Remove ESM lines in the ``easy-install.pth`` files:"] + lib_dirs,
        "install": "Install ``ESM-Tools`` again",
    }

    # Printing and questionary
    text = (
        "**Welcome to the monorepository version of ESM-Tools!**\n"
        "\n"
        f"You are trying to upgrade to the major version ``6`` which does "
        "not use multiple repositories for different ``esm_<packages>`` anymore, "
        "but instead all packages are contained in the ``esm_tools`` package (i.e. "
        "esm_runscripts, esm_parser, esm_master, etc). You can find these packages "
        "now in ``esm_tools/src/``.\n"
        "\n"
        "Also note that you won't be able to use ``esm_versions`` command from now "
        "on, as this tool is not needed anymore for the monorepository, and it has "
        "been consequently removed."
    )

    cprint()
    cprint("**" + columns * "=" + "**")
    cprint(text)
    cprint("**" + columns * "=" + "**")

    cprint(
        "The monorepository version needs a special installation. "
        "ESM-Tools will perform the next steps:"
    )

    c = 1
    for key, value in steps.items():
        if isinstance(value, list):
            cprint(f"``{c}`` - {value[0]}")
            for substeps in value[1:]:
                cprint(f"\t- {substeps}")
        else:
            cprint(f"``{c}`` - {value}")
        c += 1

    user_confirmed = False
    while not user_confirmed:
        response = questionary.select(
            "Would you like to continue?", choices=(["[Quit] No, thank you...", "Yes!"])
        ).ask()  # returns value of selection
        if "[Quit]" in response and (version=="release" or version=="develop"):
            # If the user refuses to install the monorepo bring back esm_tools to the
            # last multirepo compatible version.
            v = "v5.1.25"
            if not version == "monorepo":
                p = subprocess.check_call(
                    f"git reset {v}",
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    shell=True,
                )
            sys.exit(1)
        elif "[Quit]" in response:
            sys.exit(1)
        user_confirmed = questionary.confirm("Are you sure?").ask()

    p = subprocess.check_call(
        f"git checkout release",
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        shell=True,
    )
    p = subprocess.check_call(
        f"git pull",
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        shell=True,
    )

    # Dirty fix for installing the monorepo branch for testing previous version 6 is
    # around. Not used when the monorepo is in release
    if version == "monorepo":
        subprocess.check_call(["git", "checkout", "monorepo"])
        subprocess.check_call(["git", "pull"])

    cprint()

    # Uninstall packages
    c = 1
    cprint(f"**{c}** - {steps['uninstall']}")
    for package in packages:
        uninstall(package)

    # Cleanup esm_tools folder
    clean_folders = ["esm_tools", "esm_tools.egg-info"]
    for cf in clean_folders:
        if cf=="esm_tools":
            cf = "esm_tools/__init__.py"
        f = f"{tools_dir}/{cf}"
        if os.path.isdir(f):
            c += 1
            cprint(f"**{c}** - {steps[cf]}")
            shutil.rmtree(f)

    # Remove libs
    c += 1
    cprint(f"**{c}** - {steps['rm_libs'][0]}")
    for lib_dir in lib_dirs:
        for package in packages:
            package_files = glob.glob(f"{lib_dir}/{package}*")
            for f in package_files:
                cprint(f"\tRemoving ``{f}``")
                if os.path.isdir(f):
                    shutil.rmtree(f)
                else:
                    os.remove(f)

    # Remove bins
    c += 1
    cprint(f"**{c}** - {steps['rm_bins']}")
    for package in packages:
        bin_file = glob.glob(f"{bin_dir}/{package}*")
        if bin_file:
            bin_file = bin_file[0]
            if os.path.isfile(bin_file):
                cprint(f"\tRemoving ``{bin_file}``")
                subprocess.run(["rm", "-f", bin_file])

    # Clean ``easy-install.pth``
    c += 1
    cprint(f"**{c}** - {steps['rm_easy'][0]}")
    clean_easy_install(lib_dirs, packages)

    # Install the tools
    c += 1
    cprint(f"**{c}** - {steps['install']}")
    p = subprocess.Popen(
        f"cd {tools_dir} && pip install --user -e .",
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        shell=True,
    )
    out, err = p.communicate()

    if not err:
        cprint(f"**Version 6 installed sucessfully!**")
        sys.exit(0)
    elif "ERROR" in err.decode("utf-8"):
        print(out.decode("utf-8"))
        print(err.decode("utf-8"))
        cprint("--Installation failed!--")
        sys.exit(1)
    elif "WARNING" in err.decode("utf-8"):
        print(err.decode("utf-8"))
        cprint(f"**Version 6 installed sucessfully with warnings!**")
        sys.exit(0)
    else:
        print(out.decode("utf-8"))
        print(err.decode("utf-8"))
        cprint("--Installation failed!--")
        sys.exit(1)


def uninstall(package):
    """
    Taken from https://stackoverflow.com/questions/35080207/how-to-pass-the-same-answer-to-subprocess-popen-automatically
    """
    cprint(f"\tUninstalling ``{package}``")
    process = subprocess.Popen(
        [sys.executable, "-m", "pip", "uninstall", package],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    yes_proc = subprocess.Popen(["yes", "y"], stdout=process.stdin)
    process_output = process.communicate()[0]
    yes_proc.wait()


def find_dir_to_remove(packages):
    path_to_dists = "/".join(site.getusersitepackages().split("/")[:-2])
    python_dist_libs = [x for x in os.listdir(f"{path_to_dists}/") if "python" in x]

    lib_dirs = [f"{path_to_dists}/{x}/site-packages" for x in python_dist_libs]
    bin_dir = "/".join(path_to_dists.split("/")[:-1] + ["bin"])
    tools_dir = pkg_resources.get_distribution("esm_tools").location
    if tools_dir.endswith("esm_tools/src"):
        tools_dir = tools_dir.replace("esm_tools/src", "esm_tools")

    return tools_dir, bin_dir, lib_dirs


def clean_easy_install(lib_dirs, packages):
    for ld in lib_dirs:
        easy_install_file = f"{ld}/easy-install.pth"
        if os.path.isfile(easy_install_file):
            cprint(f"\tCleaning ``{easy_install_file}``")
            with open(easy_install_file, "r+") as f:
                lines = f.readlines()
                f.seek(0)
                for line in lines:
                    contains_package = False
                    for package in packages:
                        if package in line:
                            contains_package = True
                    if contains_package:
                        continue
                    else:
                        f.write(line)
                f.truncate()


def cprint(text=""):
    # Bold strings
    bs = "\033[1m"
    be = "\033[0m"
    reset_s = colorama.Style.RESET_ALL
    title_color = colorama.Fore.CYAN
    error_color = colorama.Fore.RED
    remarks_color = colorama.Fore.MAGENTA

    text = re.sub("\*\*([^*]*)\*\*", f"{bs}{title_color}\\1{reset_s}{be}", text)
    text = re.sub("``([^`]*)``", f"{remarks_color}\\1{reset_s}", text)
    text = re.sub("--([^-]*)--", f"{error_color}\\1{reset_s}", text)
    print(text)

if __name__ == "__main__":
    install_monorepo("", "")
