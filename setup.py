#!/usr/bin/env python

"""The setup script."""

from setuptools import setup, find_packages
from os import getenv

with open("README.rst") as readme_file:
    readme = readme_file.read()

with open("stuff/HISTORY.rst") as history_file:
    history = history_file.read()

requirements = [
    "esm_master @ git+https://gitlab.awi.de/esm_tools/esm_master.git",
    "esm_runscripts @ git+https://gitlab.awi.de/esm_tools/esm_runscripts.git",
    "esm_rcfile @ git+https://gitlab.awi.de/esm_tools/esm_rcfile.git",
    "esm_version_checker @ git+https://gitlab.awi.de/esm_tools/esm_version_checker.git",
]

setup_requirements = []

test_requirements = []

thisfolder = getenv("PWD")  # that somehow works, even though pip copies everything to
# a temp folder... seemingly without changing the PWD var
setup(
    author="Dirk Barbi",
    author_email="dirk.barbi@awi.de",
    python_requires=">=3.5",
    classifiers=[
        "Development Status :: 3 - Beta",
        "Intended Audience :: Science/Research",
        "License :: OSI Approved :: GNU General Public License v2 (GPLv2)",
        "Natural Language :: English",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.5",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
    ],
    description="ESM Tools external infrastructure for Earth System Modelling",
    install_requires=requirements,
    license="GNU General Public License v2",
    long_description=readme + "\n\n" + history,
    include_package_data=True,
    keywords="esm_tools",
    name="esm_tools",
    packages=find_packages(include=["esm_tools", "esm_tools.*"]),
    setup_requires=setup_requirements,
    test_suite="tests",
    tests_require=test_requirements,
    url="https://gitlab.awi.de/esm_tools/esm_tools",
    version="version='3.1.4'",
    zip_safe=False,
)

try:
    from esm_rcfile import set_rc_entry

    set_rc_entry("FUNCTION_PATH", thisfolder + "/configs")
    set_rc_entry("NAMELIST_PATH", thisfolder + "/namelists")
    set_rc_entry("RUNSCRIPT_PATH", thisfolder + "/runscripts")
except:
    print("RCFile could not be written!")
