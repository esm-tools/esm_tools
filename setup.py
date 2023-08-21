#!/usr/bin/env python

"""The setup script."""

from os import getenv

from setuptools import find_packages, setup

with open("README.rst") as readme_file:
    readme = readme_file.read()

with open("HISTORY.rst") as history_file:
    history = history_file.read()

requirements = [
    "Click==8.0.4",  # Maximum version for Python 3.6 support
    "PyGithub==1.55",
    "colorama==0.4.5",
    "coloredlogs==15.0.1",  # NOTE(PG): Should be removed during cleanup for loguru instead
    "emoji==1.7.0",
    "f90nml==1.4.2",
    "gfw-creator==0.2.2",
    "gitpython==3.1.20",  # Maximum version for Python 3.6 support
    "loguru==0.6.0",
    "numpy==1.22.0",  # Maximum version for Python 3.6 support
    "packaging==21.3",
    "pandas==1.1.5",  # Correct compatiability with xarray for Python 3.6
    "psutil==5.9.1",
    "pyyaml==5.1",
    "questionary==1.10.0",
    "semver==2.13.0",
    "sqlalchemy==1.4.39",
    "tabulate==0.8.10",
    "tqdm==4.64.0",
    "typing_extensions==4.1.1",  # Maximum number for Python 3.6 support
    "xdgenvpy==2.3.5",
]

setup_requirements = []

test_requirements = [
    "pyfakefs==4.6.0",
]

setup(
    author="The ESM Tools Team",
    author_email=[
        "dirk.barbi@awi.de",
        "paul.gierz@awi.de",
        "miguel.andres-martinez@awi.de",
        "deniz.ural@awi.de",
        "jan.streffing@awi.de",
        "sebastian.wahl@geomar.de",
	      "kai.himstedt@dkrz.de",
    ],
    python_requires=">=3.6, <3.10",
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Science/Research",
        "License :: OSI Approved :: GNU General Public License v2 (GPLv2)",
        "Natural Language :: English",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
    ],
    description="ESM Tools external infrastructure for Earth System Modelling",
    entry_points={
        "console_scripts": [
            "esm_archive=esm_archiving.cli:main",
            "esm_cleanup=esm_cleanup.cli:main",
            "esm_database=esm_database.cli:main",
            "esm_master=esm_master.cli:main",
            "esm_plugins=esm_plugin_manager.cli:main",
            "esm_runscripts=esm_runscripts.cli:main",
            "esm_tests=esm_tests.cli:main",
            "esm_tools=esm_tools.cli:main",
            "esm_utilities=esm_utilities.cli:main",
        ],
    },
    install_requires=requirements,
    license="GNU General Public License v2",
    long_description=readme + "\n\n" + history,
    include_package_data=True,
    keywords="esm_tools",
    name="esm-tools",
    packages=find_packages("src")
    + ["esm_tools", "esm_tools.configs", "esm_tools.namelists", "esm_tools.runscripts"],
    package_dir={
        "": "src",
        "esm_tools.configs": "configs",
        "esm_tools.namelists": "namelists",
        "esm_tools.runscripts": "runscripts",
    },
    package_data={
        "esm_tools.configs": ["../configs/*"],
        "esm_tools.namelists": ["../namelists/*"],
        "esm_tools.runscripts": ["../runscripts/*"],
    },
    setup_requires=setup_requirements,
    test_suite="tests",
    tests_require=test_requirements,
    url="https://github.com/esm-tools/esm_tools",
    version="6.23.6",
    zip_safe=False,
)
