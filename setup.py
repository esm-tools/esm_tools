#!/usr/bin/env python

"""The setup script."""

from setuptools import setup, find_packages
from os import getenv

with open("README.rst") as readme_file:
    readme = readme_file.read()

with open("HISTORY.rst") as history_file:
    history = history_file.read()

requirements = [
    "Click>=7.0",
    "PyGithub",
    "colorama",
    "coloredlogs",
    "emoji",
    "f90nml",
    "gfw-creator",
    "gitpython",
    "loguru",
    "numpy",
    "packaging",
    "pandas>=1.0",
    "psutil",
    "pyyaml",
    "pyyaml>=5.1",
    "questionary",
    "semver",
    "six",
    "sqlalchemy",
    "tabulate",
    "tqdm",
    "typing_extensions>=3.10.0.0",
    "xdgenvpy",
]

setup_requirements = []

test_requirements = ["pyfakefs"]

setup(
    author="Dirk Barbi",
    author_email="dirk.barbi@awi.de",
    python_requires=">=3.6",
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
    version="6.0.4",
    zip_safe=False,
)
