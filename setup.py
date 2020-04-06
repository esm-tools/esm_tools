#!/usr/bin/env python

"""The setup script."""

from setuptools import setup, find_packages

with open('README.rst') as readme_file:
    readme = readme_file.read()

with open('HISTORY.rst') as history_file:
    history = history_file.read()

requirements = ["esm_parser @ git+https://github.com/esm-tools/esm_parser.git",
                "esm_environment @ git+https://github.com/esm-tools/esm_environment.git",
                "esm_rcfile @ git+https://github.com/esm-tools/esm_rcfile.git"]

setup_requirements = [ ]

test_requirements = [ ]

setup(
    author="Dirk Barbi",
    author_email='dirk.barbi@awi.de',
    python_requires='>=3.5',
    classifiers=[
        'Development Status :: 4 - Beta',
        'Intended Audience :: Science/Research',
        'License :: OSI Approved :: GNU General Public License v2 (GPLv2)',
        'Natural Language :: English',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
    ],
    description="ESM Master tool for downloading, configuring and compiling of earth system model components and coupled setups",
    entry_points={
        'console_scripts': [
            'esm_master=esm_master.cli:main',
        ],
    },
    install_requires=requirements,
    license="GNU General Public License v2",
    long_description=readme + '\n\n' + history,
    include_package_data=True,
    keywords='esm_master',
    name='esm_master',
    packages=find_packages(include=['esm_master', 'esm_master.*']),
    setup_requires=setup_requirements,
    test_suite='tests',
    tests_require=test_requirements,
    url='https://github.com/dbarbi/esm_master',
    version='3.1.4',
    zip_safe=False,
)
