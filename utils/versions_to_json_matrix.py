#!/usr/bin/env python3
"""
Summary
-------
This script prepares a list of versions to be included in github actions as a
matrix. Result is printed to stdout.

Usage
-----
The script can be called with a space seperated list of versions::

    $ ./versions_to_json_matrix.py echam-6.3.05p2 echam-6.3.04 echam-6.3.05p2-concurrent-radiation
    {\"include\":[{\"version\":\"echam-6.3.05p2\"},{\"version\":\"echam-6.3.04\"},{\"version\":\"echam-6.3.05p2-concurrent-radiation\"}]}


Author and Affiliation
----------------------
Dr. Paul Gierz
Scientific Computing
AWI Bremerhaven
November 2021
"""

import argparse
import json


def parse_args():
    """
    Gets model versions from the command line

    Returns
    -------
    list of str :
        List of model version strings
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("versions", type=str, nargs="+")
    return parser.parse_args().versions


def prepare_dict_for_json(versions):
    """
    Prepares list of strings for JSON dumping
    """
    return {"include": [{"version": version} for version in versions]}


def format_json_for_github(version_dict):
    """
    Formats the JSON string for usage within github workflow steps
    """
    version_json = json.dumps(version_dict)
    github_actions_str = version_json.replace('"', '\\"').replace(" ", "")
    return github_actions_str


def main():
    """
    Wraps up all functionality
    """
    versions = parse_args()
    prep_json = prepare_dict_for_json(versions)
    github_actions_str = format_json_for_github(prep_json)
    print(github_actions_str)


if __name__ == "__main__":
    main()
