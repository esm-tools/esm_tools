#!/usr/bin/env python
"""
A small wrapper that combines the shell interface and the Python interface
"""

# Import from Python Standard Library
import argparse
import logging
import os
import sys

# Import from 3rd Party packages
import coloredlogs

from .esm_database import *


def parse_shargs():
    """The arg parser for interactive use"""
    parser = argparse.ArgumentParser()
    parser.add_argument("table", nargs="?", default=None)

    parser.add_argument("-f", "--find", help="Find keyword", default=None)

    return parser.parse_args()


def main():
    ARGS = parse_shargs()

    find = None

    parsed_args = vars(ARGS)

    if "find" in parsed_args:
        find = parsed_args["find"]

    db = DisplayDatabase(ARGS.table)
