#!/usr/bin/env python3
"""
Detects the SLURM walltime errors.

Summary
=======
This is a small program that checks a given run for SLURM walltime timeouts.
It is "well-behaved", and gives exit codes 0 and 1.

Usage
=====
The program requires a single argument, the path to the experiment base. By
default, it checks the last year in the scripts directory. Can you can also
check another year by -y YYYY, or, all years, with -y all. You can also run the
check silently with -q.

An example usage::

    # The default usage:
    $ ./check_for_walltime.py <my_experiment>
    Checking experiment T21G21O21_001...
    Year: 2068
    Run has crashed due to SLURM walltime restrictions.
    Consider adjusting `general.compute_time` in {path}/scripts/ and resubmitting.

    $ echo $?
    1

    Checking a specific year:
    $ ./check_for_walltime.py -y 2067 <my_experiment>
    Checking experiment T21G21O21_01...
    Year: 2067
    No SLURM walltime timeout detected.

    $ echo $?
    0


Author and Affiliation
======================
Paul Gierz (pgierz@awi.de)
AWI Bremerhaven
03.2021

Review and Test
===============
???

Funding
=======
PalMod II
Federal Ministry of Education and Research, Germany
"""

import argparse
import collections
import os
import sys

from .base_functions import find_log_file


def parse_args():
    parser = argparse.ArgumentParser(
        description="Dectects if a run encountered a walltime timeout."
    )
    parser.add_argument("path")
    parser.add_argument(
        "-y", "--year", help="Which year to look at (single, comma seperated, or all)"
    )
    parser.add_argument("-d", "--debug", help="debug mode", action="store_true")
    parser.add_argument("-q", "--quiet", help="quiet mode", action="store_true")
    parser.add_argument("--doc", help="Show documentation", action="store_true")
    args = parser.parse_args()
    return args


def look_for_walltime(args, log):
    walltimes = 0
    decode_errors = 0
    with open(f"{args.path}/scripts/{log}", "rb") as log:
        for line in log.readlines():
            try:
                line = line.decode()
                if "DUE TO TIME LIMIT" in line:
                    walltimes += 1
            except UnicodeDecodeError:
                decode_errors += 1
                continue
    if not args.quiet:
        if walltimes > 0:
            print(f"Run has crashed due to SLURM walltime restrictions.")
            print(
                f"Consider adjusting `general.compute_time` in {args.path}/scripts/ and resubmitting"
            )
        else:
            print("No SLURM walltime timeout detected.")
        if decode_errors > 0:
            print(f"Problem decoding {decode_errors} lines")
    return walltimes


def main():
    if "--doc" in sys.argv:
        print(__doc__)
        return 0
    args = parse_args()
    if args.debug:
        print(args)
    expid = args.path.split("/")[-1]
    if not args.quiet:
        print(f"Checking experiment {expid}...")
    year, log_file = find_log_file(args.path, args.year, args.debug)
    if args.debug:
        print(log_file)
    if isinstance(log_file, str):
        if not args.quiet:
            print(f"Year: {year}")
        walltimes = look_for_walltime(args, log_file)
    elif isinstance(log_file, list):
        walltimes = 0
        for thisyear, thislog in zip(year, log_file):
            if not args.quiet:
                print(f"Year: {thisyear}")
            walltimes += look_for_walltime(args, thislog)
    if walltimes > 0:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
