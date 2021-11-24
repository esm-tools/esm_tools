#!/usr/bin/env python3
"""
Detects the windspeed bug.

Summary
=======
This is a small program that checks a given run for the notorious "wind speed
bug" in the ECHAM family of models. It is "well-behaved", and gives exit codes
0 and 1 for "no wind speed bug found" vs "wind speed bug found".

Usage
=====
The program requires a single argument, the path to the experiment base. By
default, it checks the last year in the scripts directory. Can you can also
check another year by -y YYYY, or, all years, with -y all. You can also run the
check silently with -q. Counting the "WARNING! high wind speed detected" also as
an error can be done with the -e switch.

An example usage::

    # The default usage:
    $ ./check_for_windspeed.py <my_experiment>
    Checking experiment T21G21O21_001...
    Year: 2068
    Warning detected 12 times...
    Run has crashed due to lookup table overflow.
    Consider adjusting "distrub_years" in exp_base/scripts/disturb_years.dat

    $ echo $?
    1

    Checking a specific year:
    $ ./check_for_windspeed.py -y 2067 <my_experiment>
    Checking experiment T21G21O21_01...
    Year: 2067
    No warnings detected.
    No lookup table overflow detected.

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


def parse_args():
    parser = argparse.ArgumentParser(
        description="Dectects the windspeed bug for a given experiment"
    )
    parser.add_argument("path")
    parser.add_argument(
        "-y", "--year", help="Which year to look at (single, comma seperated, or all)"
    )
    parser.add_argument("-d", "--debug", help="debug mode", action="store_true")
    parser.add_argument(
        "-e",
        "--error-on-wind-warn",
        help="Treat high windspeed warnings as errors",
        action="store_true",
    )
    parser.add_argument("-q", "--quiet", help="quiet mode", action="store_true")
    parser.add_argument("--doc", help="Show documentation", action="store_true")
    args = parser.parse_args()
    return args


def find_log_file(args):
    expid = args.path.split("/")[-1]
    script_dir = args.path + "/scripts"
    all_script_files = os.listdir(script_dir)
    all_compute_logs = sorted(
        [
            f
            for f in all_script_files
            if f.startswith(f"{expid}_compute_") and f.endswith(".log") and "-" in f
        ]
    )
    year_and_log = collections.OrderedDict()
    for log in all_compute_logs:
        year = log.replace(f"{expid}_compute_", "")[:4]
        year_and_log[year] = log
    for year, log in year_and_log.items():
        if args.debug:
            print(f"{year}: {log}")
    if args.year is None:
        year, logfile = year_and_log.popitem()
        return year, logfile
    else:
        if args.year == "all":
            return (list(year_and_log.keys()), list(year_and_log.values()))
        elif "," in args.year:
            return (
                args.year.split(","),
                [year_and_log.get(year) for year in args.year.split(",")],
            )
        else:
            return (args.year, year_and_log.get(args.year))


def look_for_warnings(args, log):
    warnings = 0
    decode_errors = 0
    with open(f"{args.path}/scripts/{log}", "rb") as log:
        for line in log.readlines():
            try:
                line = line.decode()
                if "WARNING! high wind speed:" in line:
                    warnings += 1
            except UnicodeDecodeError:
                decode_errors += 1
                continue
    if not args.quiet:
        if warnings > 0:
            print(f"Warning detected {warnings} times...")
        else:
            print(f"No warnings detected.")
        if decode_errors > 0:
            print(f"Problem decoding {decode_errors} lines")
    return warnings


def look_for_lookup_overflow(args, log):
    lookup_overflow = 0
    decode_errors = 0
    with open(f"{args.path}/scripts/{log}", "rb") as log:
        for line in log.readlines():
            try:
                line = line.decode()
                if "lookup table overflow" in line:
                    lookup_overflow += 1
            except UnicodeDecodeError:
                decode_errors += 1
                continue
    if not args.quiet:
        if lookup_overflow > 0:
            print(f"Run has crashed due to lookup table overflow.")
            print(
                f"Consider adjusting adding the year in {args.path}/scripts/disturb_years.dat"
            )
        else:
            print("No lookup overflow detected.")
        if decode_errors > 0:
            print(f"Problem decoding {decode_errors} lines")
    return lookup_overflow


def main():
    if "--doc" in sys.argv:
        print(__doc__)
        return 0
    args = parse_args()
    expid = args.path.split("/")[-1]
    if not args.quiet:
        print(f"Checking experiment {expid}...")
    year, log_file = find_log_file(args)
    if args.debug:
        print(log_file)
    if isinstance(log_file, str):
        if not args.quiet:
            print(f"Year: {year}")
        warnings = look_for_warnings(args, log_file)
        lookup_overflow = look_for_lookup_overflow(args, log_file)
    elif isinstance(log_file, list):
        warnings = 0
        lookup_overflow = 0
        for thisyear, thislog in zip(year, log_file):
            if not args.quiet:
                print(f"Year: {thisyear}")
            warnings += look_for_warnings(args, thislog)
            lookup_overflow += look_for_lookup_overflow(args, thislog)
    if (warnings + lookup_overflow) == 0:
        return 0
    if args.error_on_wind_warn:
        if (warnings + lookup_overflow) > 0:
            return 1
    else:
        if lookup_overflow == 0 and warnings > 0:
            return 0
    if lookup_overflow > 0:
        return 1
    raise AssertionError("This should not happen")


if __name__ == "__main__":
    sys.exit(main())
