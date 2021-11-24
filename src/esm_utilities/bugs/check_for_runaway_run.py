#!/usr/bin/env python3
"""
... docs
"""
import argparse
import os
import sys


def parse_args():
    parser = argparse.ArgumentParser(
        description="Dectects runaway run for a given experiment"
    )
    parser.add_argument("path")
    parser.add_argument("-d", "--debug", help="debug mode", action="store_true")
    parser.add_argument("-q", "--quiet", help="quiet mode", action="store_true")
    parser.add_argument("--doc", help="Show documentation", action="store_true")
    args = parser.parse_args()
    return args


def find_last_valid_year(path):
    expid = path.split("/")[-1]
    restart_folder = f"{path}/restart/echam"
    restart_files = sorted([f for f in os.listdir(restart_folder) if "echam" in f])
    # Remove the file without a specific date:
    restart_files.remove(f"restart_{expid}_echam.nc")
    print(f"Last 5 files in {expid} for restart/echam:")
    for restart_file in restart_files[-5:]:
        print(f"* {restart_file}")
    last_year = (
        restart_files[-1]
        .replace(f"restart_{expid}_", "")
        .replace("_echam.nc", "")
        .replace("1231", "")
    )
    return last_year


def read_date_file(folder):
    date_file = [f for f in os.listdir(folder) if f.endswith("date")]
    if len(date_file) > 1:
        raise DateFileError("Multiple date files found, don't know how to proceed")
    date_file = date_file[0]
    with open(folder + "/" + date_file, "r") as date_file:
        date, run_number = date_file.read().strip().split()
    year = date.split("-")[0]
    return year, run_number


def main():
    args = parse_args()
    last_year = find_last_valid_year(args.path)
    year_in_date_file, _ = read_date_file(args.path + "/scripts")
    if int(year_in_date_file) > int(last_year) + 1:
        print(f"Last year (restarts) = {last_year}")
        print(f"Year (datefile)      = {year_in_date_file}")
        print()
        print("This looks like a runaway run!")
        print("Consider using the cleanup_runaway_run.py script!")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
