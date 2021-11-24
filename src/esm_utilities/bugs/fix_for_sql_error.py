#!/usr/bin/env python3
"""
docs....
"""
import os
import sys
import shutil
import subprocess

from .check_for_sql_error import parse_args
from .check_for_sql_error import main as check_for_sql_error


class DateFileError(Exception):
    """Raise when you have more than one date file"""


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
    ret_code = check_for_sql_error()
    if ret_code == 0:
        return ret_code

    args = parse_args()
    print("\n")
    print("Applying fix...")
    expid = args.path.split("/")[-1]
    script_dir = args.path + "/scripts"
    year, _ = read_date_file(script_dir)
    yaml_files = [f for f in os.listdir(script_dir) if f.endswith(".yaml")]
    if len(yaml_files) > 1:
        print("Multiple yaml files detected in script_dir!")
        print("Don't know how to proceed")
        return 1
    run_config = yaml_files[0]
    print(f"Using run config {run_config}")
    print(f"As SQL errors may be due to I/O problems, I will just resubmit")
    print()
    print("Removing run folder and old monitoring file for a clean run")
    monitoring_file = script_dir + f"/monitoring_file_{year}0101-{year}1231.out"
    if os.path.exists(monitoring_file):
        os.remove(monitoring_file)
    run_dir = args.path + f"/run_{year}0101-{year}1231"
    shutil.rmtree(run_dir, ignore_errors=True)
    print()
    print("Resubmitting job")
    subprocess.run(
        f"esm_runscripts {run_config} -e {expid}",
        shell=True,
        check=True,
        cwd=f"{script_dir}",
    )


if __name__ == "__main__":
    sys.exit(main())
