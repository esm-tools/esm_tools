#!/usr/bin/env python3

import f90nml
import pathlib
import os


def get_all_namelists():
    echam_namelists = []
    for dirpath, dirnames, filenames in os.walk("."):
        for file_ in filenames:
            if "namelist.echam" == file_:
                echam_namelists.append(pathlib.Path(f"{dirpath}/{file_}"))
    return echam_namelists


def main():
    all_namelists = get_all_namelists()
    for nmlpath in all_namelists:
        nml = f90nml.read(nmlpath)
        nml['runctl']['nproma'] = 8
        with open(nmlpath, "w") as f:
            nml.write(f)

if __name__ == "__main__":
    main()
