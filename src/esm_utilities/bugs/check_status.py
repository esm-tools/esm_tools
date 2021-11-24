#!/usr/bin/env python3

import os

experiments = os.listdir(os.getenv("PROJECT_BASE") + "/experiments_production")

for exp in experiments:
    print(exp)
    with open(
        f"{os.getenv('PROJECT_BASE')}/experiments_production/{exp}/scripts/{exp}_awiesm.date",
        "r",
    ) as date:
        date, run_number = date.readlines()[0].strip().split()
    print(date, run_number)
