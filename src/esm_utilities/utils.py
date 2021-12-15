import pandas as pd
import io
import datetime
import numpy as np


def logfile_stats(logfile_to_read):

    with open(logfile_to_read, "r") as f:
        table = f.read()

    df = pd.read_table(
        io.StringIO(table),
        sep=" : | -",
        engine="python",
        names=["Date", "Stuff", "State"],
    )
    # Remove commented rows
    df = df[~df.Stuff.str.startswith("#")]
    # Clean up Date
    df["Date"] = pd.to_datetime(df.Date, errors="coerce")
    # Clean up Stuff
    ## Split columns
    df[["Jobtype", "Job Number", "Job Date", "Job PID"]] = df.Stuff.str.split(
        " ", expand=True
    )
    del df["Stuff"]
    ## Fix types
    df["Jobtype"] = df.Jobtype.str.strip()
    df["Job Number"] = df["Job Number"].astype(int)
    df["Job Date"] = pd.to_datetime(df["Job Date"])
    df["Job PID"] = df["Job PID"].astype(int)
    # Clean up State
    df["State"] = df.State.str.strip()

    while "done" not in df.iloc[-1]["State"]:
        df = df.head(-1)

    gb = df.groupby(["Job Number", "Jobtype"])

    compute_submit_to_start = []
    compute_start_to_end = []
    tidy_start_to_end = []
    queue_time_list = []
    for (jobnumber, jobtype), subdf in gb:
        if jobnumber > 1:
            prev_subdf = gb.get_group((jobnumber - 1, jobtype))
            prev_compute = gb.get_group((jobnumber - 1, "compute"))
            prev_tidy = gb.get_group((jobnumber - 1, "tidy_and_resubmit"))
        if "compute" in jobtype:
            submit = subdf.Date[subdf.State == "submitted"].values[0].astimezone(None)
            end = subdf.Date[subdf.State == "done"].values[0].astimezone(None)
            start = subdf.Date[subdf.State == "start"].values[0].astimezone(None)
            compute_submit_to_start.append((jobnumber, start - submit))
            compute_start_to_end.append((jobnumber, end - start))
            if jobnumber > 1:
                queue_time = start - prev_compute.Date[
                    prev_compute.State == "done"
                ].values[0].astimezone(None)
            else:
                queue_time = None
            queue_time_list.append((jobnumber, queue_time))
        if "tidy" in jobtype:
            end = subdf.Date[subdf.State == "done"].values[0].astimezone(None)
            start = subdf.Date[subdf.State == "start"].values[0].astimezone(None)
            tidy_start_to_end.append((jobnumber, end - start))

    queue_time_df = (
        pd.DataFrame(queue_time_list, columns=["Job Number", "Queue Time"])
        .set_index("Job Number")
        .transpose()
    )

    compute_start_to_end_df = (
        pd.DataFrame(compute_start_to_end, columns=["Job Number", "Compute"])
        .set_index("Job Number")
        .transpose()
    )
    compute_submit_to_start_df = (
        pd.DataFrame(compute_submit_to_start, columns=["Job Number", "Submit"])
        .set_index("Job Number")
        .transpose()
    )
    tidy_df = (
        pd.DataFrame(tidy_start_to_end, columns=["Job Number", "Tidy"])
        .set_index("Job Number")
        .transpose()
    )
    df = pd.concat(
        [queue_time_df, compute_submit_to_start_df, compute_start_to_end_df, tidy_df]
    )
    df = df.transpose()
    print(df)
    print(df.mean())

    one_day = datetime.timedelta(1)
    throughput = one_day / df["Compute"][1:].mean()
    print(
        f"Theoretical Throughput assuming no queueing time: {np.round(throughput, 2)} runs per day"
    )
    return df
