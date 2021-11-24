import collections
import os


def find_log_file(path, in_year=None, debug=False):
    if debug:
        print(f"path={path}")
        print(f"in_year={in_year}")
    expid = path.split("/")[-1]
    script_dir = path + "/scripts"
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
        if debug:
            print(f"{year}: {log}")
    if in_year is None:
        if debug:
            print("No year provided, giving last item back")
        year, logfile = year_and_log.popitem()
        return year, logfile
    else:
        if in_year == "all":
            if debug:
                print(">all< year provided, return two lists")
            return (list(year_and_log.keys()), list(year_and_log.values()))
        elif "," in in_year:
            if debug:
                print("Comma seperated years given")
            return (
                year.split(","),
                [year_and_log.get(year) for year in year.split(",")],
            )
        else:
            if debug:
                print("Single year given")
            return (year, year_and_log.get(year))
