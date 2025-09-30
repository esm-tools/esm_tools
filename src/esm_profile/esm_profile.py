#!/usr/bin/env python3
import datetime
import time
from functools import wraps

from loguru import logger

TIMING_INFO = {}
"""Global list of timed functions"""
# NOTE(PG): This is a hack to get around the fact that the decorator is not
# able to get the list of functions (Or, I am too stupid to figure out how to
# do it)
#
# This is a bad idea. Don't do this. Globally available mutable objects are bad.


def get_timing_info():
    """
    Get timing information

    Returns
    -------
    list
        list of timing information, as strings
    """
    return TIMING_INFO


def timing(f, task="other"):
    """
    Decorator to time functions

    Parameters
    ----------
    f : callable
        function to be timed

    Returns
    -------
    callable
        function with timing information
    """
    TIMING_INFO[task] = TIMING_INFO.get(task, {})

    @wraps(f)
    def wrap(*args):
        time1 = time.time()
        ret = f(*args)
        time2 = time.time()
        TIMING_INFO[task][f.__qualname__] = time2 - time1
        return ret

    return wrap


def print_profile_summary():
    """
    Prints the profile summary
    """

    steps_threshold = 3
    length_key = 0
    summary = {}
    for task, timings in TIMING_INFO.items():
        summary[task] = {"total_time": 0}
        for key, value in timings.items():
            if len(key) > length_key:
                length_key = len(key)

            summary[task]["total_time"] += value

        summary[task]["sorted"] = dict(
            sorted(timings.items(), key=lambda item: item[1], reverse=True)
        )

    for task, timings in TIMING_INFO.items():
        logger.info(f"\nPROFILING FOR {task.upper()}:")
        for key, value in timings.items():
            padding = ".." + "." * (length_key - len(key))
            logger.info(f"{key}{padding}{str(datetime.timedelta(seconds=value))}")

        c = 0
        for key, value in summary[task]["sorted"].items():
            c += 1
            if c <= steps_threshold:
                percentage = value / summary[task]["total_time"] * 100
                logger.info(
                    "{:s} is taking {:.1f}% | {:s})".format(
                        key, percentage, str(datetime.timedelta(seconds=value))
                    )
                )
            else:
                continue
