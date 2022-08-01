#!/usr/bin/env python3
import time
from functools import wraps

TIMING_INFO = []
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


def timing(f):
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

    @wraps(f)
    def wrap(*args):
        time1 = time.time()
        ret = f(*args)
        time2 = time.time()
        TIMING_INFO.append(
            "{:s} function took {:.3f} ms".format(
                f.__qualname__, (time2 - time1) * 1000.0
            )
        )
        return ret

    return wrap
