"""
Unit tests for esm_calendar
"""

import pytest

import esm_calendar

def test_date_is_in_interval():
    date1 = esm_calendar.Date("1949-12-31")
    date2 = esm_calendar.Date("1950-01-01")
    interval = ("1850-01-01", "1950-01-01")

    assert date1.is_in_interval(interval)
    assert date2.is_in_interval(interval) == False
