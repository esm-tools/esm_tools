"""
Module for handling date and calendar functionalities.

This module provides classes and functions to handle various date and calendar operations,
such as date manipulation, calendar handling with different types of calendars, and date formatting.

Classes:
    DateFormat: Class for handling date formatting options.
    Calendar: Class to contain various types of calendars and their functionalities.
    Date: Class to handle date operations and manipulations.

Functions:
    find_remaining_minutes(seconds): Finds the remaining full minutes of a given number of seconds.
    date_range(start_date, stop_date, frequency): Yields dates from start_date to stop_date with a given frequency.
"""

import copy
import sys
from typing import Generator, Union

from deprecated import deprecated


def find_remaining_minutes(seconds: int) -> int:
    """
    Finds the remaining full minutes of a given number of seconds.

    Parameters
    ----------
    seconds : int
        The number of seconds to allocate.

    Returns
    -------
    int
        The leftover seconds once new minutes have been filled.

    Examples
    --------
    >>> find_remaining_minutes(125)
    5
    """
    if not isinstance(seconds, int):
        raise TypeError(
            f"You must provide an integer, instead got: {seconds} of type {type(seconds)}"
        )
    if seconds < 0:
        raise ValueError(f"You must provide a positive integer, instead got {seconds}")
    return seconds % 60


# Alias find_remaining_hours to find_remaining_minutes
find_remaining_hours = find_remaining_minutes


# NOTE(PG): A grep -i "date_range" doesn't seem to use this anywhere. For now, I'm deprecating it, and it
#          should be removed soon.
@deprecated(reason="This function is not used anywhere, and will be removed!")
def date_range(
    start_date: Union[str, "Date"], stop_date: Union[str, "Date"], frequency: str
) -> Generator["Date", None, None]:
    """
    Yields dates from start_date to stop_date with a given frequency.

    Parameters
    ----------
    start_date : str or Date
        The start date.
    stop_date : str or Date
        The stop date.
    frequency : str or Date
        The frequency of the date range.

    Yields
    ------
    Date
        The next date in the range.
    """
    if isinstance(start_date, str):
        start_date = Date(start_date)
    if isinstance(stop_date, str):
        stop_date = Date(stop_date)
    if isinstance(frequency, str):
        frequency = Date(frequency)
    current_date = start_date
    while current_date <= stop_date:
        yield current_date
        current_date += frequency


class Calendar:
    """
    Class to contain various types of calendars.

    Parameters
    ----------
    calendar_type : str
        The type of calendar to use. Options are:
        - "no_leap" : No leap years, or integer 0
        - "gregorian" : Proleptic Gregorian calendar (default), or integer 1.
        - "equal_months" : Equal months of `n` days (provide an integer instead of `n`).

    Attributes
    ----------
    time_units : list of str
        A list of accepted time units.
    month_names : list of str
        A list of valid month names, using 3 letter English abbreviation.

    Methods
    -------
    is_leap_year(year)
        Returns a boolean testing if the given year is a leap year.
    days_in_year(year)
        Returns the total number of days in a given year.
    days_in_month(year, month)
        Returns the total number of days in a given month for a given year (considering leap years).

    Examples
    --------
    >>> cal = Calendar("gregorian")
    >>> cal.is_leap_year(2024)
    True
    >>> cal.days_in_year(2024)
    366
    >>> cal.days_in_month(2024, 2)
    29
    """

    TIME_UNITS = ["years", "months", "days", "hours", "minutes", "seconds"]
    MONTH_NAMES = [
        "Jan",
        "Feb",
        "Mar",
        "Apr",
        "May",
        "Jun",
        "Jul",
        "Aug",
        "Sep",
        "Oct",
        "Nov",
        "Dec",
    ]

    # NOTE(PG): This is for backwards compatibility with old code, and will be removed!
    @staticmethod
    @deprecated(
        reason="Using 'calendar_type = 1' is deprecated, use 'calendar_type = \"gregorian\"' instead."
    )
    def _set_gregorian():
        return "gregorian"

    @staticmethod
    @deprecated(
        reason="Using 'calendar_type = 0' is deprecated, use 'calendar_type = \"no_leap\"' instead."
    )
    def _set_no_leap():
        return "no_leap"

    def __init__(self, calendar_type="gregorian"):
        # NOTE(PG): This is for backwards compatibility with old code, this
        #           should throw a deprecation warning when triggered!
        if calendar_type == 1:
            calendar_type = self._set_gregorian()
        if calendar_type == 0:
            calendar_type = self._set_no_leap()
        self.calendar_type = calendar_type

    def is_leap_year(self, year):
        """
         Checks if a year is a leap year.

         Parameters
         ----------
         year : int
             The year to check.

         Returns
         -------
         bool
             True if the given year is a leap year.

         Examples
         --------
         >>> cal = Calendar("gregorian")
         >>> cal.is_leap_year(2024)
         True
        >>> cal.is_leap_year(1900)
        False
        >>> cal.is_leap_year(2000)
        True
        >>> cal.is_leap_year(2023)
        False
        """
        return (
            self.calendar_type == "gregorian"
            and (year % 4 == 0)
            and (year % 100 != 0 or year % 400 == 0)
        )

    def days_in_year(self, year):
        """
        Finds total number of days in a year, considering leap years if the calendar type allows for them.

        Parameters
        ----------
        year : int
            The year to check.

        Returns
        -------
        int
            The total number of days for this specific calendar type.

        Examples
        --------
        >>> cal = Calendar("gregorian")
        >>> cal.days_in_year(2024)
        366
        """
        if self.calendar_type == "no_leap":
            return 365
        elif self.calendar_type == "gregorian":
            return 365 + int(self.is_leap_year(year))
        return int(self.calendar_type) * 12

    def days_in_month(self, year, month):
        """
        Finds the number of days in a given month.

        Parameters
        ----------
        year : int
            The year to check.
        month : int or str
            The month number or short name.

        Returns
        -------
        int
            The number of days in this month, considering leap years if needed.

        Raises
        ------
        ValueError
            Raised when you give an incorrect month.

        Examples
        --------
        >>> cal = Calendar("gregorian")
        >>> cal.days_in_month(2024, 2)
        29
        """
        if isinstance(month, str):
            month = self.MONTH_NAMES.index(month.capitalize()) + 1
        if not 1 <= month <= 12:
            raise ValueError("Invalid month. Must be between 1 and 12.")

        if self.calendar_type in ["no_leap", "gregorian"]:
            if month in [1, 3, 5, 7, 8, 10, 12]:
                return 31
            if month in [4, 6, 9, 11]:
                return 30
            return 28 + int(self.is_leap_year(year))
        return int(self.calendar_type)

    def __repr__(self):
        return f"Calendar(calendar_type={self.calendar_type})"

    def __str__(self):
        if self.calendar_type == "no_leap":
            return "Calendar object with no leap years allowed"
        if self.calendar_type == "gregorian":
            return "Calendar object with allowed leap years"
        return f"Calendar object with equal-length months of {self.calendar_type} days"


class DateFormat:
    """A class to represent and format Date objects as strings"""

    _DATE_SEPARATORS = ["", "-", "-", "-", " ", " ", "", "-", "", "", "/"]
    _TIME_SEPARATORS = ["", ":", ":", ":", " ", ":", ":", "", "", "", ":"]
    _DATETIME_SEPARATORS = ["_", "_", "T", " ", " ", " ", "_", "_", "", "_", " "]

    def __init__(
        self, form=1, print_hours=True, print_minutes=True, print_seconds=True
    ):
        self.form = form
        self.print_hours = print_hours
        self.print_minutes = print_minutes
        self.print_seconds = print_seconds

    def __call__(self, date: "Date") -> str: ...

    def __repr__(self):
        return (
            f"DateFormat(form={self.form}, print_hours={self.print_hours}, "
            f"print_minutes={self.print_minutes}, print_seconds={self.print_seconds})"
        )


class Date:
    """
    A class to contain dates, also compatible with paleo (negative dates)

    Parameters
    ----------
    indate : str
        The date to use.

        See ``esm_calendar.DateFormat`` for available
        formatters.

    calendar : ~`pyesm.core.time_control.esm_calendar.Calendar`, optional
        The type of calendar to use. Defaults to a greogrian proleptic calendar
        if nothing is specified.

    Attributes
    ----------
    year : int
        The year
    month : int
        The month
    day : int
        The day
    hour : int
        The hour
    minute : int
        The minute
    second : int
        The second
    _calendar : ~`pyesm.core.time_control.esm_calendar.Calendar`
        The type of calendar to use

    Methods
    -------
    """

    def __init__(self, indate, calendar=Calendar()):
        if isinstance(indate, str):
            self._init_from_str(indate, calendar=Calendar())
        elif isinstance(indate, Date):
            self._init_from_date(indate, calendar=Calendar())
        else:
            raise TypeError(
                f"{type(indate)} is not a valid type to initialize a Date object "
                "(valid types: str, Date)"
            )

    def _init_from_str(self, indate, calendar=Calendar()):
        printhours = True
        printminutes = True
        printseconds = True
        ndate = ["1900", "01", "01", "00", "00", "00"]
        ds = ""
        ts = ""
        if "T" in indate:
            indate2 = indate.replace("T", "_")
            ts = ":"
        else:
            indate2 = indate
        if "_" in indate2:
            date, time = indate2.split("_")
        elif ":" in indate2 and not "-" in indate2:
            date = "0000-00-00"
            time = indate2
        else:
            date = indate2
            time = ""
            ts = ":"
        for index in [3, 4, 5]:
            if len(time) == 2:
                ndate[index] = time
                time = time[2:]
            elif len(time) > 2:
                ndate[index] = time[:2]
                if len(time) > 2:
                    time = time[2:]
                    if time[0] == ":":
                        time = time[1:]
                        ts = ":"
            else:
                ndate[index] = "00"
                if index == 3:
                    printhours = False
                elif index == 4:
                    printminutes = False
                elif index == 5:
                    printseconds = False
        for index in [2, 1]:
            ndate[index] = date[-2:]
            date = date[:-2]
            if len(date) > 0 and date[-1] == "-":
                date = date[:-1]
                ds = "-"
        ndate[0] = date
        if ds == "-" and ts == ":":
            if "T" not in indate:
                form = 1
            else:
                form = 2
        elif ds == "-" and ts == "":
            form = 7
        elif ds == "" and ts == ":":
            form = 6
        elif ds == "" and ts == "":
            form = 9
        self.year, self.month, self.day, self.hour, self.minute, self.second = map(
            int, ndate
        )
        (
            self.syear,
            self.smonth,
            self.sday,
            self.shour,
            self.sminute,
            self.ssecond,
        ) = map(str, ndate)

        self._calendar = calendar
        self.doy = self.day_of_year()
        self.sdoy = str(self.day_of_year())

        self._date_format = DateFormat(form, printhours, printminutes, printseconds)

    def _init_from_date(self, indate, calendar=Calendar()):
        self.year, self.month, self.day, self.hour, self.minute, self.second = (
            indate.year,
            indate.month,
            indate.day,
            indate.hour,
            indate.minute,
            indate.second,
        )

    @property
    def sdoy(self):
        return self.__sdoy

    @sdoy.setter
    def sdoy(self, sdoy):
        self.__sdoy = str(self.doy)

    @property
    def syear(self):
        return self.__syear

    @syear.setter
    def syear(self, syear):
        self.__syear = str(self.year)

    @property
    def smonth(self):
        return self.__smonth

    @smonth.setter
    def smonth(self, smonth):
        self.__smonth = str(self.month).zfill(2)

    @property
    def sday(self):
        return self.__sday

    @sday.setter
    def sday(self, sday):
        self.__sday = str(self.day).zfill(2)

    @property
    def shour(self):
        return self.__shour

    @shour.setter
    def shour(self, shour):
        self.__shour = str(self.hour).zfill(2)

    @property
    def sminute(self):
        return self.__sminute

    @sminute.setter
    def sminute(self, sminute):
        self.__sminute = str(self.minute).zfill(2)

    @property
    def ssecond(self):
        return self.__ssecond

    @ssecond.setter
    def ssecond(self, ssecond):
        self.__ssecond = str(self.second).zfill(2)

    def output(self, form="SELF"):
        return self.format(form)

    @classmethod
    def from_list(cls, _list):
        """
        Creates a new Date from a list

        Parameters
        ----------
        _list : list of ints
            A list of [year, month, day, hour, minute, second]

        Returns
        -------
        date : ~`pyesm.core.time_control.esm_calendar.Date`
            A new date of year month day, hour minute, second
        """

        negative = ["-" if _list[i] < 0 else "" for i in range(6)]
        _list = [str(abs(element)) for element in _list]

        if len(_list[0]) < 4:
            _list[0] = _list[0].zfill(4)
        _list[1:6] = [
            element.zfill(2) if len(element) < 2 else element for element in _list[1:6]
        ]

        _list = [negative[i] + _list[i] for i in range(6)]

        indate = (
            _list[0]
            + "-"
            + _list[1]
            + "-"
            + _list[2]
            + "_"
            + _list[3]
            + ":"
            + _list[4]
            + ":"
            + _list[5]
        )

        return cls(indate)

    fromlist = from_list

    def __repr__(self):
        return f"Date({self.year:02}-{self.month:02}-{self.day:02}T{self.hour:02}:{self.minute:02}:{self.second:02})"

    def __getitem__(self, item):
        return (self.year, self.month, self.day, self.hour, self.minute, self.second)[
            item
        ]

    def __setitem__(self, item, value):
        value = int(value)
        if item == 0:
            self.year = value
        elif item == 1:
            self.month = value
        elif item == 2:
            self.day = value
        elif item == 3:
            self.hour = value
        elif item == 4:
            self.minute = value
        elif item == 5:
            self.second = value
        else:
            raise IndexError("You can only assign up to 5!")

    def __lt__(self, other):
        self_tup = (
            self.year,
            self.month,
            self.day,
            self.hour,
            self.minute,
            self.second,
        )
        other_tup = (
            other.year,
            other.month,
            other.day,
            other.hour,
            other.minute,
            other.second,
        )
        return self_tup < other_tup

    def __le__(self, other):
        self_tup = (
            self.year,
            self.month,
            self.day,
            self.hour,
            self.minute,
            self.second,
        )
        other_tup = (
            other.year,
            other.month,
            other.day,
            other.hour,
            other.minute,
            other.second,
        )
        return self_tup <= other_tup

    def __eq__(self, other):
        if not isinstance(other, Date):
            return False

        self_tup = (
            self.year,
            self.month,
            self.day,
            self.hour,
            self.minute,
            self.second,
        )
        other_tup = (
            other.year,
            other.month,
            other.day,
            other.hour,
            other.minute,
            other.second,
        )
        return self_tup == other_tup

    def __ne__(self, other):
        if not isinstance(other, Date):
            return True

        self_tup = (
            self.year,
            self.month,
            self.day,
            self.hour,
            self.minute,
            self.second,
        )
        other_tup = (
            other.year,
            other.month,
            other.day,
            other.hour,
            other.minute,
            other.second,
        )
        return self_tup != other_tup

    def __ge__(self, other):
        self_tup = (
            self.year,
            self.month,
            self.day,
            self.hour,
            self.minute,
            self.second,
        )
        other_tup = (
            other.year,
            other.month,
            other.day,
            other.hour,
            other.minute,
            other.second,
        )
        return self_tup >= other_tup

    def __gt__(self, other):
        self_tup = (
            self.year,
            self.month,
            self.day,
            self.hour,
            self.minute,
            self.second,
        )
        other_tup = (
            other.year,
            other.month,
            other.day,
            other.hour,
            other.minute,
            other.second,
        )
        return self_tup > other_tup

    def __sub__(self, other):
        if isinstance(other, Date):
            return self.sub_date(other)
        elif isinstance(other, tuple):
            return self.sub_tuple(other)
        else:
            print("No known combination for subtraction")
            sys.exit(1)

    def sub_date(self, other):
        d2 = copy.deepcopy(
            [self.year, self.month, self.day, self.hour, self.minute, self.second]
        )
        d1 = copy.deepcopy(
            [other.year, other.month, other.day, other.hour, other.minute, other.second]
        )

        diff = [0, 0, 0, 0, 0, 0]

        while d1[1] > 1:
            diff[1] -= 1
            d1[1] -= 1
            diff[2] -= self._calendar.day_in_month(d1[0], d1[1])

        while d2[1] > 1:
            diff[1] += 1
            d2[1] -= 1
            diff[2] += self._calendar.day_in_month(d2[0], d2[1])

        while d1[2] > 1:
            diff[2] -= 1
            d1[2] -= 1

        while d2[2] > 1:
            diff[2] += 1
            d2[2] -= 1

        if diff[1] < 0:
            diff[0] = diff[0] - 1

        while d2[0] > d1[0]:
            diff[0] += 1
            diff[1] += 12
            diff[2] += self._calendar.day_in_year(d1[0])
            d1[0] += 1

        diff[3] += diff[2] * 24
        if diff[3] < 0:
            diff[3] += 24
        diff[4] += diff[3] * 60
        if diff[4] < 0:
            diff[4] += 60
        diff[5] += diff[4] * 60
        if diff[5] < 0:
            diff[5] += 60

        return diff

    def time_between(self, date, outformat="seconds"):
        """
        Computes the time between two dates

        Parameters
        ----------
        date : ~`pyesm.core.time_control.date`
            The date to compare against.

        Returns
        -------
        ??
        """
        if date > self:
            diff = date - self
        else:
            diff = self - date

        for index in range(0, 6):
            if outformat == self._calendar.timeunits[index]:
                # FIXME: Wouldn't this stop after the very first index that matches?
                # I think that is the point, but I'm not sure.
                return diff[index]
        return None  # ...? Or raise an error?

    def day_of_year(self):
        """
        Gets the day of the year, counting from Jan. 1

        Returns
        -------
        int
            The day of the current year.
        """
        if self[1] == self[2] == 1:
            return 1
        else:
            date2 = Date(str(self[0]) + "-01-01T00:00:00", self._calendar)
            return self.time_between(date2, "days") + 1

    def __str__(self):
        return (
            "-".join([str(self.year), str(self.month).zfill(2), str(self.day).zfill(2)])
            + "T"
            + ":".join(
                [
                    str(self.hour).zfill(2),
                    str(self.minute).zfill(2),
                    str(self.second).zfill(2),
                ]
            )
        )

    def format(
        self, form="SELF", givenph=None, givenpm=None, givenps=None
    ):  # basically format_date
        """
        Beautifully returns a ``Date`` object as a string.

        Parameters
        ----------
        form : str or int
            Logic taken from from MPI-Met
        givenph : bool-ish
            Print hours
        givenpm : bool-ish
            Print minutes
        givenps : bool-ish
            Print seconds

        Note
        ----
        **How to use the ``form`` argument**
            The following forms are accepted:
            + SELF: uses the format which was given when constructing the date
            + 0: A Date formatted as YYYY

            In [5]: test.format(form=1)
            Out[5]: '1850-01-01_00:00:00'

            In [6]: test.format(form=2)
            Out[6]: '1850-01-01T00:00:00'

            In [7]: test.format(form=3)
            Out[7]: '1850-01-01 00:00:00'

            In [8]: test.format(form=4)
            Out[8]: '1850 01 01 00 00 00'

            In [9]: test.format(form=5)
            Out[9]: '01 Jan 1850 00:00:00'

            In [10]: test.format(form=6)
            Out[10]: '18500101_00:00:00'

            In [11]: test.format(form=7)
            Out[11]: '1850-01-01_000000'

            In [12]: test.format(form=8)
            Out[12]: '18500101000000'

            In [13]: test.format(form=9)
            Out[13]: '18500101_000000'

            In [14]: test.format(form=10)
            Out[14]: '01/01/1850 00:00:00'
        """
        # Programmer notes to not be ever included in the doc-string: Paul really, Really, REALLY
        # dislikes this function. It rubs me in the wrong way.
        if form == "SELF":
            form = self._date_format.form

        ph = self._date_format.printhours
        pm = self._date_format.printminutes
        ps = self._date_format.printseconds

        # These variables are....utterly pointless
        if givenph is not None:
            ph = givenph
        if givenpm is not None:
            pm = givenpm
        if givenps is not None:
            ps = givenps
        ndate = list(
            map(
                str,
                (
                    self.year,
                    self.smonth,
                    self.sday,
                    self.shour,
                    self.sminute,
                    self.ssecond,
                ),
            )
        )
        if form == 0:
            if len(ndate[0]) < 4:
                for _ in range(1, 4 - len(ndate[0])):
                    ndate[0] = "0" + ndate[0]
        elif form == 5:
            temp = ndate[0]
            ndate[0] = ndate[2]
            ndate[2] = temp
            ndate[1] = self._calendar.monthnames[int(ndate[1]) - 1]
        elif form == 8:
            if len(ndate[0]) < 4:
                print("Format 8 clear with 4 digit year only")
                sys.exit(2)
        elif form == 10:
            temp = ndate[0]
            ndate[0] = ndate[1]
            ndate[1] = ndate[2]
            ndate[2] = temp

        for index in range(0, 6):
            if len(ndate[index]) < 2:
                ndate[index] = "0" + ndate[index]

        ndate[1] = self._date_format.datesep[form] + ndate[1]
        ndate[2] = self._date_format.datesep[form] + ndate[2]
        ndate[3] = self._date_format.dtsep[form] + ndate[3]
        ndate[4] = self._date_format.timesep[form] + ndate[4]
        ndate[5] = self._date_format.timesep[form] + ndate[5]

        if not ps:
            ndate[5] = ""
        if not pm and ndate[5] == "":
            ndate[4] = ""
        if not ph and ndate[4] == "":
            ndate[3] = ""

        return ndate[0] + ndate[1] + ndate[2] + ndate[3] + ndate[4] + ndate[5]

    def makesense(self, ndate):
        """
        Puts overflowed time back into the correct unit.

        When manipulating the date, it might be that you have "70 seconds", or
        something similar. Here, we put the overflowed time into the
        appropriate unit.
        """
        # ndate = copy.deepcopy(self)
        ndate[4] = ndate[4] + ndate[5] // 60
        ndate[5] = ndate[5] % 60

        ndate[3] = ndate[3] + ndate[4] // 60
        ndate[4] = ndate[4] % 60

        ndate[2] = ndate[2] + ndate[3] // 24
        ndate[3] = ndate[3] % 24

        ndate[0] = ndate[0] + (ndate[1] - 1) // 12
        ndate[1] = (ndate[1] - 1) % 12 + 1

        while ndate[2] > self._calendar.day_in_month(ndate[0], ndate[1]):
            ndate[2] = ndate[2] - self._calendar.day_in_month(ndate[0], ndate[1])
            ndate[1] = ndate[1] + 1
            ndate[0] = ndate[0] + (ndate[1] - 1) // 12
            ndate[1] = (ndate[1] - 1) % 12 + 1

        ndate[0] = ndate[0] + (ndate[1] - 1) // 12
        ndate[1] = (ndate[1] - 1) % 12 + 1

        while ndate[2] <= 0:
            ndate[1] = ndate[1] - 1
            if ndate[1] == 0:
                ndate[1] = 12
                ndate[0] = ndate[0] - 1
            ndate[2] = ndate[2] + self._calendar.day_in_month(ndate[0], ndate[1])

        if ndate[1] == 0:
            ndate[1] = 12
            ndate[0] = ndate[0] - 1

        return ndate

        # self.year, self.month, self.day, self.hour, self.minute, self.second = map(
        #    int, ndate
        # )
        # self.syear, self.smonth, self.sday, self.shour, self.sminute, self.ssecond = map(
        #    str, ndate
        # )

    def add(self, to_add):
        """
        Adds another date to this one.

        Parameters
        ----------
        to_add : ~`pyesm.core.time_control.Date`
            The other date to add to this one.

        Returns
        -------
        new_date : ~`pyesm.core.time_control.Date`
            A new date object with the added dates
        """

        me = [self.year, self.month, self.day, self.hour, self.minute, self.second]
        result = [me[i] + to_add[i] for i in range(6)]
        result = self.makesense(result)

        new_date = self.from_list(result)
        return new_date

    __add__ = add

    def sub_tuple(self, to_sub):
        """
        Adds another date to from one.

        Parameters
        ----------
        to_sub : ~`pyesm.core.time_control.Date`
            The other date to sub from this one.

        Returns
        -------
        new_date : ~`pyesm.core.time_control.Date`
            A new date object with the subtracted dates
        """

        me = [self.year, self.month, self.day, self.hour, self.minute, self.second]
        result = [me[i] - to_sub[i] for i in range(6)]
        result = self.makesense(result)

        new_date = self.from_list(result)
        return new_date
