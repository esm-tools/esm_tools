"""
Module for handling date and calendar functionalities in the esm-tools framework.

This module provides classes and functions to handle various date operations,
including date manipulation, calendar handling with different types of calendars,
and date formatting.

Classes
-------
DateFormat
    Handles date formatting options.
Calendar
    Contains various types of calendars and their functionalities.
Date
    Handles date operations and manipulations.

Functions
---------
find_remaining_minutes(seconds)
    Finds the leftover seconds after full minutes are subtracted.
date_range(start_date, stop_date, frequency)
    (Deprecated) Yields dates from start_date to stop_date with a given frequency.

Examples
--------
Using ``find_remaining_minutes``:

.. code-block:: python

    >>> find_remaining_minutes(125)
    5

Creating a date and formatting it:

.. code-block:: python

    >>> d = Date("2024-02-16T11:30:00")
    >>> print(d)
    2024-02-16T11:30:00
    >>> d.format(form=2)
    '2024-02-16T11:30:00'
"""

from typing import Generator, List, Optional, Tuple, Union

from deprecated import deprecated


def find_remaining_minutes(seconds: int) -> int:
    """
    Finds the remaining full minutes given a number of seconds.

    Parameters
    ----------
    seconds : int
        The number of seconds to evaluate.

    Returns
    -------
    int
        The leftover seconds after full minutes have been allocated.

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


# Alias remaining hours to the same as remaining minutes for now.
find_remaining_hours = find_remaining_minutes


@deprecated(reason="This function is not used anywhere, and will be removed!")
def date_range(
    start_date: Union[str, "Date"],
    stop_date: Union[str, "Date"],
    frequency: Union[str, "Date"],
) -> Generator["Date", None, None]:
    """
    Yields dates from start_date to stop_date with a given frequency.

    Parameters
    ----------
    start_date : str or Date
        The starting date.
    stop_date : str or Date
        The ending date.
    frequency : str or Date
        A date representing the frequency interval.

    Yields
    ------
    Date
        The next date in the range.

    Examples
    --------
    >>> for d in date_range("2024-01-01T00:00:00", "2024-01-03T00:00:00", "0000-00-01T00:00:00"):
    ...     print(d)
    2024-01-01T00:00:00
    2024-01-02T00:00:00
    2024-01-03T00:00:00
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
    A class to handle various calendar systems.

    Parameters
    ----------
    calendar_type : Union[str, int], optional
        The type of calendar to use. Acceptable values:
          - "no_leap" or 0: No leap years.
          - "gregorian" or 1: Proleptic Gregorian calendar (default).
          - Any integer value: Represents a calendar with equal months of given days.

    Attributes
    ----------
    TIME_UNITS : List[str]
        List of accepted time units: years, months, days, hours, minutes, and seconds.
    MONTH_NAMES : List[str]
        List of month names as 3-letter English abbreviations.

    Methods
    -------
    is_leap_year(year: int) -> bool
        Returns True if the specified year is a leap year.
    days_in_year(year: int) -> int
        Returns the total number of days in the specified year.
    days_in_month(year: int, month: Union[int, str]) -> int
        Returns the number of days in a given month for a given year.
    """

    TIME_UNITS: List[str] = ["years", "months", "days", "hours", "minutes", "seconds"]
    MONTH_NAMES: List[str] = [
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

    @staticmethod
    @deprecated(
        reason="Using 'calendar_type = 1' is deprecated, use 'calendar_type = \"gregorian\"' instead."
    )
    def _set_gregorian() -> str:
        """
        (Deprecated) Returns the Gregorian calendar type.
        """
        return "gregorian"

    @staticmethod
    @deprecated(
        reason="Using 'calendar_type = 0' is deprecated, use 'calendar_type = \"no_leap\"' instead."
    )
    def _set_no_leap() -> str:
        """
        (Deprecated) Returns the no-leap calendar type.
        """
        return "no_leap"

    def __init__(self, calendar_type: Union[str, int] = "gregorian") -> None:
        # Support deprecated integer values for backwards compatibility.
        if calendar_type == 1:
            calendar_type = self._set_gregorian()
        if calendar_type == 0:
            calendar_type = self._set_no_leap()
        self.calendar_type = calendar_type

    def is_leap_year(self, year: int) -> bool:
        """
        Checks if a year is a leap year based on the calendar type.

        Parameters
        ----------
        year : int
            The year to evaluate.

        Returns
        -------
        bool
            True if the year is a leap year (for Gregorian), False otherwise.

        Examples
        --------
        >>> cal = Calendar("gregorian")
        >>> cal.is_leap_year(2024)
        True
        >>> cal.is_leap_year(1900)
        False
        """
        if self.calendar_type == "gregorian":
            return year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)
        # For 'no_leap' and other calendar types, adjust accordingly.
        return False

    def days_in_year(self, year: int) -> int:
        """
        Returns the total number of days in a given year.

        Parameters
        ----------
        year : int
            The year to evaluate.

        Returns
        -------
        int
            Total number of days in the year (366 for leap years in the Gregorian calendar).

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
        # For equal-month calendars, assume the provided integer indicates days per month.
        return int(self.calendar_type) * 12

    def days_in_month(self, year: int, month: Union[int, str]) -> int:
        """
        Returns the number of days in a specific month for a given year.

        Parameters
        ----------
        year : int
            The year.
        month : int or str
            The month (either as an integer 1-12 or a 3-letter month name).

        Returns
        -------
        int
            Number of days in the month.

        Raises
        ------
        ValueError
            If the month is invalid.

        Examples
        --------
        >>> cal = Calendar("gregorian")
        >>> cal.days_in_month(2024, 2)
        29
        """
        if isinstance(month, str):
            try:
                month = self.MONTH_NAMES.index(month.capitalize()) + 1
            except ValueError:
                raise ValueError("Invalid month name provided.")
        if not 1 <= month <= 12:
            raise ValueError("Invalid month. Must be between 1 and 12.")

        if self.calendar_type in ["no_leap", "gregorian"]:
            if month in [1, 3, 5, 7, 8, 10, 12]:
                return 31
            if month in [4, 6, 9, 11]:
                return 30
            return 28 + int(self.is_leap_year(year))
        return int(self.calendar_type)

    def __repr__(self) -> str:
        return f"Calendar(calendar_type={self.calendar_type})"

    def __str__(self) -> str:
        if self.calendar_type == "no_leap":
            return "Calendar object with no leap years allowed"
        if self.calendar_type == "gregorian":
            return "Calendar object with allowed leap years"
        return f"Calendar object with equal-length months of {self.calendar_type} days"


class DateFormat:
    """
    Class for handling date formatting configuration.

    Parameters
    ----------
    form : int
        The format code determining the output layout.
    print_hours : bool
        Whether hours are printed.
    print_minutes : bool
        Whether minutes are printed.
    print_seconds : bool
        Whether seconds are printed.

    Attributes
    ----------
    datesep : dict
        Mapping between format codes and date separators.
    dtsep : dict
        Mapping between format codes and datetime separators.
    timesep : dict
        Mapping between format codes and time separators.

    Examples
    --------
    Using the date 2001-11-03 12:34:56, the formats would be:

    Format 0: 2001-11-03T12:34:56
    Format 1: 2001-11-03_12:34:56
    Format 2: 2001-11-03T12:34:56
    Format 3: 2001-11-03 12:34:56
    Format 4: 2001 11 03 12 34 56
    Format 5: 2001 11 03 12:34:56
    Format 6: 20011103_12:34:56
    Format 7: 2001-11-03_123456
    Format 8: 20011103 123456
    Format 9: 20011103_123456
    Format 10: 2001/11/03 12:34:56
    """

    def __init__(
        self, form: int, print_hours: bool, print_minutes: bool, print_seconds: bool
    ) -> None:
        self.form = form
        self.print_hours = print_hours
        self.print_minutes = print_minutes
        self.print_seconds = print_seconds
        self.datesep = {
            0: "-",
            1: "-",
            2: "-",
            3: "-",
            4: " ",
            5: " ",
            6: "",
            7: "-",
            8: "",
            9: "",
            10: "/",
        }
        self.dtsep = {
            0: "T",
            1: "_",
            2: "T",
            3: " ",
            4: " ",
            5: " ",
            6: "_",
            7: "_",
            8: "",
            9: "_",
            10: " ",
        }
        self.timesep = {
            0: ":",
            1: ":",
            2: ":",
            3: ":",
            4: " ",
            5: ":",
            6: ":",
            7: "",
            8: "",
            9: "",
            10: ":",
        }

    def __repr__(self) -> str:
        return f"DateFormat(form={self.form}, print_hours={self.print_hours}, print_minutes={self.print_minutes}, print_seconds={self.print_seconds})"


class DateDelta:
    """
    A class to represent the difference between two dates.

    Attributes
    ----------
    years : int
        The difference in years.
    months : int
        The difference in months.
    days : int
        The difference in days.
    hours : int
        The difference in hours.
    minutes : int
        The difference in minutes.
    seconds : int
        The difference in seconds.
    number_leap_years : int
        The number of leap years between the two dates.
    """

    def __init__(
        self,
        years: int,
        months: int,
        days: int,
        hours: int,
        minutes: int,
        seconds: int,
        number_leap_years: int = 0,
    ):
        self.years = years
        self.months = months
        self.days = days
        self.hours = hours
        self.minutes = minutes
        self.seconds = seconds
        self.number_leap_years = number_leap_years

    # Support for indexing and transforming to tuple

    def _as_tuple(self) -> Tuple[int, int, int, int, int, int]:
        """
        Return the date components as a tuple.

        Returns
        -------
        tuple
            A tuple of (years, months, days, hours, minutes, seconds).
        """
        return (
            self.years,
            self.months,
            self.days,
            self.hours,
            self.minutes,
            self.seconds,
        )

    def __iter__(self):
        return iter(self._as_tuple())

    def __getitem__(self, index: int) -> int:
        return self._as_tuple()[index]

    @classmethod
    def from_dates(
        cls, date1: "Date", date2: "Date", calendar: Optional[Calendar] = None
    ) -> "DateDelta":
        """
        Create a DateDelta object from two Date objects.

        Parameters
        ----------
        date1 : Date
            The first Date object.
        date2 : Date
            The second Date object.
        calendar : Calendar, optional
            An object that provides methods `days_in_year` and `is_leap_year`. Defaults
            to a gregorian calendar.

        Returns
        -------
        DateDelta
            The difference between the two dates.
        """
        if calendar is None:
            calendar = Calendar("gregorian")

        # Swap dates if necessary
        date1, date2 = (date1, date2) if date1 > date2 else (date2, date1)
        diff = date1 - date2

        # Count the number of leap years between date1 and date2
        leap_years = sum(
            1
            for year in range(date2.year, date1.year + 1)
            if calendar.is_leap_year(year)
        )

        return cls(*diff, number_leap_years=leap_years)

    def __repr__(self):
        return (
            f"DateDelta(years={self.years}, months={self.months}, days={self.days}, "
            f"hours={self.hours}, minutes={self.minutes}, seconds={self.seconds}, "
            f"number_leap_years={self.number_leap_years})"
        )

    def accumulate(
        self,
        start_date: "Date",
        end_date: "Date",
        calendar: Optional[Calendar] = None,
        rtype: type = dict,
    ):
        """
        Accumulate the differences into total months, days, hours, minutes, and seconds if it
        is applied from start_date to end_date.

        Parameters
        ----------
        start_date : Date
            The starting date.
        end_date : Date
            The ending date.
        calendar : Calendar, optional
            An object that provides methods `days_in_year` and `days_in_month`. Defaults
            to a gregorian calendar.
        rtype : type, optional
            The return type. Defaults to dict.

        Returns
        -------
        obj:
            The accumulated differences, as either dict, list, or tuple.

        Raises
        ------
        ValueError
            If an unsupported return type is provided.
        """
        if calendar is None:
            calendar = Calendar("gregorian")

        # Determine the number of days in a year and a month using the start_date
        days_in_year = calendar.days_in_year(start_date.year)
        days_in_month = calendar.days_in_month(start_date.year, start_date.month)

        total_months = self.years * 12 + self.months
        total_days = (
            self.years * days_in_year
            + self.months * days_in_month
            + self.days
            + self.number_leap_years  # Adjust for leap years
        )
        total_hours = total_days * 24 + self.hours
        total_minutes = total_hours * 60 + self.minutes
        total_seconds = total_minutes * 60 + self.seconds

        if rtype == dict:
            return {
                "total_years": self.years,
                "total_months": total_months,
                "total_days": total_days,
                "total_hours": total_hours,
                "total_minutes": total_minutes,
                "total_seconds": total_seconds,
            }
        elif rtype == list:
            return [
                self.years,
                total_months,
                total_days,
                total_hours,
                total_minutes,
                total_seconds,
            ]
        elif rtype == tuple:
            return (
                self.years,
                total_months,
                total_days,
                total_hours,
                total_minutes,
                total_seconds,
            )
        else:
            raise ValueError(f"Unsupported return type: {rtype}")


class Date:
    """
    Class for handling dates and times, including support for paleo (negative) dates.

    Parameters
    ----------
    indate : Union[str, Date]
        A string representation of the date or another Date object.
    calendar : Optional[Calendar], optional
         Calendar system to use. Defaults to a Gregorian calendar.

    Attributes
    ----------
    year : int
        Year component.
    month : int
        Month component.
    day : int
        Day component.
    hour : int
        Hour component.
    minute : int
        Minute component.
    second : int
        Second component.
    calendar : Calendar
        The calendar system.
    doy : int
        Day of the year.
    _date_format : DateFormat
        Date formatting configuration.

    Examples
    --------
    .. code-block:: python

        >>> d = Date("2024-02-16T11:30:00")
        >>> print(d)
        2024-02-16T11:30:00
        >>> d.day_of_year()
        47
    """

    def __init__(
        self, indate: Union[str, "Date"], calendar: Optional[Calendar] = None
    ) -> None:
        self.calendar: Calendar = calendar or Calendar("gregorian")
        self.year: int = 0
        self.month: int = 0
        self.day: int = 0
        self.hour: int = 0
        self.minute: int = 0
        self.second: int = 0
        self.doy: int = 0
        self._date_format: DateFormat = DateFormat(0, True, True, True)

        if isinstance(indate, str):
            self._init_from_str(indate)
        elif isinstance(indate, Date):
            self._init_from_date(indate)
        else:
            raise TypeError(
                f"{type(indate)} is not valid to initialize a Date object (valid types: str, Date)"
            )

    def _init_from_str(self, indate: str) -> None:
        printhours = True
        printminutes = True
        printseconds = True
        components: List[str] = ["1900", "01", "01", "00", "00", "00"]
        time_sep_used = ""
        original_format = "T" if "T" in indate else "_"

        working_date = indate.replace("T", "_") if "T" in indate else indate

        if "_" in working_date:
            date_part, time_part = working_date.split("_", 1)
            time_sep_used = ":" if ":" in time_part else ""
        elif ":" in working_date and "-" not in working_date:
            date_part = "0000-00-00"
            time_part = working_date
            time_sep_used = ":"
        else:
            date_part = working_date
            time_part = ""
            time_sep_used = ":"

        # Process time components
        time = time_part
        for idx in [3, 4, 5]:
            if len(time) >= 2:
                components[idx] = time[:2]
                time = time[2:]
                if time and time[0] == ":":
                    time = time[1:]
            else:
                components[idx] = "00"
                if idx == 3:
                    printhours = False
                elif idx == 4:
                    printminutes = False
                elif idx == 5:
                    printseconds = False

        # Process date components
        date = date_part
        date_sep = "-" if "-" in date else ""
        for idx in [2, 1]:
            if date:
                components[idx] = date[-2:]
                date = date[:-2]
                if date and date[-1] == "-":
                    date = date[:-1]
        components[0] = date if date else components[0]

        # Set the format based on the original separators
        form = self._determine_format(date_sep, time_sep_used, original_format == "T")

        self.year, self.month, self.day, self.hour, self.minute, self.second = map(
            int, components
        )
        self.doy = self.day_of_year()
        self._date_format = DateFormat(form, printhours, printminutes, printseconds)

    def _determine_format(self, date_sep: str, time_sep: str, has_t: bool) -> int:
        """
        Determines a format code based on the separators in the input string.

        Parameters
        ----------
        date_sep : str
            The date separator used.
        time_sep : str
            The time separator used.
        has_t : bool
            Flag indicating whether 'T' was in the original string.

        Returns
        -------
        int
            The determined format code.
        """
        if date_sep == "-" and time_sep == ":":
            return 2 if has_t else 1
        elif date_sep == "-" and not time_sep:
            return 7
        elif not date_sep and time_sep == ":":
            return 6
        return 9

    def _init_from_date(self, other: "Date") -> None:
        """
        Initialize a Date object from another Date instance.

        Parameters
        ----------
        other : Date
            The source Date object.
        """
        self.year = other.year
        self.month = other.month
        self.day = other.day
        self.hour = other.hour
        self.minute = other.minute
        self.second = other.second
        self.calendar = other.calendar
        self.doy = other.day_of_year()
        self._date_format = other._date_format

    @property
    def syear(self) -> str:
        """
        Returns a zero-padded string of the year.

        Returns
        -------
        str
            The year as a 4-digit string.
        """
        return f"{self.year:04d}"

    @property
    def sdoy(self) -> str:
        """
        Returns a string of the day of the year.

        Returns
        -------
        str
            The day of the year as a 3-digit string.
        """
        return f"{self.day_of_year()}"

    @property
    def smonth(self) -> str:
        """
        Returns a zero-padded string of the month.

        Returns
        -------
        str
            The month as a 2-digit string.
        """
        return f"{self.month:02d}"

    @property
    def sday(self) -> str:
        """
        Returns a zero-padded string of the day.

        Returns
        -------
        str
            The day as a 2-digit string.
        """
        return f"{self.day:02d}"

    @property
    def shour(self) -> str:
        """
        Returns a zero-padded string of the hour.

        Returns
        -------
        str
            The hour as a 2-digit string.
        """
        return f"{self.hour:02d}"

    @property
    def sminute(self) -> str:
        """
        Returns a zero-padded string of the minute.

        Returns
        -------
        str
            The minute as a 2-digit string.
        """
        return f"{self.minute:02d}"

    @property
    def ssecond(self) -> str:
        """
        Returns a zero-padded string of the second.

        Returns
        -------
        str
            The second as a 2-digit string.
        """
        return f"{self.second:02d}"

    def day_of_year(self) -> int:
        """
        Compute the day of the year.

        Returns
        -------
        int
            The day number (1 through 366).

        Examples
        --------
        >>> d = Date("2024-02-16T00:00:00")
        >>> d.day_of_year()
        47
        """
        if self.month == 1:
            return self.day

        # Sum days in all completed months
        days = 0
        for month in range(1, self.month):
            days += self.calendar.days_in_month(self.year, month)

        # Add days in current month
        days += self.day

        return days

    def time_between(self, other: "Date", unit: str = "seconds") -> Optional[int]:
        """
        Compute the time difference between two dates in a specified unit.

        Parameters
        ----------
        other : Date
            The date to compare against.
        unit : str, optional
            The unit of time (years, months, days, hours, minutes, seconds).

        Returns
        -------
        int:
            The time difference in the specified unit.

        Raises
        ------
        ValueError
            If an unsupported time unit is provided.
        """
        if unit not in self.calendar.TIME_UNITS:
            return None

        diff = self - other if self > other else other - self

        # Convert to the requested unit
        if unit == "seconds":
            return (
                (diff[0] * 365 + diff[1] * 30 + diff[2]) * 86400
                + diff[3] * 3600
                + diff[4] * 60
                + diff[5]
            )
        elif unit == "minutes":
            return (
                (diff[0] * 365 + diff[1] * 30 + diff[2]) * 1440 + diff[3] * 60 + diff[4]
            )
        elif unit == "hours":
            return (diff[0] * 365 + diff[1] * 30 + diff[2]) * 24 + diff[3]
        elif unit == "days":
            return diff[0] * 365 + diff[1] * 30 + diff[2]
        elif unit == "months":
            return diff[0] * 12 + diff[1]
        elif unit == "years":
            return diff[0]

        raise ValueError(f"Unsupported time unit: {unit}")

    def output(self, form="SELF") -> str:
        """Shortcut method to nicely print a Date"""
        return self.format(form)

    def format(
        self,
        form: Union[str, int] = "SELF",
        print_hours: Optional[bool] = None,
        print_minutes: Optional[bool] = None,
        print_seconds: Optional[bool] = None,
    ) -> str:
        """
        Format the Date into a string representation.

        Parameters
        ----------
        form : Union[str, int]
            Format style ("SELF" to use the original format or an int code, e.g. 0-10).
        print_hours : bool, optional
            Override printing of hours.
        print_minutes : bool, optional
            Override printing of minutes.
        print_seconds : bool, optional
            Override printing of seconds.

        Returns
        -------
        str
            The formatted date string.

        Examples
        --------
        >>> d = Date("2024-02-16T11:30:00")
        >>> d.format(form=2)
        '2024-02-16T11:30:00'
        >>> d.format(form=5)
        '16 Feb 2024 11:30:00'
        """
        if form == "SELF":
            form = self._date_format.form

        ph = print_hours if print_hours is not None else self._date_format.print_hours
        pm = (
            print_minutes
            if print_minutes is not None
            else self._date_format.print_minutes
        )
        ps = (
            print_seconds
            if print_seconds is not None
            else self._date_format.print_seconds
        )

        components = [
            str(self.year),
            self.smonth,
            self.sday,
            self.shour,
            self.sminute,
            self.ssecond,
        ]

        if form == 0 and len(components[0]) < 4:
            components[0] = components[0].zfill(4)
        elif form == 5:
            # Swap day and year, use month name.
            components[0], components[2] = components[2], components[0]
            components[1] = self.calendar.MONTH_NAMES[int(components[1]) - 1]
        elif form == 10:
            # Rotate date components.
            components[0], components[1], components[2] = (
                components[1],
                components[2],
                components[0],
            )

        date_sep = self._date_format.datesep.get(form, "")
        dt_sep = self._date_format.dtsep.get(form, "")
        time_sep = self._date_format.timesep.get(form, "")

        result = components[0] + date_sep + components[1] + date_sep + components[2]
        if ph:
            result += dt_sep + components[3]
            if pm:
                result += time_sep + components[4]
                if ps:
                    result += time_sep + components[5]
        return result

    def __str__(self) -> str:
        """
        Return the ISO-like string representation of the Date.

        Returns
        -------
        str
            A string in the form 'YYYY-MM-DDTHH:MM:SS'.
        """
        return f"{self.year:04d}-{self.month:02d}-{self.day:02d}T{self.hour:02d}:{self.minute:02d}:{self.second:02d}"

    def __repr__(self) -> str:
        return f"Date({self.format()})"

    def _as_tuple(self) -> Tuple[int, int, int, int, int, int]:
        """
        Return the date components as a tuple.

        Returns
        -------
        tuple
            A tuple of (year, month, day, hour, minute, second).
        """
        return (self.year, self.month, self.day, self.hour, self.minute, self.second)

    def __getitem__(self, index: int) -> int:
        return self._as_tuple()[index]

    def __setitem__(self, index: int, value: int) -> None:
        if not 0 <= index <= 5:
            raise IndexError("Index must be between 0 and 5")
        attrs = ["year", "month", "day", "hour", "minute", "second"]
        setattr(self, attrs[index], int(value))

    def __lt__(self, other: "Date") -> bool:
        return self._as_tuple() < other._as_tuple()

    def __le__(self, other: "Date") -> bool:
        return self._as_tuple() <= other._as_tuple()

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Date):
            return False
        return self._as_tuple() == other._as_tuple()

    def __ne__(self, other: object) -> bool:
        return not self.__eq__(other)

    def __gt__(self, other: "Date") -> bool:
        return self._as_tuple() > other._as_tuple()

    def __ge__(self, other: "Date") -> bool:
        return self._as_tuple() >= other._as_tuple()

    def __sub__(
        self, other: Union["Date", Tuple[int, int, int, int, int, int]]
    ) -> List[int]:
        """
        Subtract another Date or a tuple of time components from this Date.

        Parameters
        ----------
        other : Date or tuple of int
            The Date or tuple to subtract.

        Returns
        -------
        list of int
            Differences as [years, months, days, hours, minutes, seconds]. These diffs
            represent the cumulative time between the two dates in each of the units.

        Examples
        --------
        >>> d1 = Date("2024-02-16T12:30:00")
        >>> d2 = Date("2024-02-16T11:30:00")
        >>> d1 - d2
        [0, 0, 0, 1, 60, 3600]
        """
        if isinstance(other, Date):
            return self._sub_date(other)
        elif isinstance(other, tuple):
            return self._sub_tuple(other)
        else:
            raise TypeError(f"Unsupported type for subtraction: {type(other)}")

    ################################################################################
    def _sub_date(self, other: "Date") -> List[int]:
        """
        Subtract one Date from another component-wise.

        Parameters
        ----------
        other : Date
            The Date to subtract from this Date.

        Returns
        -------
        list of int
            Differences as [years, months, days, hours, minutes, seconds].
        """
        d1 = list(other._as_tuple())  # The earlier date
        d2 = list(self._as_tuple())  # The later date

        # Initialize the difference list
        diff = [0, 0, 0, 0, 0, 0]

        # Handle seconds
        if d2[5] < d1[5]:
            d2[4] -= 1
            d2[5] += 60
        diff[5] = d2[5] - d1[5]

        # Handle minutes
        if d2[4] < d1[4]:
            d2[3] -= 1
            d2[4] += 60
        diff[4] = d2[4] - d1[4]

        # Handle hours
        if d2[3] < d1[3]:
            d2[2] -= 1
            d2[3] += 24
        diff[3] = d2[3] - d1[3]

        # Handle days
        if d2[2] < d1[2]:
            d2[1] -= 1
            if d2[1] == 0:
                d2[0] -= 1
                d2[1] = 12
            d2[2] += self.calendar.days_in_month(d2[0], d2[1])
        diff[2] = d2[2] - d1[2]

        # Handle months
        if d2[1] < d1[1]:
            d2[0] -= 1
            d2[1] += 12
        diff[1] = d2[1] - d1[1]

        # Handle years
        diff[0] = d2[0] - d1[0]

        # FIXME(PG): Mimic old behavior with the accumulated differences
        return DateDelta(*diff).accumulate(
            start_date=other,
            end_date=self,
            calendar=self.calendar,
            rtype=list,
        )

    def _sub_tuple(self, to_sub: Tuple[int, int, int, int, int, int]) -> List[int]:
        """
        Subtract a tuple of time components from this Date.

        Parameters
        ----------
        to_sub : tuple of int
            A tuple in the form (years, months, days, hours, minutes, seconds).

        Returns
        -------
        list of int
            Normalized differences as [years, months, days, hours, minutes, seconds].
        """
        me = list(self._as_tuple())
        result = [me[i] - to_sub[i] for i in range(6)]
        result = self._normalize_date(result)
        return self.from_list(result)

    def _normalize_date(self, comps: List[int]) -> List[int]:
        """
        Normalize the time components, cascading overflows correctly.

        Parameters
        ----------
        comps : list of int
            A list of date components [year, month, day, hour, minute, second].

        Returns
        -------
        list of int
            The normalized date components.
        """
        comps[4] += comps[5] // 60
        comps[5] %= 60

        comps[3] += comps[4] // 60
        comps[4] %= 60

        comps[2] += comps[3] // 24
        comps[3] %= 24

        comps[0] += (comps[1] - 1) // 12
        comps[1] = (comps[1] - 1) % 12 + 1

        while comps[2] > self.calendar.days_in_month(comps[0], comps[1]):
            comps[2] -= self.calendar.days_in_month(comps[0], comps[1])
            comps[1] += 1
            if comps[1] > 12:
                comps[1] = 1
                comps[0] += 1

        while comps[2] <= 0:
            comps[1] -= 1
            if comps[1] == 0:
                comps[1] = 12
                comps[0] -= 1
            comps[2] += self.calendar.days_in_month(comps[0], comps[1])

        return comps

    def add(self, other: "Date") -> "Date":
        """
        Add another Date's components to this Date.

        Parameters
        ----------
        other : Date
            The Date whose components will be added.

        Returns
        -------
        Date
            A new Date with added components.

        Examples
        --------
        >>> d1 = Date("2024-02-16T11:30:00")
        >>> d2 = Date("0000-00-00T01:00:00")
        >>> d3 = d1 + d2
        >>> print(d3)
        2024-02-16T12:30:00
        """
        my_comps = list(self._as_tuple())
        other_comps = [other[i] for i in range(6)]
        result = [my_comps[i] + other_comps[i] for i in range(6)]
        result = self._normalize_date(result)
        return Date.from_list(result)

    __add__ = add

    @classmethod
    def from_list(cls, components: List[int]) -> "Date":
        """
        Create a Date from a list of integer components.

        Parameters
        ----------
        components : list of int
            List in the order [year, month, day, hour, minute, second].

        Returns
        -------
        Date
            A new Date instance.

        Examples
        --------
        .. code-block:: python

            >>> Date.from_list([2024, 2, 16, 11, 30, 0])
            Date(2024-02-16_11:30:00)
        """
        signs = ["-" if n < 0 else "" for n in components]
        str_components = [str(abs(n)) for n in components]
        str_components[0] = str_components[0].zfill(4)
        for i in range(1, 6):
            str_components[i] = str_components[i].zfill(2)
        date_str = (
            signs[0]
            + str_components[0]
            + "-"
            + signs[1]
            + str_components[1]
            + "-"
            + signs[2]
            + str_components[2]
            + "_"
            + signs[3]
            + str_components[3]
            + ":"
            + signs[4]
            + str_components[4]
            + ":"
            + signs[5]
            + str_components[5]
        )
        return cls(date_str)

    fromlist = from_list
    """Alias for :meth:`from_list`."""
