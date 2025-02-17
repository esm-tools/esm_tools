import pytest

from esm_calendar import Calendar, Date, DateFormat, find_remaining_minutes


def test_find_remaining_minutes_typical_value():
    assert find_remaining_minutes(125) == 5


def test_find_remaining_minutes_zero_seconds():
    assert find_remaining_minutes(0) == 0


def test_find_remaining_minutes_exactly_one_minute():
    assert find_remaining_minutes(60) == 0


def test_find_remaining_minutes_less_than_a_minute():
    assert find_remaining_minutes(59) == 59


def test_find_remaining_minutes_multiple_minutes():
    assert find_remaining_minutes(3600) == 0


def test_find_remaining_minutes_large_number_of_seconds():
    assert find_remaining_minutes(987654321) == 21


def test_find_remaining_minutes_invalid_type_string():
    with pytest.raises(TypeError):
        find_remaining_minutes("125")


def test_find_remaining_minutes_invalid_type_float():
    with pytest.raises(TypeError):
        find_remaining_minutes(125.5)


def test_find_remaining_minutes_negative_value():
    with pytest.raises(ValueError):
        find_remaining_minutes(-125)


def test_is_leap_year_gregorian_true():
    cal = Calendar("gregorian")
    assert cal.is_leap_year(2024) is True


def test_is_leap_year_gregorian_false():
    cal = Calendar("gregorian")
    assert cal.is_leap_year(2023) is False


def test_is_leap_year_gregorian_century_false():
    cal = Calendar("gregorian")
    assert cal.is_leap_year(1900) is False


def test_is_leap_year_gregorian_century_true():
    cal = Calendar("gregorian")
    assert cal.is_leap_year(2000) is True


def test_is_leap_year_no_leap():
    cal = Calendar("no_leap")
    assert cal.is_leap_year(2024) is False


def test_days_in_year_gregorian_leap():
    cal = Calendar("gregorian")
    assert cal.days_in_year(2024) == 366


def test_days_in_year_gregorian_non_leap():
    cal = Calendar("gregorian")
    assert cal.days_in_year(2023) == 365


def test_days_in_year_no_leap():
    cal = Calendar("no_leap")
    assert cal.days_in_year(2024) == 365


def test_days_in_month_gregorian_january():
    cal = Calendar("gregorian")
    assert cal.days_in_month(2024, 1) == 31


def test_days_in_month_gregorian_february_leap():
    cal = Calendar("gregorian")
    assert cal.days_in_month(2024, 2) == 29


def test_days_in_month_gregorian_february_non_leap():
    cal = Calendar("gregorian")
    assert cal.days_in_month(2023, 2) == 28


def test_days_in_month_gregorian_april():
    cal = Calendar("gregorian")
    assert cal.days_in_month(2024, 4) == 30


def test_days_in_month_no_leap_february():
    cal = Calendar("no_leap")
    assert cal.days_in_month(2024, 2) == 28


def test_days_in_month_invalid_month():
    cal = Calendar("gregorian")
    with pytest.raises(ValueError):
        cal.days_in_month(2024, 13)


def test_days_in_month_string_month():
    cal = Calendar("gregorian")
    assert cal.days_in_month(2024, "Feb") == 29


def test_repr():
    cal = Calendar("gregorian")
    assert repr(cal) == "Calendar(calendar_type=gregorian)"


def test_str_no_leap():
    cal = Calendar("no_leap")
    assert str(cal) == "Calendar object with no leap years allowed"


def test_str_gregorian():
    cal = Calendar("gregorian")
    assert str(cal) == "Calendar object with allowed leap years"


def test_str_equal_months():
    cal = Calendar(30)
    assert str(cal) == "Calendar object with equal-length months of 30 days"


def test_date_initialization_from_string():
    d = Date("2024-02-16T11:30:00")
    assert str(d) == "2024-02-16T11:30:00"


def test_date_initialization_from_date():
    d1 = Date("2024-02-16T11:30:00")
    d2 = Date(d1)
    assert str(d2) == "2024-02-16T11:30:00"


def test_date_year_component():
    d = Date("2024-02-16T11:30:00")
    assert d.year == 2024


def test_date_month_component():
    d = Date("2024-02-16T11:30:00")
    assert d.month == 2


def test_date_day_component():
    d = Date("2024-02-16T11:30:00")
    assert d.day == 16


def test_date_hour_component():
    d = Date("2024-02-16T11:30:00")
    assert d.hour == 11


def test_date_minute_component():
    d = Date("2024-02-16T11:30:00")
    assert d.minute == 30


def test_date_second_component():
    d = Date("2024-02-16T11:30:00")
    assert d.second == 0


def test_date_day_of_year():
    d = Date("2024-02-16T00:00:00")
    assert d.day_of_year() == 47


def test_date_format_self():
    d = Date("2024-02-16T11:30:00")
    assert d.format() == "2024-02-16T11:30:00"


def test_date_format_1():
    d = Date("2024-02-16T11:30:00")
    assert d.format(form=1) == "2024-02-16_11:30:00"


def test_date_format_2():
    d = Date("2024-02-16T11:30:00")
    assert d.format(form=2) == "2024-02-16T11:30:00"


def test_date_format_3():
    d = Date("2024-02-16T11:30:00")
    assert d.format(form=3) == "2024-02-16 11:30:00"


def test_date_format_4():
    d = Date("2024-02-16T11:30:00")
    assert d.format(form=4) == "2024 02 16 11 30 00"


def test_date_format_5():
    d = Date("2024-02-16T11:30:00")
    assert d.format(form=5) == "16 Feb 2024 11:30:00"


def test_date_syear():
    d = Date("2024-02-16T11:30:00")
    assert d.syear == "2024"


def test_date_smonth():
    d = Date("2024-02-16T11:30:00")
    assert d.smonth == "02"


def test_date_sday():
    d = Date("2024-02-16T11:30:00")
    assert d.sday == "16"


def test_date_shour():
    d = Date("2024-02-16T11:30:00")
    assert d.shour == "11"


def test_date_sminute():
    d = Date("2024-02-16T11:30:00")
    assert d.sminute == "30"


def test_date_ssecond():
    d = Date("2024-02-16T11:30:00")
    assert d.ssecond == "00"


def test_date_time_between():
    d1 = Date("2024-02-16T12:30:00")
    d2 = Date("2024-02-16T11:30:00")
    assert d1.time_between(d2, "hours") == 1


def test_date_subtraction():
    d1 = Date("2024-02-16T12:30:00")
    d2 = Date("2024-02-16T11:30:00")
    assert d1 - d2 == [0, 0, 0, 1, 60, 3600]


def test_date_addition():
    d1 = Date("2024-02-16T11:30:00")
    d2 = Date("0000-00-00T01:00:00")
    d3 = d1 + d2
    assert str(d3) == "2024-02-16T12:30:00"


def test_date_from_list():
    d = Date.from_list([2024, 2, 16, 11, 30, 0])
    assert str(d) == "2024-02-16T11:30:00"


def test_date_comparison_lt():
    d1 = Date("2024-02-16T11:30:00")
    d2 = Date("2024-02-17T11:30:00")
    assert d1 < d2


def test_date_comparison_le():
    d1 = Date("2024-02-16T11:30:00")
    d2 = Date("2024-02-16T11:30:00")
    assert d1 <= d2


def test_date_comparison_eq():
    d1 = Date("2024-02-16T11:30:00")
    d2 = Date("2024-02-16T11:30:00")
    assert d1 == d2


def test_date_comparison_ne():
    d1 = Date("2024-02-16T11:30:00")
    d2 = Date("2024-02-17T11:30:00")
    assert d1 != d2


def test_date_comparison_gt():
    d1 = Date("2024-02-17T11:30:00")
    d2 = Date("2024-02-16T11:30:00")
    assert d1 > d2


def test_date_comparison_ge():
    d1 = Date("2024-02-16T11:30:00")
    d2 = Date("2024-02-16T11:30:00")
    assert d1 >= d2
