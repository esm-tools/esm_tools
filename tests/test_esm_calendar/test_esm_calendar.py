import pytest

from esm_calendar import Calendar, find_remaining_minutes


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
    assert cal.is_leap_year(2024) == True


def test_is_leap_year_gregorian_false():
    cal = Calendar("gregorian")
    assert cal.is_leap_year(2023) == False


def test_is_leap_year_gregorian_century_false():
    cal = Calendar("gregorian")
    assert cal.is_leap_year(1900) == False


def test_is_leap_year_gregorian_century_true():
    cal = Calendar("gregorian")
    assert cal.is_leap_year(2000) == True


def test_is_leap_year_no_leap():
    cal = Calendar("no_leap")
    assert cal.is_leap_year(2024) == False


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
