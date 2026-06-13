"""Generic GRIB scanning via a tiny committed fixture."""

from __future__ import annotations

import pathlib

from esm_catalog.scan.detect import scan_file
from esm_catalog.scan.grib import scan_grib

FIX = pathlib.Path(__file__).parent / "fixtures" / "tiny.grib2"


def test_scan_grib_fixture():
    md = scan_grib(FIX)
    assert md["format"] == "grib"
    assert md["file_size"] > 0


def test_scan_grib_keys():
    md = scan_grib(FIX)
    expected_keys = {
        "variable",
        "stream",
        "variables",
        "cf_parameters",
        "dimensions",
        "bbox",
        "geometry",
        "datetime_start",
        "datetime_end",
        "datetime_str",
        "file_size",
        "conventions",
        "format",
    }
    assert expected_keys == set(md.keys())


def test_scan_grib_variable():
    md = scan_grib(FIX)
    # tiny.grib2 has paramId=130, shortName="t" (temperature)
    assert md["variable"] == "t"
    assert len(md["variables"]) >= 1
    assert md["variables"][0]["name"] == "t"


def test_scan_grib_cf_parameters():
    md = scan_grib(FIX)
    # Temperature maps to air_temperature
    assert any(p["name"] == "air_temperature" for p in md["cf_parameters"])


def test_scan_grib_datetime():
    md = scan_grib(FIX)
    # tiny.grib2 has dataDate=20000101
    assert md["datetime_start"] is not None
    assert md["datetime_str"] == "200001"


def test_scan_file_dispatches_grib():
    md = scan_file(FIX)
    assert md["format"] == "grib"
