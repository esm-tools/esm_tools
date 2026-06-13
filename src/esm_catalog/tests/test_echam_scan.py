"""ECHAM-specific helpers: codes-file parsing, stream/CF mapping, detection."""

from __future__ import annotations

from pathlib import Path

from esm_catalog.scan.echam import (
    _extract_stream_type,
    _indicator_to_cf,
    _parse_codes_file,
    is_echam_file,
)


def test_parse_codes_file(tmp_path):
    codes = tmp_path / "x.codes"
    codes.write_text("130    1 st      0.      1. surface temperature [K]\n")
    table = _parse_codes_file(codes)
    assert table[130]["shortName"] == "st"
    assert table[130]["units"] == "K"
    assert table[130]["longName"] == "surface temperature"


def test_parse_codes_file_no_units(tmp_path):
    """Lines without bracket units are handled gracefully."""
    codes = tmp_path / "y.codes"
    codes.write_text("5    1 sn      0.      1. snow depth\n")
    table = _parse_codes_file(codes)
    assert table[5]["shortName"] == "sn"
    assert table[5]["units"] == ""
    assert table[5]["longName"] == "snow depth"


def test_parse_codes_file_skips_comments(tmp_path):
    codes = tmp_path / "z.codes"
    codes.write_text(
        "# comment line\n"
        "\n"
        "130    1 st      0.      1. surface temperature [K]\n"
    )
    table = _parse_codes_file(codes)
    assert len(table) == 1


def test_extract_stream_type():
    assert _extract_stream_type(Path("basic-001_185001.01_echam")) == "echam"
    assert _extract_stream_type(Path("basic-001_185001.01_co2")) == "co2"
    assert _extract_stream_type(Path("basic-001_185002.01_echam_18500201-18500228")) == "echam"
    assert _extract_stream_type(Path("basic-001_185001.01_accw")) == "accw"


def test_indicator_to_cf():
    assert _indicator_to_cf(130, "st") == "surface_temperature"
    assert _indicator_to_cf(999, "unknown_xyz") is None
    # Standard temperature shortName
    assert _indicator_to_cf(130, "t") == "air_temperature"


def test_is_echam_file_by_codes_companion(tmp_path):
    grib = tmp_path / "basic-001_185001.01_echam"
    grib.write_bytes(b"GRIB....")
    (tmp_path / (grib.name + ".codes")).write_text(
        "130 1 st 0. 1. surface temperature [K]\n"
    )
    assert is_echam_file(grib) is True


def test_is_echam_file_by_filename_pattern():
    # Filename pattern detection doesn't require the file to exist
    assert is_echam_file(Path("expid_185001.01_echam")) is True
    assert is_echam_file(Path("random.nc")) is False


def test_is_echam_file_rejects_plain_nc(tmp_path):
    nc = tmp_path / "data.nc"
    nc.write_bytes(b"\x89HDF")
    assert is_echam_file(nc) is False
