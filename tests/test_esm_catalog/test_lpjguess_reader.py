"""LPJ-GUESS's own plain-text table format
(esm_catalog.scan.model_plugins.readers.lpj_guess).

Column layouts below are copied from a real AWI-ESM3 run's
``outdata/lpj_guess/18500101-18501231/`` directory -- confirmed live via ssh
that the un-suffixed and ``_monthly`` files are annual too (one row per
gridcell-year), not one row per month, despite some carrying misleadingly
month-like names.
"""

from __future__ import annotations

from upath import UPath

from esm_catalog.scan.format import FileFormat, UnknownFormatError, detect
from esm_catalog.scan.model_plugins.readers.lpj_guess import (
    LPJGuessReader,
    claim_by_suffix,
)
from esm_catalog.scan.reader import reader_for

_YEARLY = """\
          Lon          Lat  Year     Total
   -75.000000    82.751724  1850   18.4415
   -37.058838    78.077003  1850    0.0000
"""

_MONTHLY = """\
          Lon          Lat  Year       Jan       Feb       Mar       Apr       May       Jun       Jul       Aug       Sep       Oct       Nov       Dec
   -75.000000    82.751724  1850   18.4415   18.4415   18.4415   18.4415   18.4415   18.4415   18.4415   18.4415   18.4415   18.4415   18.4415   18.4415
   -37.058838    78.077003  1850    0.0000    0.0000    0.0000    0.0000    0.0000    0.0000    0.0000    0.0000    0.0000    0.0000    0.0000    0.0000
"""

_BARE = """\
          Lon          Lat  Year      BNE     BINE      TeBS
   -75.000000    82.751724  1850    0.000    0.000    0.000
   -37.058838    78.077003  1850    0.000    0.000    0.000
"""


def test_claim_by_suffix_scoped_to_lpj_guess_component():
    assert claim_by_suffix(".out", "lpj_guess") == FileFormat.lpjguess_table


def test_claim_by_suffix_rejects_other_components():
    assert claim_by_suffix(".out", "fesom") is None
    assert claim_by_suffix(".out", None) is None


def test_claim_by_suffix_rejects_other_suffixes():
    assert claim_by_suffix(".nc", "lpj_guess") is None


def test_detect_dispatches_through_pluggy_with_component(tmp_path):
    path = UPath(tmp_path / "cmass.out")
    path.write_text(_BARE)

    assert detect(path, "lpj_guess") == FileFormat.lpjguess_table


def test_detect_rejects_without_the_right_component(tmp_path):
    path = UPath(tmp_path / "cmass.out")
    path.write_text(_BARE)

    for component in (None, "fesom"):
        try:
            detect(path, component)
        except UnknownFormatError:
            continue
        raise AssertionError(f"expected UnknownFormatError for component={component!r}")


def test_reader_for_resolves_to_lpjguess_reader():
    assert isinstance(reader_for(FileFormat.lpjguess_table), LPJGuessReader)


def test_yearly_file_is_one_row_per_gridcell_year(tmp_path):
    path = UPath(tmp_path / "cLand_yearly.out")
    path.write_text(_YEARLY)

    meta = LPJGuessReader().read(path)

    assert meta.variable == "cLand"
    assert meta.frequency == "yr"
    assert [v.name for v in meta.variables] == ["Total"]
    assert meta.datetime_start.year == 1850
    assert meta.datetime_end.year == 1850
    assert meta.bbox == [-75.0, 78.077003, -37.058838, 82.751724]


def test_monthly_file_has_twelve_columns_but_is_still_annual(tmp_path):
    path = UPath(tmp_path / "cLand_monthly.out")
    path.write_text(_MONTHLY)

    meta = LPJGuessReader().read(path)

    assert meta.variable == "cLand"
    assert meta.frequency == "mon"
    assert [v.name for v in meta.variables] == [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    ]
    assert meta.datetime_start.year == 1850
    assert meta.datetime_end.year == 1850


def test_bare_file_is_annual_with_breakdown_columns(tmp_path):
    path = UPath(tmp_path / "cmass.out")
    path.write_text(_BARE)

    meta = LPJGuessReader().read(path)

    assert meta.variable == "cmass"
    assert meta.frequency == "yr"
    assert [v.name for v in meta.variables] == ["BNE", "BINE", "TeBS"]
