"""ECHAM's path-facet extractor (esm_catalog.scan.path_facets.echam).

Confirmed live against a real production filename
(``historical_c14_init_185001.01_echam``).
"""

from __future__ import annotations

from datetime import datetime

from upath import UPath

from esm_catalog.scan.path_facets import get_path_facet_plugin_manager
from esm_catalog.scan.path_facets.echam import extract_start_datetime


def _extract(path: str, component: str, stream):
    return extract_start_datetime(UPath(path), component, stream)


def test_matches_the_real_production_filename():
    dt = _extract("/work/.../historical_c14_init_185001.01_echam", "echam", "echam")
    assert dt == datetime(1850, 1, 1)


def test_expid_containing_underscores_does_not_confuse_the_match():
    dt = _extract("/x/historical_c14_init_200002.01_echam", "echam", "echam")
    assert dt == datetime(2000, 2, 1)


def test_declines_a_different_component():
    assert _extract("/x/foo_200002.01_echam", "fesom", "echam") is None


def test_declines_when_stream_is_none():
    assert _extract("/x/foo_200002.01_echam", "echam", None) is None


def test_declines_when_the_filename_does_not_match_the_convention():
    assert _extract("/x/some_other_naming.nc", "echam", "echam") is None


def test_declines_when_stream_suffix_does_not_match():
    # a real file's stream is "co2", but we ask for "echam" -- no match.
    assert _extract("/x/historical_c14_init_185001.01_co2", "echam", "echam") is None


def test_registered_in_the_plugin_manager():
    pm = get_path_facet_plugin_manager()
    result = pm.hook.extract_start_datetime(
        path=UPath("/x/historical_c14_init_185001.01_echam"),
        component="echam",
        stream="echam",
    )
    assert result == datetime(1850, 1, 1)
