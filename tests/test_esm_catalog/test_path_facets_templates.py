"""The generic, declarative path-facet template engine
(esm_catalog.scan.path_facets.templates), driven by templates.yaml.

Confirmed live against real production filenames -- ECHAM's
``<expid>_YYYYMM.DD_<stream>`` and FESOM's ``<stream>.fesom.<year>.nc`` /
``fesom.<year>.<stream>.restart``. One engine, no per-component Python code;
only the shipped YAML table differs per component.
"""

from __future__ import annotations

from datetime import datetime

import pytest
from upath import UPath

from esm_catalog.scan.path_facets import get_path_facet_plugin_manager
from esm_catalog.scan.path_facets.templates import (
    _compile,
    _date_field_regex,
    extract_path_facets,
)


def _extract(path: str, component: str, stream=None):
    return extract_path_facets(UPath(path), component, stream)


# --- real, confirmed-live conventions, via the shipped templates.yaml ---


def test_matches_real_echam_filename():
    assert _extract("/x/historical_c14_init_185001.01_echam", "echam") == (
        "echam",
        datetime(1850, 1, 1),
    )


def test_matches_a_different_real_echam_stream():
    assert _extract("/x/historical_c14_init_185001.01_co2", "echam") == (
        "co2",
        datetime(1850, 1, 1),
    )


def test_echam_expid_with_underscores_does_not_confuse_the_match():
    assert _extract("/x/historical_c14_init_200002.01_echam", "echam") == (
        "echam",
        datetime(2000, 2, 1),
    )


def test_matches_real_fesom_outdata_filename():
    assert _extract("/x/a_ice.fesom.1850.nc", "fesom") == (
        "a_ice",
        datetime(1850, 1, 1),
    )


def test_fesom_stream_containing_an_underscore_is_not_split():
    # a_ice has an underscore in it -- the stream field must not exclude '_'.
    assert _extract("/x/a_ice.fesom.1850.nc", "fesom") == (
        "a_ice",
        datetime(1850, 1, 1),
    )


def test_matches_real_fesom_restart_filenames():
    assert _extract("/x/fesom.1850.oce.restart", "fesom") == (
        "oce",
        datetime(1850, 1, 1),
    )
    assert _extract("/x/fesom.1850.ice.restart", "fesom") == (
        "ice",
        datetime(1850, 1, 1),
    )


def test_matches_real_echam_restart_filename():
    # Stream-then-date order -- the opposite of ECHAM's outdata naming above.
    assert _extract("/x/restart_historical_c14_init_accw_18501231.nc", "echam") == (
        "accw",
        datetime(1850, 12, 31),
    )


def test_matches_real_jsbach_outdata_filename():
    # jsbach couples through ECHAM's own GRIB convention.
    assert _extract("/x/historical_c14_init_185001.01_yasso", "jsbach") == (
        "yasso",
        datetime(1850, 1, 1),
    )


def test_matches_real_jsbach_restart_filename():
    # Date-then-stream order -- the opposite of ECHAM's restart naming.
    result = _extract("/x/restart_historical_c14_init_18501231_yasso.nc", "jsbach")
    assert result == ("yasso", datetime(1850, 12, 31))


def test_matches_real_hdmodel_restart_filename():
    # Same date-then-stream order as jsbach's restart, not ECHAM's.
    result = _extract("/x/restart_historical_c14_init_18501231_hdrestart.nc", "hdmodel")
    assert result == ("hdrestart", datetime(1850, 12, 31))


def test_oasis3mct_date_range_stamps_are_deliberately_uncovered():
    # A run-segment date *range* (not a single timestamp) and an undated
    # bare filename -- neither fits this template model; both must decline
    # cleanly rather than guess.
    assert _extract("/x/a2o_flux_18500101-18501231", "oasis3mct") is None
    assert _extract("/x/a2o_flux", "oasis3mct") is None


def test_confirms_an_already_known_stream():
    assert _extract("/x/historical_c14_init_185001.01_echam", "echam", "echam") == (
        "echam",
        datetime(1850, 1, 1),
    )


def test_declines_when_a_known_stream_does_not_match_the_filename():
    assert _extract("/x/historical_c14_init_185001.01_co2", "echam", "echam") is None


def test_declines_a_component_with_no_templates():
    assert _extract("/x/whatever_185001.nc", "hdmodel") is None


def test_declines_when_no_template_matches():
    assert _extract("/x/some_other_naming_entirely.nc", "echam") is None


def test_registered_in_the_plugin_manager():
    pm = get_path_facet_plugin_manager()
    result = pm.hook.extract_path_facets(
        path=UPath("/x/a_ice.fesom.1850.nc"), component="fesom", stream=None
    )
    assert result == ("a_ice", datetime(1850, 1, 1))


# --- the generic compiler itself, independent of the shipped table ---


def test_date_field_regex_converts_known_strftime_codes():
    assert _date_field_regex("%Y%m.%d") == r"\d{4}\d{2}\.\d{2}"


def test_compile_rejects_an_unknown_placeholder():
    with pytest.raises(ValueError, match="unknown placeholder"):
        _compile("x", "{{ bogus }}", "%Y")


def test_compile_handles_a_stream_prefixed_template():
    template = _compile("x", "{{ stream }}_v{{ date }}.nc", "%Y%m")
    match = template.regex.match("temperature_v200001.nc")
    assert match is not None
    assert match.group("stream") == "temperature"
    assert match.group("date") == "200001"
