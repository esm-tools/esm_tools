"""The generic, declarative path-facet template engine
(esm_catalog.scan.path_facets.templates), driven by templates.yaml.

Confirmed live against real production filenames -- ECHAM's
``<expid>_YYYYMM.DD_<stream>``, JSBACH's identical convention, and FESOM's
``<stream>.fesom.<year>.nc``. One engine, no per-component Python code; only
the shipped YAML table differs per component.
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
        None,
    )


def test_matches_a_different_real_echam_stream():
    assert _extract("/x/historical_c14_init_185001.01_co2", "echam") == (
        "co2",
        datetime(1850, 1, 1),
        None,
    )


def test_echam_expid_with_underscores_does_not_confuse_the_match():
    assert _extract("/x/historical_c14_init_200002.01_echam", "echam") == (
        "echam",
        datetime(2000, 2, 1),
        None,
    )


def test_matches_real_fesom_outdata_filename():
    assert _extract("/x/a_ice.fesom.1850.nc", "fesom") == (
        "a_ice",
        datetime(1850, 1, 1),
        None,
    )


def test_matches_real_awicm3_fesom_outdata_filename():
    # Confirmed live against a real AWI-CM3 v3.2 run -- a different filename
    # shape from the plain year-stamped one above; previously fell through
    # to a full read for every FESOM file on that run.
    assert _extract("/x/a_ice.fesom.185001.01.nc", "fesom") == (
        "a_ice",
        datetime(1850, 1, 1),
        None,
    )


def test_fesom_stream_containing_an_underscore_is_not_split():
    # a_ice has an underscore in it -- the stream field must not exclude '_'.
    assert _extract("/x/a_ice.fesom.1850.nc", "fesom") == (
        "a_ice",
        datetime(1850, 1, 1),
        None,
    )


def test_matches_real_jsbach_outdata_filename():
    # jsbach couples through ECHAM's own GRIB convention.
    assert _extract("/x/historical_c14_init_185001.01_yasso", "jsbach") == (
        "yasso",
        datetime(1850, 1, 1),
        None,
    )


def test_matches_real_oifs_xios_filename_with_explicit_end():
    # Confirmed live against a real AWI-CM3 (develop-is) run: OIFS/XIOS
    # bundles a whole year of steps per file, so the end is the filename's
    # own second year -- not something frequency+start could reconstruct.
    assert _extract("/x/atm_1d_10u_1d_1900-1900.nc", "oifs") == (
        "10u",
        datetime(1900, 1, 1),
        datetime(1900, 1, 1),
    )


def test_oifs_stream_containing_an_underscore_is_not_split():
    assert _extract("/x/atm_remapped_1m_lai_hv_1m_1900-1900.nc", "oifs") == (
        "lai_hv",
        datetime(1900, 1, 1),
        datetime(1900, 1, 1),
    )


def test_oifs_variable_name_shaped_like_a_frequency_code_is_not_confused():
    # "10u" ends in 'u', not a frequency unit (h/d/m/y) -- {{ ignore }} must
    # not swallow it, even though it shares the digit-run-then-letter shape.
    assert _extract("/x/atm_1d_10u_1d_1900-1900.nc", "oifs")[0] == "10u"


def test_matches_real_oifs_pressure_level_filename():
    # The "pl" marker brackets the variable name the same way the frequency
    # code does for 2D fields; tried first so a 2D template cannot swallow
    # "pl" into the stream name.
    assert _extract("/x/atm_remapped_1m_pl_q_1m_pl_1900-1900.nc", "oifs") == (
        "q",
        datetime(1900, 1, 1),
        datetime(1900, 1, 1),
    )
    assert _extract("/x/atm_remapped_6h_pl_vo_6h_pl_1901-1901.nc", "oifs") == (
        "vo",
        datetime(1901, 1, 1),
        datetime(1901, 1, 1),
    )


def test_restart_filenames_are_deliberately_uncovered():
    """Every restart OutputFile -- declared or walked -- carries a fixed,
    component-wide stream="restart" (sourcing.py's restart_files()); no
    path-facet template can ever resolve to that literal string, so restart
    files always fall back to a real read, on purpose. Confirmed live:
    FESOM's restart "files" are actually directories of one-variable-per-file
    fragments (area.nc, hice.nc, cfc11.nc, ...) -- there is no single stable
    schema to freeze across them even if a template did somehow match."""
    assert _extract("/x/fesom.1850.oce.restart", "fesom", "restart") is None
    assert _extract("/x/fesom.1850.ice.restart", "fesom", "restart") is None
    assert (
        _extract("/x/restart_historical_c14_init_accw_18501231.nc", "echam", "restart")
        is None
    )
    assert (
        _extract(
            "/x/restart_historical_c14_init_18501231_yasso.nc", "jsbach", "restart"
        )
        is None
    )
    assert (
        _extract(
            "/x/restart_historical_c14_init_18501231_hdrestart.nc",
            "hdmodel",
            "restart",
        )
        is None
    )


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
        None,
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
    assert result == ("a_ice", datetime(1850, 1, 1), None)


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
