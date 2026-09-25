"""ECHAM's path-facet extractor (esm_catalog.scan.path_facets.echam).

Confirmed live against real production filenames
(``historical_c14_init_185001.01_echam``, and confirmed the declared
``outdata_targets`` can all be stale -- the real files are discovered by
walking the filesystem, with no stream identity known ahead of time).
"""

from __future__ import annotations

from datetime import datetime

from upath import UPath

from esm_catalog.scan.path_facets import get_path_facet_plugin_manager
from esm_catalog.scan.path_facets.echam import extract_path_facets


def _extract(path: str, component: str, stream):
    return extract_path_facets(UPath(path), component, stream)


def test_matches_the_real_production_filename_stream_unknown():
    # A walked file: caller has no stream identity yet.
    result = _extract("/work/.../historical_c14_init_185001.01_echam", "echam", None)
    assert result == ("echam", datetime(1850, 1, 1))


def test_matches_a_different_real_stream():
    result = _extract("/x/historical_c14_init_185001.01_co2", "echam", None)
    assert result == ("co2", datetime(1850, 1, 1))


def test_expid_containing_underscores_does_not_confuse_the_match():
    result = _extract("/x/historical_c14_init_200002.01_echam", "echam", None)
    assert result == ("echam", datetime(2000, 2, 1))


def test_confirms_an_already_known_stream():
    result = _extract("/x/historical_c14_init_185001.01_echam", "echam", "echam")
    assert result == ("echam", datetime(1850, 1, 1))


def test_declines_when_a_known_stream_does_not_match_the_filename():
    # A declared stream that disagrees with what the filename actually says --
    # not confident enough to claim it.
    result = _extract("/x/historical_c14_init_185001.01_co2", "echam", "echam")
    assert result is None


def test_declines_a_different_component():
    assert _extract("/x/foo_200002.01_echam", "fesom", None) is None


def test_declines_when_the_filename_does_not_match_the_convention():
    assert _extract("/x/some_other_naming.nc", "echam", None) is None


def test_registered_in_the_plugin_manager():
    pm = get_path_facet_plugin_manager()
    result = pm.hook.extract_path_facets(
        path=UPath("/x/historical_c14_init_185001.01_echam"),
        component="echam",
        stream=None,
    )
    assert result == ("echam", datetime(1850, 1, 1))
