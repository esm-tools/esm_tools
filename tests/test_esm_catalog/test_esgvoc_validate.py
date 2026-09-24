"""Tests for esgvoc-backed CV validation of declared cmip6 facets.

Requires the optional ``esgvoc`` package and a locally installed CMIP6 CV
(``esgvoc use cmip6@latest``) -- skips the whole module if either is missing,
since this is a live-CV conformance check, not core logic.
"""

from __future__ import annotations

import pytest

from esm_catalog.cmip6 import Cmip6Config

esgvoc_api = pytest.importorskip("esgvoc.api")

from esm_catalog.esgvoc_validate import (  # noqa: E402
    Cmip6ValidationIssue,
    validate_cmip6_config,
)


def _cv_installed() -> bool:
    try:
        esgvoc_api.valid_term_in_collection("CMIP6", "cmip6", "mip_era")
        return True
    except Exception:  # noqa: BLE001 -- any failure means "not installed/reachable"
        return False


pytestmark = pytest.mark.skipif(
    not _cv_installed(),
    reason="CMIP6 CV not installed locally (run: esgvoc use cmip6@latest)",
)


def test_none_config_has_no_issues():
    assert validate_cmip6_config(None) == []


def test_config_declaring_nothing_has_no_issues():
    assert validate_cmip6_config(Cmip6Config()) == []


def test_fully_valid_config_has_no_issues():
    config = Cmip6Config(
        activity_id="PMIP",
        institution_id="AWI",
        source_id="AWI-CM-1-1-MR",
        experiment_id="piControl",
        mip_era="CMIP6",
    )
    assert validate_cmip6_config(config) == []


def test_unregistered_source_id_is_flagged():
    # AWI-CM3 has no registered CMIP6 source_id (its successor is still
    # going through WCRP registration) -- this must be flagged, not silently
    # accepted, exactly the correctness property this validator exists for.
    config = Cmip6Config(source_id="AWI-CM-3")
    issues = validate_cmip6_config(config)
    assert issues == [
        Cmip6ValidationIssue(field="source_id", value="AWI-CM-3", collection="source_id")
    ]


def test_lowercase_mip_era_is_flagged():
    # esgvoc's own CV term id for this round is lowercase "cmip6", but the
    # real DRS/STAC value is "CMIP6" -- a lowercase declaration must be
    # flagged, not silently normalized away.
    config = Cmip6Config(mip_era="cmip6")
    issues = validate_cmip6_config(config)
    assert issues == [
        Cmip6ValidationIssue(field="mip_era", value="cmip6", collection="mip_era")
    ]


def test_multiple_invalid_fields_all_flagged():
    config = Cmip6Config(source_id="AWI-CM-3", activity_id="NOT-A-REAL-ACTIVITY")
    issues = validate_cmip6_config(config)
    fields = {issue.field for issue in issues}
    assert fields == {"source_id", "activity_id"}


def test_mip_era_derives_the_esgvoc_project_not_hardcoded():
    # CMIP7's model-identity collection is genuinely named "source", not
    # "source_id" -- a real, registered CMIP7 source_id must not crash or be
    # falsely flagged just because our field->collection mapping is CMIP6-shaped.
    try:
        import esgvoc.api as _api

        _api.valid_term_in_collection("CMIP7", "cmip7", "mip_era")
    except Exception:  # noqa: BLE001
        pytest.skip("CMIP7 CV not installed locally (run: esgvoc use cmip7@latest)")

    config = Cmip6Config(source_id="AWI-ESM3-4-2-veg-HR", mip_era="CMIP7")
    assert validate_cmip6_config(config) == []


def test_unvalidatable_fields_are_never_flagged():
    # variant_label/source_type/further_info_url have no CV collection this
    # validator checks against -- declaring them must never raise or flag.
    config = Cmip6Config(
        variant_label="r1i1p1f1", source_type="AOGCM", further_info_url="https://x"
    )
    assert validate_cmip6_config(config) == []
