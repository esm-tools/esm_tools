"""Unit tests for CollectionContext."""

from __future__ import annotations

from pathlib import Path

import pytest

from esm_catalog.context import CollectionContext


def test_context_minimal():
    ctx = CollectionContext(
        experiment_id="exp-alpha",
        component="echam",
        collection_id="exp-alpha",
    )
    assert ctx.experiment_id == "exp-alpha"
    assert ctx.experiment_path is None
    assert ctx.namelists_by_component == {}


def test_context_carries_prescanned_namelists():
    ctx = CollectionContext(
        experiment_id="exp-alpha",
        component="echam",
        collection_id="exp-alpha",
        experiment_path=Path("/exp/alpha"),
        namelists_by_component={"echam": {"namelist.echam": {"runctl": {"dt": 450}}}},
    )
    assert ctx.namelists_by_component["echam"]["namelist.echam"]["runctl"]["dt"] == 450


def test_production_context_requires_description():
    with pytest.raises(ValueError, match="description"):
        CollectionContext(
            experiment_id="exp-alpha",
            component="echam",
            collection_id="exp-alpha",
            production=True,
        )


def test_production_missing_both_fields_names_both():
    with pytest.raises(ValueError, match="description, data_license"):
        CollectionContext(
            experiment_id="exp-alpha",
            component="echam",
            collection_id="exp-alpha",
            production=True,
        )


def test_production_empty_string_counts_as_missing():
    with pytest.raises(ValueError, match="description"):
        CollectionContext(
            experiment_id="exp-alpha",
            component="echam",
            collection_id="exp-alpha",
            production=True,
            description="",
            data_license="CC-BY-4.0",
        )
