"""Unit tests for CollectionContext and Contact."""

from __future__ import annotations

from pathlib import Path

import pytest

from esm_catalog.context import CollectionContext, Contact


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


# --- Contact ---

def test_contact_from_dict_full():
    c = Contact.from_dict({
        "name": "Jane Doe",
        "orcid": "0000-0001-2345-6789",
        "institution": "AWI",
        "roles": ["principal_investigator"],
    })
    assert c.name == "Jane Doe"
    assert c.orcid == "0000-0001-2345-6789"
    assert c.institution == "AWI"


def test_contact_from_dict_minimal():
    c = Contact.from_dict({"name": "Jane Doe"})
    assert c.institution is None
    assert c.roles == ["principal_investigator"]


def test_contact_validate_passes_with_institution():
    Contact(name="Jane", institution="AWI").validate()


def test_contact_validate_fails_without_institution():
    with pytest.raises(ValueError, match="institution"):
        Contact(name="Jane").validate()


def test_production_context_requires_contact():
    with pytest.raises(ValueError, match="contact"):
        CollectionContext(
            experiment_id="exp", component="echam", collection_id="exp",
            production=True, description="desc", data_license="CC-BY-4.0",
        )


def test_production_context_requires_contact_with_institution():
    with pytest.raises(ValueError, match="institution"):
        CollectionContext(
            experiment_id="exp", component="echam", collection_id="exp",
            production=True, description="desc", data_license="CC-BY-4.0",
            contacts=[Contact(name="Jane")],
        )


def test_production_context_passes_with_valid_contact():
    ctx = CollectionContext(
        experiment_id="exp", component="echam", collection_id="exp",
        production=True, description="desc", data_license="CC-BY-4.0",
        contacts=[Contact(name="Jane", institution="AWI")],
    )
    assert ctx.production is True
