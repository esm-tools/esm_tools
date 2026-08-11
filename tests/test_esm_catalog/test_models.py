"""Unit tests for the ExperimentMetadata and Contact models."""

from __future__ import annotations

from pathlib import Path

import pytest
from pydantic import ValidationError

from esm_catalog.models import Contact, ExperimentMetadata


def test_exp_metadata_minimal():
    exp_metadata = ExperimentMetadata(
        experiment_id="exp-alpha",
        experiment_path=Path("/exp/alpha"),
    )
    assert exp_metadata.experiment_id == "exp-alpha"
    assert exp_metadata.experiment_path == Path("/exp/alpha")
    assert exp_metadata.namelists_by_component == {}
    assert exp_metadata.components == []


def test_exp_metadata_requires_a_path():
    """An experiment always has a location; the path is required."""
    with pytest.raises(ValidationError):
        ExperimentMetadata(experiment_id="exp-alpha")


def test_collection_id_is_name_plus_path_hash():
    """The Collection id is the (reusable) name plus a path hash, so two
    experiments sharing a name get distinct, stable ids."""
    a = ExperimentMetadata(experiment_id="PI_ctrl", experiment_path=Path("/runs/a"))
    b = ExperimentMetadata(experiment_id="PI_ctrl", experiment_path=Path("/runs/b"))
    again = ExperimentMetadata(experiment_id="PI_ctrl", experiment_path=Path("/runs/a"))

    assert a.collection_id.startswith("PI_ctrl-")
    assert a.collection_id != b.collection_id  # same name, different path
    assert a.collection_id == again.collection_id  # stable for the same path


def test_exp_metadata_carries_prescanned_namelists():
    exp_metadata = ExperimentMetadata(
        experiment_id="exp-alpha",
        experiment_path=Path("/exp/alpha"),
        namelists_by_component={"echam": {"namelist.echam": {"runctl": {"dt": 450}}}},
    )
    assert (
        exp_metadata.namelists_by_component["echam"]["namelist.echam"]["runctl"]["dt"]
        == 450
    )


# --- Contact ---


def test_contact_defaults():
    c = Contact.model_validate({"name": "Jane Doe"})
    assert c.institution is None
    assert c.roles == ["principal_investigator"]


def test_contact_to_stac_carries_default_roles():
    # roles has no alias and is not dropped by exclude_none, so the default
    # principal_investigator must survive serialization to the STAC entry.
    stac = Contact(name="Jane", institution="AWI").to_stac()
    assert stac["roles"] == ["principal_investigator"]


def test_contact_to_stac_carries_custom_roles():
    stac = Contact(
        name="Jane", institution="AWI", roles=["author", "processor"]
    ).to_stac()
    assert stac["roles"] == ["author", "processor"]


def test_contact_to_stac_aliases_institution_to_organization():
    stac = Contact(name="Jane", institution="AWI").to_stac()
    assert stac["organization"] == "AWI"


def test_contact_to_stac_expands_bare_orcid_to_url():
    stac = Contact(name="Jane", orcid="0000-0001-2345-6789").to_stac()
    assert stac["identifier"] == "https://orcid.org/0000-0001-2345-6789"


def test_contact_to_stac_passes_through_full_orcid_url():
    stac = Contact(name="Jane", orcid="https://orcid.org/0000-0001-2345-6789").to_stac()
    assert stac["identifier"] == "https://orcid.org/0000-0001-2345-6789"


def test_contact_to_stac_omits_absent_orcid():
    stac = Contact(name="Jane", institution="AWI").to_stac()
    assert "identifier" not in stac
