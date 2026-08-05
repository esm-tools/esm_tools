"""Unit tests for the ExperimentMetadata and Contact models."""

from __future__ import annotations

from pathlib import Path

from esm_catalog.models import Contact, ExperimentMetadata


def test_exp_metadata_minimal():
    exp_metadata = ExperimentMetadata(
        experiment_id="exp-alpha",
    )
    assert exp_metadata.experiment_id == "exp-alpha"
    assert exp_metadata.experiment_path is None
    assert exp_metadata.namelists_by_component == {}


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
