"""Tests for the contacts STAC extension and Contact dataclass."""

from __future__ import annotations

import pytest

from esm_catalog.contacts import add_contacts
from esm_catalog.context import CollectionContext, Contact


def _item() -> dict:
    return {"properties": {}, "stac_extensions": []}


def _ctx(*contacts: Contact) -> CollectionContext:
    return CollectionContext(
        experiment_id="exp", component="echam", collection_id="exp",
        contacts=list(contacts),
    )


# --- Contact dataclass ---

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


# --- add_contacts ---

def test_add_contacts_noop_without_contacts():
    item = _item()
    add_contacts(item, _ctx())
    assert "contacts" not in item["properties"]
    assert item["stac_extensions"] == []


def test_add_contacts_full():
    item = _item()
    add_contacts(item, _ctx(Contact(
        name="Jane Doe", orcid="0000-0001-2345-6789", institution="AWI"
    )))
    contacts = item["properties"]["contacts"]
    assert len(contacts) == 1
    assert contacts[0]["name"] == "Jane Doe"
    assert contacts[0]["identifier"] == {"scheme": "orcid", "identifier": "0000-0001-2345-6789"}
    assert contacts[0]["organization"] == "AWI"


def test_add_contacts_orcid_optional():
    item = _item()
    add_contacts(item, _ctx(Contact(name="Jane Doe", institution="AWI")))
    assert "identifier" not in item["properties"]["contacts"][0]


def test_add_contacts_multiple():
    item = _item()
    add_contacts(item, _ctx(
        Contact(name="Jane Doe", institution="AWI"),
        Contact(name="John Smith", institution="DKRZ"),
    ))
    assert len(item["properties"]["contacts"]) == 2


def test_add_contacts_registers_extension_url():
    item = _item()
    add_contacts(item, _ctx(Contact(name="Jane", institution="AWI")))
    assert any("contacts" in url for url in item["stac_extensions"])


# --- Production validation ---

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
