"""Contacts STAC extension: PI/author contacts on a STAC Item.

The contacts schema is hosted upstream (stac-extensions.github.io), so there is
no local copy to validate against — this extension only registers its URL.
"""

from __future__ import annotations

import pystac

from esm_catalog.models import Contact
from esm_catalog.registry import Extension
from esm_catalog.stac_ext import apply_extension


def add_contacts_item_extension(item: pystac.Item, contacts: list[Contact]) -> None:
    """Inject the contacts extension URL and properties into *item*.

    No-op when *contacts* is empty. Takes the contacts slice directly (like the
    other item extensions) rather than the whole experiment, so it need not
    depend on the ExperimentMetadata model.

    Parameters
    ----------
    item : pystac.Item
        The item to annotate in place.
    contacts : list of Contact
        The experiment's PI/author contacts.
    """
    if not contacts:
        return
    item.properties["contacts"] = [contact.to_stac() for contact in contacts]
    # remote schema (upstream stac-extensions) — nothing local to validate against
    apply_extension(item, Extension.contacts, validate=False)
