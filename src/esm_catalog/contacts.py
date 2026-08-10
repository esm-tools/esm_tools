"""Contacts STAC extension: PI/author contacts on a STAC Collection.

Contacts describe the experiment, so they attach to the Collection. An Item
reaches them through its ``rel="collection"`` link.

The contacts schema is hosted upstream (stac-extensions.github.io), so there is
no local copy to validate against — this extension only registers its URL.
"""

from __future__ import annotations

import pystac

from esm_catalog.models import Contact
from esm_catalog.registry import Extension
from esm_catalog.stac_ext import apply_extension


def add_contacts_collection_extension(
    collection: pystac.Collection, contacts: list[Contact]
) -> None:
    """Inject the contacts extension URL and contacts into *collection*.

    PI/author contacts describe the experiment, so they attach to the Collection.
    No-op when *contacts* is empty. A Collection has no ``properties``, so the
    contacts live in ``extra_fields``.

    Parameters
    ----------
    collection : pystac.Collection
        The collection to annotate in place.
    contacts : list of Contact
        The experiment's PI/author contacts.
    """
    if not contacts:
        return
    collection.extra_fields["contacts"] = [contact.to_stac() for contact in contacts]
    # remote schema (upstream stac-extensions) — nothing local to validate against
    apply_extension(collection, Extension.contacts, validate=False)
