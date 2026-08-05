"""Contacts STAC extension: PI/author contacts on a STAC Item.

The contacts schema is hosted upstream (stac-extensions.github.io), so there is
no local copy to validate against — this extension only registers its URL.
"""

from __future__ import annotations

import pystac

from esm_catalog.models import ExperimentMetadata
from esm_catalog.registry import Extension
from esm_catalog.stac_ext import apply_extension


def add_contacts_item_extension(
    item: pystac.Item, exp_metadata: ExperimentMetadata
) -> None:
    """Inject the contacts extension URL and properties into *item*.

    No-op when the experiment declares no contacts.

    Parameters
    ----------
    item : pystac.Item
        The item to annotate in place.
    exp_metadata : ExperimentMetadata
        The owning experiment, source of the contacts.
    """
    if not exp_metadata.contacts:
        return
    item.properties["contacts"] = [
        contact.to_stac() for contact in exp_metadata.contacts
    ]
    # remote schema (upstream stac-extensions) — nothing local to validate against
    apply_extension(item, Extension.contacts, validate=False)
