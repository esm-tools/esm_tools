"""Contacts STAC extension: PI/author contacts on a STAC Collection.

Contacts describe the experiment, so they attach to the Collection. An Item
reaches them through its ``rel="collection"`` link.

The contacts schema is hosted upstream (stac-extensions.github.io), so there is
no local copy to validate against — this extension only registers its URL.
"""

from __future__ import annotations

import pystac

from esm_catalog.models import Contact
from esm_catalog.plugins import hookimpl
from esm_catalog.registry import EXTENSION_URLS, Extension
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


def contact_from_stac(entry: dict) -> Contact:
    """Read one STAC ``contacts`` entry into a :class:`Contact` (reverse of ``to_stac``)."""
    orcid = entry.get("identifier")
    if orcid and str(orcid).startswith("https://orcid.org/"):
        orcid = str(orcid)[len("https://orcid.org/") :]
    return Contact(
        name=entry.get("name"),
        institution=entry.get("organization"),
        orcid=orcid,
        emails=[e["value"] for e in entry.get("emails") or [] if isinstance(e, dict) and e.get("value")],
        roles=list(entry.get("roles") or ["author"]),
    )


def set_collection_contacts(collection: dict, contacts: list[Contact]) -> None:
    """Replace the contacts (and list the extension) of a Collection JSON dict."""
    if contacts:
        collection["contacts"] = [contact.to_stac() for contact in contacts]
        url = EXTENSION_URLS[Extension.contacts]
        extensions = collection.setdefault("stac_extensions", [])
        if url not in extensions:
            extensions.append(url)
    else:
        collection.pop("contacts", None)


@hookimpl
def apply_to_collection(collection, exp_metadata, hints) -> None:
    """Register this extension for the collection contract (no item-level counterpart)."""
    add_contacts_collection_extension(collection, exp_metadata.contacts)
