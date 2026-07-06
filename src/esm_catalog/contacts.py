"""Contacts STAC extension: PI and author information."""

from __future__ import annotations

from typing import TYPE_CHECKING

from esm_catalog.registry import EXTENSION_URLS

if TYPE_CHECKING:
    from esm_catalog.context import CollectionContext


def add_contacts(item: dict, ctx: "CollectionContext") -> None:
    """Inject contacts extension fields into *item* from *ctx*.

    Reads ``ctx.contacts`` (a list of :class:`~esm_catalog.context.Contact`)
    and maps each to the STAC contacts format::

        properties:
            contacts:
                - name: "Jane Doe"
                  roles: ["principal_investigator"]
                  identifier: {scheme: "orcid", identifier: "0000-0001-2345-6789"}
                  organization: "AWI"

    Does nothing if ``ctx.contacts`` is empty.
    Appends to any existing ``contacts`` list in the item properties.
    """
    if not ctx.contacts:
        return

    stac_contacts = []
    for contact in ctx.contacts:
        entry: dict = {"name": contact.name, "roles": contact.roles}
        if contact.orcid:
            entry["identifier"] = {"scheme": "orcid", "identifier": contact.orcid}
        if contact.institution:
            entry["organization"] = contact.institution
        stac_contacts.append(entry)

    item["properties"].setdefault("contacts", []).extend(stac_contacts)

    url = EXTENSION_URLS["contacts"]
    if url not in item.get("stac_extensions", []):
        item.setdefault("stac_extensions", []).append(url)
