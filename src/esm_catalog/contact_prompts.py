"""Review and edit a collection's contacts in the terminal before publishing.

Used by ``esm-catalog server put``: before anything is sent, the contacts of
each Collection are shown, and the user may add or change them. Changes are
saved to ``catalog_contacts.yaml`` next to the collection (so the next scan
keeps them) and written into the local ``collection.json`` (so this upload
carries them).

Every question has a default, so pressing Enter is always enough:

    About to publish run01-abc to https://stac.example.org
      Contacts:
        1. Ada Lovelace (a123456) - author
    Add or change contacts? Press Enter to keep, or type y:

Only plain text prompts are used (click), no arrow-key menus.
"""

from __future__ import annotations

import json
import sys
from pathlib import Path
from typing import Optional

import click
from upath import UPath

from esm_catalog.contacts import contact_from_stac, set_collection_contacts
from esm_catalog.models import Contact

ROLES_HINT = "author, principalInvestigator, pointOfContact"
"""The roles offered in the role question; any other text is accepted too."""

CLEAR = "-"
"""Typed in a field to remove its value (at Name: the whole contact)."""

HOW_TO_ANSWER = (
    'Press Enter to continue, type a new one to replace it, or "-" to remove it. '
    "For several emails, separate them by commas."
)
"""Shown once per round of questions, so each question can stay short."""


def can_ask() -> bool:
    """Whether a person is at the terminal to answer questions."""
    return sys.stdin.isatty() and sys.stdout.isatty()


def describe(contact: Contact) -> str:
    """One line per contact, e.g. ``Ada Lovelace - author, Example Institute, ada@example.org``."""
    details = [", ".join(contact.roles)]
    details += [v for v in (contact.institution, *contact.emails, contact.orcid) if v]
    return f"{contact.name} - {', '.join(details)}"


def show(contacts: list[Contact]) -> None:
    """Print the numbered contact list (or that there is none)."""
    click.echo("  Contacts:")
    if not contacts:
        click.echo("    (none)")
    for number, contact in enumerate(contacts, start=1):
        click.echo(f"    {number}. {describe(contact)}")


def review_contacts(collection_file: Path, server_url: Optional[str]) -> None:
    """Show a collection's contacts and let the user change them.

    Parameters
    ----------
    collection_file : Path
        The local ``collection.json`` about to be published. Its directory is
        the catalog directory, where ``catalog_contacts.yaml`` is saved.
    server_url : str, optional
        Where it will be published; only shown to the user.
    """
    collection = json.loads(collection_file.read_text())
    contacts = [contact_from_stac(entry) for entry in collection.get("contacts", [])]

    target = f" to {server_url}" if server_url else ""
    click.echo(f"About to publish {collection.get('id')}{target}")
    show(contacts)

    answer = click.prompt(
        "Add or change contacts? Press Enter to skip, or type y",
        default="",
        show_default=False,
    )
    if answer.strip().lower() not in ("y", "yes"):
        return

    # Edit, show the result, and offer another round until it looks right.
    while True:
        contacts = edit_contacts(contacts)
        save(collection_file, collection, contacts)
        show(contacts)
        if not click.confirm("Change anything?", default=False):
            return


def edit_contacts(contacts: list[Contact]) -> list[Contact]:
    """One round of changing (or removing) each contact, then adding new ones."""
    click.echo(HOW_TO_ANSWER)
    edited: list[Contact] = []
    for contact in contacts:
        edited.append(ask_contact(contact))
    while click.confirm(
        "Add another contact (e.g. a principal investigator or point of contact)?",
        default=False,
    ):
        edited.append(ask_contact(None))
    return [contact for contact in edited if contact is not None]


def ask_contact(current: Optional[Contact]) -> Optional[Contact]:
    """Ask for one contact's fields; *current* values are the defaults.

    Returns None when "-" is typed as the name: the contact is removed.
    """
    name = click.prompt("  Name", default=current.name if current else None)
    if name.strip() == CLEAR:
        return None
    roles_text = click.prompt(
        f"  Role ({ROLES_HINT})",
        default=", ".join(current.roles) if current else "author",
    )
    roles = [role.strip() for role in roles_text.split(",") if role.strip()]
    return Contact(
        name=name.strip(),
        roles=roles or ["author"],
        institution=_ask_optional("Institution", current.institution if current else None),
        emails=_ask_emails(current.emails if current else []),
        orcid=_ask_optional("ORCID", current.orcid if current else None),
    )


def _ask_optional(label: str, current: Optional[str]) -> Optional[str]:
    """Ask for an optional field. Enter keeps *current* (or leaves it empty)."""
    if current:
        prompt = f"  {label} [{current}]"
    else:
        prompt = f"  {label} (optional)"
    answer = click.prompt(prompt, default="", show_default=False).strip()
    if answer == CLEAR:
        return None
    return answer or current


def _ask_emails(current: list[str]) -> list[str]:
    """Ask for zero, one or several email addresses, separated by commas."""
    answer = _ask_optional("Email", ", ".join(current) or None)
    return [email.strip() for email in (answer or "").split(",") if email.strip()]


def save(collection_file: Path, collection: dict, contacts: list[Contact]) -> None:
    """Write *contacts* to ``catalog_contacts.yaml`` and the local collection."""
    from esm_catalog.scan.sourcing import write_contacts_file

    contacts_file = write_contacts_file(UPath(collection_file.parent), contacts)
    set_collection_contacts(collection, contacts)
    collection_file.write_text(json.dumps(collection))
    click.echo(f"Saved contacts to {contacts_file}")
