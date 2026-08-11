"""Extension registry — the single source of truth for STAC extensions.

``Extension`` is a StrEnum of every extension esm_catalog can attach; EXTENSION_URLS
is keyed by it, so a schema URL can never reference a name the type system does
not know, and every call site is checked against the enum rather than a bare str.
"""

from __future__ import annotations

from enum import auto

from esm_catalog._compat import StrEnum


class Extension(StrEnum):
    """A STAC extension esm_catalog knows how to attach to Items/Collections.

    Members use auto(), which StrEnum resolves to the (lowercase) member name —
    so the value is the name, never a repeated literal.
    """

    datacube = auto()
    contacts = auto()
    paleo = auto()
    namelist = auto()


EXTENSION_URLS: dict[Extension, str] = {
    # Upstream stac-extensions (remote schema, no local copy to validate against)
    Extension.datacube: "https://stac-extensions.github.io/datacube/v2.2.0/schema.json",
    Extension.contacts: "https://stac-extensions.github.io/contacts/v0.1.1/schema.json",
    # Custom ESM-Tools extensions — https://esm-tools.github.io/stac-extensions/
    Extension.paleo: "https://esm-tools.github.io/stac-extensions/paleo/v1.0.0/schema.json",
    Extension.namelist: "https://esm-tools.github.io/stac-extensions/namelist/v1.0.0/schema.json",
}
