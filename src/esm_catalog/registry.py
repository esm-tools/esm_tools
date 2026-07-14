"""Extension URL registry — canonical schema URLs for all STAC extensions used."""

from __future__ import annotations

EXTENSION_URLS: dict[str, str] = {
    "datacube": "https://stac-extensions.github.io/datacube/v2.2.0/schema.json",
    "cf": "https://stac-extensions.github.io/cf/v0.2.0/schema.json",
    "file": "https://stac-extensions.github.io/file/v2.1.0/schema.json",
    "contacts": "https://stac-extensions.github.io/contacts/v0.1.1/schema.json",
    "scientific": "https://stac-extensions.github.io/scientific/v1.0.0/schema.json",
    # Custom ESM-Tools extensions — https://esm-tools.github.io/stac-extensions/
    "hpc": "https://esm-tools.github.io/stac-extensions/hpc/v1.0.0/schema.json",
    "paleo": "https://esm-tools.github.io/stac-extensions/paleo/v1.0.0/schema.json",
    "namelist": "https://esm-tools.github.io/stac-extensions/namelist/v1.0.0/schema.json",
}
