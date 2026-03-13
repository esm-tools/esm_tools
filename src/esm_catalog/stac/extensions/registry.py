"""Extension URL registry — canonical schema URLs for all STAC extensions used."""

EXTENSION_URLS: dict[str, str] = {
    "datacube": "https://stac-extensions.github.io/datacube/v2.2.0/schema.json",
    "cf": "https://stac-extensions.github.io/cf/v0.2.0/schema.json",
    "file": "https://stac-extensions.github.io/file/v2.1.0/schema.json",
    "contacts": "https://stac-extensions.github.io/contacts/v0.1.1/schema.json",
    "scientific": "https://stac-extensions.github.io/scientific/v1.0.0/schema.json",
    # Custom extension — spec document TBD in hpc/hpc_spec.md
    "hpc": "https://esm-tools.github.io/stac-hpc-extension/v0.1.0/schema.json",
}
