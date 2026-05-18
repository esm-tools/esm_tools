"""Contacts STAC extension: authors, ORCID, institution."""

from esm_catalog.stac.extensions.registry import EXTENSION_URLS


def add_contacts_extension(item: dict, config: dict | None = None) -> dict:
    """Inject contacts extension fields into *item* from ESM-Tools *config*.

    Looks for these keys in config["general"]:
        pi_name, pi_orcid, pi_institution

    Does nothing if config is None or no PI information is present.
    """
    if not config:
        return item

    general = config.get("general", {})
    pi_name = general.get("pi_name") or general.get("PI_name")
    pi_orcid = general.get("pi_orcid") or general.get("PI_orcid")
    pi_institution = general.get("pi_institution") or general.get("PI_institution")

    if not pi_name:
        return item

    contact: dict = {"name": pi_name, "roles": ["principal_investigator"]}
    if pi_orcid:
        contact["identifier"] = {
            "scheme": "orcid",
            "identifier": pi_orcid,
        }
    if pi_institution:
        contact["organization"] = pi_institution

    item["properties"].setdefault("contacts", []).append(contact)

    url = EXTENSION_URLS["contacts"]
    if url not in item["stac_extensions"]:
        item["stac_extensions"].append(url)

    return item
