from __future__ import annotations
"""Datacube STAC extension: cube:dimensions and cube:variables."""

from esm_catalog.stac.extensions.registry import EXTENSION_URLS


def add_datacube_extension(item: dict, metadata: dict) -> dict:
    """Inject datacube extension fields into *item* from *metadata*.

    Populates:
        item["properties"]["cube:dimensions"]
        item["properties"]["cube:variables"]

    Appends the datacube schema URL to item["stac_extensions"].
    """
    dims = metadata.get("dimensions", {})
    variables = metadata.get("variables", [])

    if not dims and not variables:
        return item

    if dims:
        item["properties"]["cube:dimensions"] = dims

    if variables:
        cube_vars = {}
        for v in variables:
            name = v["name"]
            entry: dict = {"dimensions": v.get("dimensions", [])}
            if "standard_name" in v:
                entry["description"] = v["standard_name"]
            if "units" in v:
                entry["unit"] = v["units"]
            if "long_name" in v:
                entry["description"] = v["long_name"]
            cube_vars[name] = entry
        item["properties"]["cube:variables"] = cube_vars

    url = EXTENSION_URLS["datacube"]
    if url not in item["stac_extensions"]:
        item["stac_extensions"].append(url)

    return item
