"""Datacube STAC extension: cube:dimensions and cube:variables.

Expected metadata shapes (the contract the file scanners must meet):

    metadata["dimensions"]: dict mapping dimension name to a STAC datacube
        Dimension Object, e.g.
        {"time": {"type": "temporal", "extent": ["2000-01-01T00:00:00Z",
                                                 "2000-12-31T00:00:00Z"]},
         "lon": {"type": "spatial", "axis": "x", "extent": [-180.0, 180.0]}}

    metadata["variables"]: list of dicts with keys
        name (str, required — entries without it are skipped),
        dimensions (list[str]), units (str), and any of
        description / long_name / standard_name (str).

The v2.2.0 schema requires cube:dimensions whenever the extension URL is
declared, so the extension is a no-op unless dimensions are present.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from esm_catalog.registry import EXTENSION_URLS
from esm_catalog.stac_ext import register_extension

if TYPE_CHECKING:
    import pystac


def add_datacube_extension(item: "pystac.Item", metadata: dict) -> None:
    """Inject datacube extension fields into *item* from *metadata*.

    Sets item.properties["cube:dimensions"] (pass-through) and
    item.properties["cube:variables"] (mapped), and appends the datacube
    schema URL to item.stac_extensions (idempotent). No-op when *metadata*
    carries no dimensions.
    """
    dims = metadata.get("dimensions", {})
    if not dims:
        return

    item.properties["cube:dimensions"] = dims

    cube_vars = _to_cube_variables(metadata.get("variables", []))
    if cube_vars:
        item.properties["cube:variables"] = cube_vars

    register_extension(item, EXTENSION_URLS["datacube"])


def _to_cube_variables(variables: list) -> dict:
    """Map scanner variable entries to datacube Variable Objects."""
    cube_vars = {}
    for v in variables:
        name = v.get("name")
        if not name:
            continue
        entry: dict = {"dimensions": v.get("dimensions", [])}
        if "units" in v:
            entry["unit"] = v["units"]
        for key in ("description", "long_name", "standard_name"):
            if key in v:
                entry["description"] = v[key]
                break
        cube_vars[name] = entry
    return cube_vars
