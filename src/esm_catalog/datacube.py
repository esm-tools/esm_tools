"""Datacube STAC extension: cube:dimensions and cube:variables.

The metadata a file scanner must provide::

    file_metadata["dimensions"]: dict mapping dimension name to a STAC datacube
        Dimension Object, e.g.
        {"time": {"type": "temporal", "extent": ["2000-01-01T00:00:00Z",
                                                 "2000-12-31T00:00:00Z"]},
         "lon": {"type": "spatial", "axis": "x", "extent": [-180.0, 180.0]}}

    file_metadata["variables"]: list of dicts with keys
        name (str, required; entries without it are skipped),
        dimensions (list[str]), units (str), and any of
        description / long_name / standard_name (str).

The v2.2.0 schema requires cube:dimensions whenever the extension URL is
declared, so the extension is a no-op unless dimensions are present.
"""

from __future__ import annotations

from typing import TypedDict

import pystac

from esm_catalog.registry import Extension
from esm_catalog.stac_ext import apply_extension
from esm_catalog.types import FileMetadata, ScannedVariable, VariableName


class CubeVariable(TypedDict, total=False):
    """The ``cube:variables`` entry this extension emits for one data variable.

    A subset of the STAC datacube Variable Object — only the fields this
    extension populates.
    """

    dimensions: list[str]
    unit: str
    description: str


CubeVariables = dict[VariableName, CubeVariable]
"""Emitted ``cube:variables``: variable name -> CubeVariable."""


def add_datacube_item_extension(item: pystac.Item, file_metadata: FileMetadata) -> None:
    """Inject datacube extension fields into *item* from *file_metadata*.

    Sets ``item.properties["cube:dimensions"]`` (pass-through) and
    ``item.properties["cube:variables"]`` (mapped), and appends the datacube
    schema URL to ``item.stac_extensions`` (idempotent).

    Parameters
    ----------
    item : pystac.Item
        The item to annotate in place.
    file_metadata : FileMetadata
        The file's scanned metadata. No-op when it carries no dimensions.
    """
    dimensions = file_metadata.get("dimensions", {})
    if not dimensions:
        return

    item.properties["cube:dimensions"] = dimensions

    cube_variables = _to_cube_variables(file_metadata.get("variables", []))
    if cube_variables:
        item.properties["cube:variables"] = cube_variables

    # remote schema (upstream stac-extensions) — nothing local to validate against
    apply_extension(item, Extension.datacube, validate=False)


def _to_cube_variables(variables: list[ScannedVariable]) -> CubeVariables:
    """Map scanner variable entries to datacube Variable Objects.

    Entries without a name are skipped. The first present of ``description`` /
    ``long_name`` / ``standard_name`` becomes the ``description``.

    Parameters
    ----------
    variables : list of ScannedVariable
        Scanner variable entries.

    Returns
    -------
    CubeVariables
        Variable name -> CubeVariable.
    """
    cube_variables: CubeVariables = {}
    for variable in variables:
        name = variable.get("name")
        if not name:
            continue
        entry: CubeVariable = {"dimensions": variable.get("dimensions", [])}
        if "units" in variable:
            entry["unit"] = variable["units"]
        for description_key in ("description", "long_name", "standard_name"):
            if description_key in variable:
                entry["description"] = variable[description_key]
                break
        cube_variables[name] = entry
    return cube_variables
