"""Namelist STAC extension: simulation parameters from Fortran namelists.

Adds namelist configuration data to STAC collections and items, allowing
users to search for simulations by their runtime parameters (timestep,
coupling settings, physics options, etc.).

Properties added at collection level (collection.extra_fields):
    nml:files        - List of namelist filenames
    nml:groups       - List of namelist groups (chapters) across all files
    nml:parameters   - Flattened key-value parameters for search

Properties added at item level (item.properties), one entry per parameter
across ALL components:
    nml:{component}:{group}:{key} - value

This allows CQL2 queries like:
    - Find simulations with delta_time > 300
    - Find all coupled simulations (lcouple = true)
    - Find simulations with specific CO2 levels
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from esm_catalog.registry import EXTENSION_URLS

if TYPE_CHECKING:
    import pystac

# Values with more elements than this are skipped when flattening for search.
_MAX_LIST_LENGTH = 10

# filename -> {group -> {key -> scalar value}}
NamelistData = dict[str, dict[str, dict[str, Any]]]


def add_namelist_extension(
    collection: "pystac.Collection", namelists: NamelistData
) -> None:
    """Inject namelist extension fields into a STAC collection.

    Args:
        collection: pystac Collection to modify in place.
        namelists: mapping filename -> {group -> {key -> value}}, e.g. the
            component's own entry from ctx.namelists_by_component.
    """
    if not namelists:
        return

    files = sorted(namelists.keys())

    groups: set = set()
    for file_data in namelists.values():
        groups.update(file_data.keys())

    parameters = _flatten_for_search(namelists)

    collection.extra_fields["nml:files"] = files
    collection.extra_fields["nml:groups"] = sorted(groups)
    collection.extra_fields["nml:parameters"] = parameters

    url = EXTENSION_URLS["namelist"]
    if url not in collection.stac_extensions:
        collection.stac_extensions.append(url)


def _flatten_for_search(namelists: NamelistData) -> dict[str, Any]:
    """Flatten namelist structure for searchable parameters.

    Creates keys in the format "group:key" for CQL2 filtering. Only includes
    scalar values (and short scalar lists) that are useful for search.
    """
    result: dict = {}

    for _filename, groups in namelists.items():
        for group_name, values in groups.items():
            for key, value in values.items():
                if not _is_searchable(value):
                    continue
                flat_key = f"{group_name}:{key}"
                # If key already exists from another file, prefer the first.
                if flat_key not in result:
                    result[flat_key] = value

    return result


def _is_searchable(value) -> bool:
    """Return whether *value* is a scalar (or short scalar list) worth indexing."""
    if value is None or isinstance(value, dict):
        return False
    if isinstance(value, list):
        if len(value) > _MAX_LIST_LENGTH:
            return False
        return all(isinstance(v, (int, float, str, bool, type(None))) for v in value)
    return True


def add_namelist_item_extension(item: "pystac.Item", ctx) -> None:
    """Inject namelist parameters from ALL components into a STAC item.

    Reads ctx.namelists_by_component (populated by the scan layer); this
    function performs no scanning and imports nothing from scan/.
    """
    total_params = 0

    for component_name, namelists in ctx.namelists_by_component.items():
        for _filename, groups in namelists.items():
            for group_name, values in groups.items():
                for key, value in values.items():
                    if not _is_searchable(value):
                        continue
                    item.properties[f"nml:{component_name}:{group_name}:{key}"] = value
                    total_params += 1

    if total_params > 0:
        url = EXTENSION_URLS["namelist"]
        if url not in item.stac_extensions:
            item.stac_extensions.append(url)
