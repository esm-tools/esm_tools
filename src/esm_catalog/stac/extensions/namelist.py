"""Namelist STAC extension: simulation parameters from Fortran namelists.

Adds namelist configuration data to STAC collections, allowing users to
search for simulations by their runtime parameters (timestep, coupling
settings, physics options, etc.).

Properties added at collection level:
    nml:files        - List of namelist filenames
    nml:groups       - List of namelist groups (chapters) across all files
    nml:parameters   - Flattened key-value parameters for search
    nml:raw          - Full nested namelist structure for display

Example collection properties::

    {
        "nml:files": ["namelist.echam", "namelist.jsbach"],
        "nml:groups": ["runctl", "radctl", "jsbach_ctl"],
        "nml:parameters": {
            "runctl:delta_time": 450,
            "runctl:lcouple": true,
            "radctl:co2vmr": 0.000284
        },
        "nml:raw": {
            "namelist.echam": {
                "runctl": {"delta_time": 450, ...},
                "radctl": {"co2vmr": 0.000284, ...}
            }
        }
    }

This allows CQL2 queries like:
    - Find simulations with delta_time > 300
    - Find all coupled simulations (lcouple = true)
    - Find simulations with specific CO2 levels
"""

from __future__ import annotations

from loguru import logger

from esm_catalog.stac.extensions.registry import EXTENSION_URLS


def add_namelist_extension(
    collection: dict,
    namelists: dict,
) -> dict:
    """Inject namelist extension fields into a STAC collection.

    Args:
        collection: STAC collection dict to modify.
        namelists: Output from scan_namelist_directory(), mapping
            filename -> {group -> {key -> value}}.

    Returns:
        Modified collection dict with namelist properties.
    """
    if not namelists:
        return collection

    # Extract file list
    files = sorted(namelists.keys())

    # Extract unique group names across all files
    groups: set[str] = set()
    for file_data in namelists.values():
        groups.update(file_data.keys())

    # Flatten parameters for searchable properties
    parameters = _flatten_for_search(namelists)

    # Add to collection properties (or root for collection-level metadata)
    # STAC collections can have arbitrary properties at root level
    collection["nml:files"] = files
    collection["nml:groups"] = sorted(groups)
    collection["nml:parameters"] = parameters
    collection["nml:raw"] = namelists

    # Register extension
    url = EXTENSION_URLS.get("namelist")
    if url and url not in collection.get("stac_extensions", []):
        collection.setdefault("stac_extensions", []).append(url)

    logger.debug(
        "Added namelist extension: {} files, {} groups, {} parameters",
        len(files),
        len(groups),
        len(parameters),
    )

    return collection


def _flatten_for_search(namelists: dict) -> dict:
    """Flatten namelist structure for searchable parameters.

    Creates keys in format "group:key" for CQL2 filtering.
    Only includes scalar values that are useful for search.
    """
    result: dict = {}

    for _filename, groups in namelists.items():
        for group_name, values in groups.items():
            for key, value in values.items():
                # Skip None values
                if value is None:
                    continue

                # Skip complex nested structures
                if isinstance(value, dict):
                    continue

                # For lists, only keep simple scalar lists
                if isinstance(value, list):
                    if len(value) > 10:
                        continue  # Skip large arrays
                    if not all(
                        isinstance(v, (int, float, str, bool, type(None)))
                        for v in value
                    ):
                        continue

                flat_key = f"{group_name}:{key}"

                # If key already exists from another file, prefer the first
                # (or we could merge, but that gets complicated)
                if flat_key not in result:
                    result[flat_key] = value

    return result


def add_namelist_item_extension(item: dict, ctx) -> dict:
    """Inject namelist parameters from ALL components into a STAC item.

    Reads ctx.namelists_by_component (populated by the scan layer); this
    function performs no scanning and imports nothing from scan/.
    """
    by_component = ctx.namelists_by_component
    total_params = 0

    for component_name, namelists in by_component.items():
        for _filename, groups in namelists.items():
            for group_name, values in groups.items():
                for key, value in values.items():
                    if value is None or isinstance(value, dict):
                        continue
                    if isinstance(value, list):
                        if len(value) > 10:
                            continue
                        if not all(
                            isinstance(v, (int, float, str, bool, type(None)))
                            for v in value
                        ):
                            continue
                    item["properties"][f"nml:{component_name}:{group_name}:{key}"] = value
                    total_params += 1

    if total_params > 0:
        url = EXTENSION_URLS.get("namelist")
        if url and url not in item.get("stac_extensions", []):
            item.setdefault("stac_extensions", []).append(url)

    return item


