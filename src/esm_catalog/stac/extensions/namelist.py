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

from pathlib import Path
from typing import TYPE_CHECKING

from loguru import logger

from esm_catalog.stac.extensions.registry import EXTENSION_URLS

if TYPE_CHECKING:
    pass


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
    """Inject namelist parameters into a STAC item for queryable filtering.

    This propagates namelist parameters from ALL components in the experiment
    to each item, enabling cross-component queries like "find ocean files
    that ran with CO2 = X" (where CO2 is an atmosphere parameter).

    Args:
        item: STAC item dict to modify.
        ctx: CollectionContext with experiment_path and component.

    Returns:
        Modified item dict with namelist properties in item["properties"].
    """
    if ctx.experiment_path is None:
        return item

    config_root = ctx.experiment_path / "config"
    if not config_root.is_dir():
        return item

    try:
        from esm_catalog.scan.namelist import scan_namelist_directory

        total_params = 0

        # Scan ALL component config directories, not just the current component
        for component_dir in config_root.iterdir():
            if not component_dir.is_dir():
                continue

            component_name = component_dir.name
            namelists = scan_namelist_directory(component_dir, component_name)
            if not namelists:
                continue

            # Flatten and add with component prefix to avoid collisions
            # Format: nml:{component}:{group}:{key}
            for filename, groups in namelists.items():
                for group_name, values in groups.items():
                    for key, value in values.items():
                        if value is None:
                            continue
                        if isinstance(value, dict):
                            continue
                        if isinstance(value, list):
                            if len(value) > 10:
                                continue
                            if not all(isinstance(v, (int, float, str, bool, type(None))) for v in value):
                                continue

                        prop_key = f"nml:{component_name}:{group_name}:{key}"
                        item["properties"][prop_key] = value
                        total_params += 1

        # Register extension if we added any parameters
        if total_params > 0:
            url = EXTENSION_URLS.get("namelist")
            if url and url not in item.get("stac_extensions", []):
                item.setdefault("stac_extensions", []).append(url)

            logger.debug(
                "Added {} namelist parameters to item {}",
                total_params,
                item.get("id", "unknown"),
            )

    except ImportError:
        # f90nml not installed
        pass
    except Exception as e:
        logger.debug("Failed to add namelist params to item: {}", e)

    return item


def get_namelist_config_path(
    experiment_path: Path,
    component: str,
) -> Path | None:
    """Determine the config directory for a component.

    ESM-Tools convention: config/{component}/ contains namelists.

    Args:
        experiment_path: Path to experiment root (e.g., /exp/basic-001/).
        component: Component name (e.g., "echam", "fesom").

    Returns:
        Path to config directory if it exists, None otherwise.
    """
    # Try standard ESM-Tools layout
    config_path = experiment_path / "config" / component
    if config_path.is_dir():
        return config_path

    # Try alternative layout (config at experiment root)
    alt_path = experiment_path / component / "config"
    if alt_path.is_dir():
        return alt_path

    return None
