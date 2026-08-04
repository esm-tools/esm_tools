"""Namelist STAC extension: Fortran namelist parameters for search.

Collection level (collection.extra_fields):
    nml:files       - namelist filenames
    nml:groups      - namelist groups across all files
    nml:parameters  - flattened "group:key" -> scalar, for CQL2 filtering

Item level (item.properties), one entry per parameter across all components:
    nml:{component}:{group}:{key} -> value
"""

from __future__ import annotations

from typing import Iterator, Union

import pystac

from esm_catalog.registry import EXTENSION_URLS

_URL = EXTENSION_URLS["namelist"]

NamelistScalar = Union[str, int, float, bool, None]
# An f90nml value: a scalar, a list, or a nested group (dict).
NamelistValue = Union[NamelistScalar, list, dict]
# filename -> group -> key -> value
NamelistData = dict[str, dict[str, dict[str, NamelistValue]]]
# component -> that component's namelist data
NamelistsByComponent = dict[str, NamelistData]


def add_namelist_extension(collection: pystac.Collection, namelists: NamelistData) -> None:
    """Set collection-level nml:files/groups/parameters from *namelists*, or nothing."""
    if not namelists:
        return
    groups: set[str] = set()
    for file_groups in namelists.values():
        groups.update(file_groups)
    parameters: dict[str, NamelistValue] = {}
    for group, key, value in _searchable(namelists):
        parameters.setdefault(f"{group}:{key}", value)  # first file wins
    collection.extra_fields["nml:files"] = sorted(namelists)
    collection.extra_fields["nml:groups"] = sorted(groups)
    collection.extra_fields["nml:parameters"] = parameters
    _register(collection)


def add_namelist_item_extension(
    item: pystac.Item, namelists_by_component: NamelistsByComponent
) -> None:
    """Set item-level nml:{component}:{group}:{key} from *namelists_by_component*."""
    wrote = False
    for component, namelists in namelists_by_component.items():
        for group, key, value in _searchable(namelists):
            item.properties[f"nml:{component}:{group}:{key}"] = value
            wrote = True
    if wrote:
        _register(item)


def _searchable(namelists: NamelistData) -> Iterator[tuple[str, str, NamelistValue]]:
    """Yield (group, key, value) for every searchable parameter."""
    for groups in namelists.values():
        for group, values in groups.items():
            for key, value in values.items():
                if _is_searchable(value):
                    yield group, key, value


def _is_searchable(value: NamelistValue) -> bool:
    """A scalar or a list of scalars; nested dicts and None are skipped."""
    if value is None or isinstance(value, dict):
        return False
    if isinstance(value, list):
        return all(isinstance(v, (int, float, str, bool, type(None))) for v in value)
    return True


def _register(obj: pystac.STACObject) -> None:
    if _URL not in obj.stac_extensions:
        obj.stac_extensions.append(_URL)
