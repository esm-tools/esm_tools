"""Namelist STAC extension: Fortran namelist parameters for search.

Collection level (collection.extra_fields):
    nml:files       - namelist filenames
    nml:groups      - namelist groups across all files
    nml:parameters  - flattened "file:group:key" -> scalar, for CQL2 filtering

Item level (item.properties), one entry per parameter across all components:
    nml:{component}:{file}:{group}:{key} -> value
"""

from __future__ import annotations

from typing import Iterator, Union

import f90nml
import pystac

from esm_catalog.registry import EXTENSION_URLS

_URL = EXTENSION_URLS["namelist"]

ComponentName = str
"""A model component, e.g. 'echam', 'fesom'."""

NamelistFileName = str
"""A namelist filename, e.g. 'namelist.echam'."""

GroupName = str
"""A namelist group (chapter), e.g. 'runctl'."""

ParameterName = str
"""A namelist parameter key, e.g. 'delta_time'."""

FlatKey = str
"""A flattened 'file:group:key' identifier, e.g. 'namelist.echam:runctl:delta_time'."""

Namelist = f90nml.Namelist
"""A parsed Fortran namelist (group -> parameters; nested groups are Namelists)."""

NamelistValue = Union[str, int, float, bool, None, list, Namelist]
"""An f90nml value: a scalar, a list, or a nested group (Namelist)."""

NamelistData = dict[NamelistFileName, Namelist]
"""One component's namelists: filename -> parsed namelist."""

NamelistsByComponent = dict[ComponentName, NamelistData]
"""All components' namelists: component -> that component's namelists."""


def add_namelist_extension(collection: pystac.Collection, namelists: NamelistData) -> None:
    """Set collection-level nml:files/groups/parameters from *namelists*, or nothing."""
    if not namelists:
        return
    groups: set[GroupName] = set()
    for file_groups in namelists.values():
        groups.update(file_groups)
    parameters: dict[FlatKey, NamelistValue] = {}
    for file, group, key, value in _searchable(namelists):
        parameters[f"{file}:{group}:{key}"] = value
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
        for file, group, key, value in _searchable(namelists):
            item.properties[f"nml:{component}:{file}:{group}:{key}"] = value
            wrote = True
    if wrote:
        _register(item)


def _searchable(
    namelists: NamelistData,
) -> Iterator[tuple[NamelistFileName, GroupName, ParameterName, NamelistValue]]:
    """Yield (file, group, key, value) for every searchable parameter."""
    for file, groups in namelists.items():
        for group, values in groups.items():
            for key, value in values.items():
                if _is_searchable(value):
                    yield file, group, key, value


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
