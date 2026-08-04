"""Namelist STAC extension: Fortran namelist parameters as CQL2 queryables.

Collection level (collection.extra_fields):
    nml:files       - namelist filenames
    nml:groups      - namelist groups across all files
    nml:parameters  - flattened "file:group:key" -> value, for CQL2 filtering

Item level (item.properties), one entry per parameter across all components:
    nml:{component}:{file}:{group}:{key} -> value
"""

from __future__ import annotations

from typing import Iterator, Union

import f90nml
import pystac

from esm_catalog.registry import EXTENSION_URLS
from esm_catalog.stac_ext import register_extension, validate

_URL = EXTENSION_URLS["namelist"]

ComponentName = str
"""A model component, e.g. 'echam', 'fesom'."""

NamelistFilename = str
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

ComponentNamelists = dict[NamelistFilename, Namelist]
"""One component's namelists: filename -> parsed namelist."""

NamelistsByComponent = dict[ComponentName, ComponentNamelists]
"""All components' namelists: component -> that component's namelists."""


def add_namelist_collection_extension(
    collection: pystac.Collection, namelists: ComponentNamelists
) -> None:
    """Set collection-level nml:files/groups/parameters from *namelists*, or nothing."""
    if not namelists:
        return
    groups: set[GroupName] = set()
    for file_groups in namelists.values():
        groups.update(file_groups)
    parameters: dict[FlatKey, NamelistValue] = {}
    for filename, group, key, value in _iter_queryable_params(namelists):
        parameters[f"{filename}:{group}:{key}"] = value
    collection.extra_fields["nml:files"] = sorted(namelists)
    collection.extra_fields["nml:groups"] = sorted(groups)
    collection.extra_fields["nml:parameters"] = parameters
    register_extension(collection, _URL)
    validate(collection.to_dict(), "namelist")


def add_namelist_item_extension(
    item: pystac.Item, namelists_by_component: NamelistsByComponent
) -> None:
    """Set item-level nml:{component}:{group}:{key} from *namelists_by_component*."""
    props: dict[str, NamelistValue] = {}
    for component, namelists in namelists_by_component.items():
        for filename, group, key, value in _iter_queryable_params(namelists):
            props[f"nml:{component}:{filename}:{group}:{key}"] = value
    if not props:
        return
    item.properties.update(props)
    register_extension(item, _URL)
    validate(item.to_dict(), "namelist")


def _iter_queryable_params(
    namelists: ComponentNamelists,
) -> Iterator[tuple[NamelistFilename, GroupName, ParameterName, NamelistValue]]:
    """Yield (file, group, key, value) for every queryable parameter.

    A group repeated within a file (an f90nml Cogroup) is disambiguated with an
    ``[index]`` array suffix on the group name — ``rep[0]``, ``rep[1]`` — so the
    occurrences do not collapse onto the same flattened key. A group that
    appears once keeps its bare name.
    """
    for filename, namelist in namelists.items():
        # .items() flattens a repeated group (Cogroup) into one entry per
        # occurrence; count them first so only genuine repeats get an index.
        group_entries = list(namelist.items())
        counts: dict[GroupName, int] = {}
        for group_name, _params in group_entries:
            counts[group_name] = counts.get(group_name, 0) + 1
        next_index: dict[GroupName, int] = {}
        for group_name, params in group_entries:
            if counts[group_name] > 1:
                index = next_index.get(group_name, 0)
                group = f"{group_name}[{index}]"
                next_index[group_name] = index + 1
            else:
                group = group_name
            for key, value in params.items():
                if _is_queryable(value):
                    yield filename, group, key, value


def _is_queryable(value: NamelistValue) -> bool:
    """A JSON scalar, or a list of JSON scalars.

    Nested groups (dicts), None, and non-JSON scalars f90nml can produce
    (e.g. a Fortran ``complex``) are skipped — the scalar and list branches
    whitelist the *same* types so a bare complex can't slip through and crash
    JSON serialization later.
    """
    if isinstance(value, list):
        return all(isinstance(v, (int, float, str, bool, type(None))) for v in value)
    return isinstance(value, (int, float, str, bool))
