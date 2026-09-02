"""Namelist STAC extension: Fortran namelist parameters as CQL2 queryables.

Collection level (``collection.extra_fields``)::

    nml:files       - "component__file" namelist filenames
    nml:groups      - namelist groups across all files
    nml:parameters  - flattened "component__file__group__key" -> value, for CQL2
                      filtering (component-qualified so two components sharing a
                      filename cannot collide)

Item level (``item.properties``), one entry per parameter across all
components::

    nml__{component}__{file}__{group}__{key} -> value

The flattened keys use ``__`` as the separator and sanitise every other
character to ``_``. This is deliberate: pgstac builds an (unquoted) JSON-path
from an unregistered property name, so a name containing ``:`` (the STAC
namespace idiom), ``.`` (from a filename like ``namelist.echam``) or ``[]``
(a repeated-group index) yields a broken path and the CQL2 filter silently
matches nothing. A ``[A-Za-z0-9_]``-only key resolves like any plain property
(``component``, ``variable``), so namelist params are filterable with no
queryables registration at all.
"""

from __future__ import annotations

import re
from collections import Counter
from typing import Iterator, Union

import f90nml
import pystac

from esm_catalog.registry import Extension
from esm_catalog.stac_ext import apply_extension
from esm_catalog.types import ComponentName

#: Separator between the segments of a flattened namelist key.
_KEY_SEP = "__"

#: The item-property prefix marking a flattened namelist parameter.
_ITEM_PREFIX = "nml"


def _flatten(*parts: str) -> str:
    """Join *parts* into a JSON-path-safe flat key.

    Every character outside ``[A-Za-z0-9_]`` is replaced with ``_`` (so a
    filename's ``.`` or a repeated-group ``[N]`` can never reach the key), and
    the sanitised parts are joined with :data:`_KEY_SEP`.
    """
    return _KEY_SEP.join(re.sub(r"[^0-9A-Za-z_]", "_", part) for part in parts)

NamelistFilename = str
"""A namelist filename, e.g. 'namelist.echam'."""

GroupName = str
"""A namelist group (chapter), e.g. 'runctl'."""

ParameterName = str
"""A namelist parameter key, e.g. 'delta_time'."""

FlatKey = str
"""A flattened 'component__file__group__key' identifier, e.g.
'echam__namelist_echam__runctl__delta_time'."""

Namelist = f90nml.Namelist
"""A parsed Fortran namelist (group -> parameters; nested groups are Namelists)."""

NamelistValue = Union[str, int, float, bool, None, list, Namelist]
"""An f90nml value: a scalar, a list, or a nested group (Namelist)."""

ComponentNamelists = dict[NamelistFilename, Namelist]
"""One component's namelists: filename -> parsed namelist."""

NamelistsByComponent = dict[ComponentName, ComponentNamelists]
"""All components' namelists: component -> that component's namelists."""


def add_namelist_collection_extension(
    collection: pystac.Collection, namelists_by_component: NamelistsByComponent
) -> None:
    """Set collection-level nml:files/groups/parameters from every component.

    A Collection is the whole experiment, so parameters are keyed
    ``component:file:group:key`` — component-qualified, so two components that
    ship a same-named namelist file cannot overwrite each other. No-op when
    *namelists_by_component* is empty.

    Parameters
    ----------
    collection : pystac.Collection
        The collection to annotate in place.
    namelists_by_component : NamelistsByComponent
        Every component's namelists, whose files, groups, and queryable
        parameters are summarised at collection level.
    """
    if not namelists_by_component:
        return
    groups: set[GroupName] = set()
    for namelists in namelists_by_component.values():
        for file_groups in namelists.values():
            groups.update(file_groups)
    parameters: dict[FlatKey, NamelistValue] = {
        _flatten(component, filename, group, key): value
        for component, namelists in namelists_by_component.items()
        for filename, group, key, value in _iter_queryable_params(namelists)
    }
    collection.extra_fields["nml:files"] = sorted(
        _flatten(component, filename)
        for component, namelists in namelists_by_component.items()
        for filename in namelists
    )
    collection.extra_fields["nml:groups"] = sorted(groups)
    collection.extra_fields["nml:parameters"] = parameters
    apply_extension(collection, Extension.namelist)


def add_namelist_item_extension(
    item: pystac.Item, namelists_by_component: NamelistsByComponent
) -> None:
    """Set item-level nml:{component}:{file}:{group}:{key} from the given namelists.

    No-op when no queryable parameters are found.

    Parameters
    ----------
    item : pystac.Item
        The item to annotate in place.
    namelists_by_component : NamelistsByComponent
        Every component's namelists, flattened into one queryable property per
        parameter.
    """
    props: dict[str, NamelistValue] = {
        _flatten(_ITEM_PREFIX, component, filename, group, key): value
        for component, namelists in namelists_by_component.items()
        for filename, group, key, value in _iter_queryable_params(namelists)
    }
    if not props:
        return
    item.properties.update(props)
    apply_extension(item, Extension.namelist)


def _iter_queryable_params(
    namelists: ComponentNamelists,
) -> Iterator[tuple[NamelistFilename, GroupName, ParameterName, NamelistValue]]:
    """Yield (file, group, key, value) for every queryable parameter.

    A group repeated within a file (an f90nml Cogroup) is disambiguated with an
    ``[index]`` array suffix on the group name — ``rep[0]``, ``rep[1]`` — so the
    occurrences do not collapse onto the same flattened key. A group that
    appears once keeps its bare name.

    Parameters
    ----------
    namelists : ComponentNamelists
        One component's namelists, filename -> parsed namelist.

    Yields
    ------
    tuple of (NamelistFilename, GroupName, ParameterName, NamelistValue)
        One tuple per queryable parameter.
    """
    for filename, namelist in namelists.items():
        # .items() flattens a repeated group (Cogroup) into one entry per
        # occurrence; count them first so only genuine repeats get an index.
        group_entries = list(namelist.items())
        counts = Counter(group_name for group_name, _params in group_entries)
        next_index: dict[GroupName, int] = {}
        for group_name, params in group_entries:
            if counts[group_name] > 1:
                index = next_index.get(group_name, 0)
                # '_N', not '[N]': brackets are JSON-path array syntax and would
                # break the flattened key's resolution (see module docstring).
                group = f"{group_name}_{index}"
                next_index[group_name] = index + 1
            else:
                group = group_name
            for key, value in params.items():
                if _is_queryable(value):
                    yield filename, group, key, _arrow_safe(value)


def _arrow_safe(value: NamelistValue) -> NamelistValue:
    """Make a namelist value storable in a single-typed column (geoparquet).

    A scalar passes through. A list is stored as a shard column, which arrow
    requires to be one type; f90nml, however, produces mixed-kind lists such as
    ``putrerun = 1, 'months', 'first', 0`` -> ``[1, 'months', 'first', 0]`` (a
    Fortran output-interval triplet). A list mixing text with numbers (or bools)
    cannot be a typed column, so every element is stringified to a uniform
    ``list[str]``; homogeneous numeric or text lists are left as-is. ``None`` is
    preserved so the column can null it.
    """
    if not isinstance(value, list):
        return value
    kinds = set()
    for element in value:
        if element is None:
            continue
        if isinstance(element, bool):
            kinds.add("bool")
        elif isinstance(element, (int, float)):
            kinds.add("number")
        elif isinstance(element, str):
            kinds.add("text")
        else:
            kinds.add("other")
    if len(kinds) <= 1:
        return value
    return [None if element is None else str(element) for element in value]


def _is_queryable(value: NamelistValue) -> bool:
    """Return whether *value* is a JSON scalar, or a list of JSON scalars.

    Nested groups (dicts), None, and non-JSON scalars f90nml can produce
    (e.g. a Fortran ``complex``) are rejected — the scalar and list branches
    whitelist the *same* types so a bare complex can't slip through and crash
    JSON serialization later.

    Parameters
    ----------
    value : NamelistValue
        A parsed namelist value.

    Returns
    -------
    bool
        True if the value is safe to emit as a queryable JSON value.
    """
    if isinstance(value, list):
        return all(
            isinstance(element, (int, float, str, bool, type(None)))
            for element in value
        )
    return isinstance(value, (int, float, str, bool))
