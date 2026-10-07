"""Namelist STAC extension: Fortran namelist parameters as CQL2 queryables.

Collection level (``collection.extra_fields``)::

    namelist:parameters - nested component -> file -> group -> key -> value

Item level (``item.properties``), one entry per parameter across all
components::

    namelist__{component}__{file}__{group}__{key} -> value

The item-level keys are flat, joined with ``__``, and every segment sanitises
every character outside ``[A-Za-z0-9_]`` to ``_``. This is deliberate: pgstac
builds an (unquoted) JSON-path from an *unregistered* property name, so a name
containing ``:`` (the STAC namespace idiom), ``.`` (from a filename like
``namelist.echam``) or ``[]`` (a repeated-group index) yields a broken path and
the CQL2 filter silently matches nothing. A ``[A-Za-z0-9_]``-only key resolves
like any plain property (``component``, ``variable``), so item-level namelist
params are filterable with no queryables registration at all.

The collection-level structure is real nesting instead of a flattened key:
each segment is sanitised the same way, but *registered* as a dotted-path
queryable (``namelist:parameters.<component>.<file>.<group>.<key>``, see
:func:`namelist_collection_queryables`) rather than joined into one string.
pgstac's registered-queryable resolution splits a dotted name into a nested
JSON path correctly; it is only the *unregistered* fallback that breaks on a
nested/dotted name (confirmed against pgstac's own source and
https://github.com/stac-utils/pgstac/issues/483 — see
docs/superpowers/specs/2026-10-07-namelist-stac-extension-redesign-design.md).
"""

from __future__ import annotations

import re
from collections import Counter
from typing import Iterator, Optional, Union

import f90nml
import pystac

from esm_catalog.plugins import hookimpl
from esm_catalog.registry import Extension
from esm_catalog.stac_ext import apply_extension
from esm_catalog.types import ComponentName

#: Separator between the segments of a flattened item-level namelist key.
_KEY_SEP = "__"

#: The item-property prefix marking a flattened namelist parameter, and the
#: collection-property prefix marking the nested parameters dict.
_ITEM_PREFIX = "namelist"

#: The collection-level extra_fields key holding the nested parameters.
_COLLECTION_KEY = "namelist:parameters"


def _sanitize_segment(part: str) -> str:
    """Make *part* JSON-path-safe: every character outside ``[A-Za-z0-9_]``
    becomes ``_`` (so a filename's ``.`` or a repeated-group ``[N]`` can never
    reach a flattened key or a registered dotted-path queryable)."""
    return re.sub(r"[^0-9A-Za-z_]", "_", part)


def _flatten(*parts: str) -> str:
    """Join *parts* into a JSON-path-safe flat key (item-level only).

    Every segment is sanitised by :func:`_sanitize_segment`, then joined with
    :data:`_KEY_SEP`.
    """
    return _KEY_SEP.join(_sanitize_segment(part) for part in parts)

NamelistFilename = str
"""A namelist filename, e.g. 'namelist.echam'."""

GroupName = str
"""A namelist group (chapter), e.g. 'runctl'."""

ParameterName = str
"""A namelist parameter key, e.g. 'delta_time'."""

Namelist = f90nml.Namelist
"""A parsed Fortran namelist (group -> parameters; nested groups are Namelists)."""

NamelistValue = Union[str, int, float, bool, None, list, Namelist]
"""An f90nml value: a scalar, a list, or a nested group (Namelist)."""

ComponentNamelists = dict[NamelistFilename, Namelist]
"""One component's namelists: filename -> parsed namelist."""

NamelistsByComponent = dict[ComponentName, ComponentNamelists]
"""All components' namelists: component -> that component's namelists."""


def _nested_parameters(
    namelists_by_component: NamelistsByComponent,
) -> dict:
    """Build namelist:parameters as real nesting: component -> file -> group -> key -> value.

    A Collection is the whole experiment, so the outermost level is the
    component — two components shipping a same-named namelist file land in
    separate branches and cannot collide. Every segment is sanitised exactly
    as an item-level flattened key's segments are (see
    :func:`_sanitize_segment`); only the joining differs, real dict nesting
    instead of string concatenation.
    """
    nested: dict = {}
    for component, namelists in namelists_by_component.items():
        component_level = nested.setdefault(_sanitize_segment(component), {})
        for filename, group, key, value in _iter_queryable_params(namelists):
            file_level = component_level.setdefault(_sanitize_segment(filename), {})
            group_level = file_level.setdefault(_sanitize_segment(group), {})
            group_level[_sanitize_segment(key)] = value
    return nested


def add_namelist_collection_extension(
    collection: pystac.Collection, namelists_by_component: NamelistsByComponent
) -> None:
    """Set collection-level namelist:parameters (nested) from every component.

    No-op when *namelists_by_component* is empty.

    Parameters
    ----------
    collection : pystac.Collection
        The collection to annotate in place.
    namelists_by_component : NamelistsByComponent
        Every component's namelists, nested into the collection-level
        queryable parameters.
    """
    if not namelists_by_component:
        return
    collection.extra_fields[_COLLECTION_KEY] = _nested_parameters(
        namelists_by_component
    )
    apply_extension(collection, Extension.namelist)


def namelist_collection_queryables(
    namelists_by_component: NamelistsByComponent,
) -> dict[str, dict[str, str]]:
    """Map each namelist:parameters leaf to a dotted-path queryable definition.

    Registered as ``"namelist:parameters.<component>.<file>.<group>.<key>"`` --
    pgstac's registered-queryable resolution splits a dotted name into a
    nested jsonb path, so the collection-level nested structure stays
    filterable via CQL2 collection search. Empty when there are no namelists.
    """
    return {
        f"{_COLLECTION_KEY}."
        + ".".join(
            _sanitize_segment(part) for part in (component, filename, group, key)
        ): {"type": _json_type(value)}
        for component, namelists in namelists_by_component.items()
        for filename, group, key, value in _iter_queryable_params(namelists)
    }


def namelist_item_props(
    namelists_by_component: NamelistsByComponent,
) -> dict[str, NamelistValue]:
    """Flatten every component's namelists into item-level namelist__ properties.

    The same for every item in an experiment -- the caller should compute this
    once per scan and reuse it, rather than call it per item (namelist trees
    can be large; walking one per file, 30k+ times over, is real wasted work).
    """
    return {
        _flatten(_ITEM_PREFIX, component, filename, group, key): value
        for component, namelists in namelists_by_component.items()
        for filename, group, key, value in _iter_queryable_params(namelists)
    }


def add_namelist_item_extension(
    item: pystac.Item,
    namelists_by_component: NamelistsByComponent,
    *,
    props: Optional[dict[str, NamelistValue]] = None,
    validate: bool = True,
) -> None:
    """Set item-level namelist__{component}__{file}__{group}__{key} from the namelists.

    No-op when no queryable parameters are found.

    Parameters
    ----------
    item : pystac.Item
        The item to annotate in place.
    namelists_by_component : NamelistsByComponent
        Every component's namelists, flattened into one queryable property per
        parameter.
    props : dict, optional
        The already-flattened properties (see :func:`namelist_item_props`), when
        the caller is applying this to many items and has computed it once.
        Recomputed from *namelists_by_component* when omitted.
    validate : bool, optional
        Whether to jsonschema-validate the item after applying the extension.
        Measured dominant cost of a bulk scan's per-item work (patternProperties
        matching against every namelist__ property, with recursive oneOf/$ref
        resolution) -- a bulk caller that already trusts these code paths (e.g.
        covered by the test suite's schema-conformance tests) should pass False.
    """
    if props is None:
        props = namelist_item_props(namelists_by_component)
    if not props:
        return
    item.properties.update(props)
    apply_extension(item, Extension.namelist, validate=validate)


#: collection_id -> already-flattened namelist item props, computed and
#: validated once per experiment (see :func:`_namelist_props_once`).
_PROPS_CACHE: dict[str, dict] = {}


def _namelist_props_once(exp_metadata) -> tuple[dict, bool]:
    """The namelist item props for *exp_metadata*, computed once per experiment.

    Every item in an experiment gets the same properties -- recomputing (a
    namelist-tree walk) and re-validating (jsonschema patternProperties
    matching, the measured dominant per-item cost on a bulk scan) for each one
    is redundant. Cached by ``collection_id`` rather than *exp_metadata*
    itself, since ``ExperimentMetadata`` isn't hashable (its
    ``namelists_by_component`` holds ``f90nml.Namelist`` objects) -- and rather
    than ``experiment_id`` alone, which is documented as reusable across
    distinct experiments (``collection_id`` is what disambiguates them).

    Returns
    -------
    tuple of (dict, bool)
        The props, and whether this call computed them fresh (the caller
        should validate only when it did).
    """
    collection_id = exp_metadata.collection_id
    if collection_id in _PROPS_CACHE:
        return _PROPS_CACHE[collection_id], False
    props = namelist_item_props(exp_metadata.namelists_by_component)
    _PROPS_CACHE[collection_id] = props
    return props, True


@hookimpl
def apply_to_item(item, file_metadata, exp_metadata, hints) -> None:
    props, validate = _namelist_props_once(exp_metadata)
    add_namelist_item_extension(
        item,
        exp_metadata.namelists_by_component,
        props=props,
        validate=validate,
    )


@hookimpl
def apply_to_collection(collection, exp_metadata, hints) -> None:
    add_namelist_collection_extension(collection, exp_metadata.namelists_by_component)


def _json_type(value: NamelistValue) -> str:
    """The JSON-Schema ``type`` for a queryable definition of *value*.

    ``bool`` is checked before ``int`` (it subclasses it). Mixed lists have
    already been stringified by :func:`_arrow_safe`, so a list is always
    ``array``. The types drive pgstac's CQL2 casting (``integer``/``number`` ->
    numeric comparison; ``array`` -> text array; everything else -> text).
    """
    if isinstance(value, bool):
        return "boolean"
    if isinstance(value, int):
        return "integer"
    if isinstance(value, float):
        return "number"
    if isinstance(value, list):
        return "array"
    return "string"


def namelist_queryables(
    namelists_by_component: NamelistsByComponent,
) -> dict[str, dict[str, str]]:
    """Map each item-level ``namelist__`` key to a JSON-Schema queryable definition.

    The result is the ``properties`` body of a ``pypgstac load-queryables`` file:
    ``{name: {"type": <json-type>}}``, one entry per item-level namelist property.
    Empty when there are no namelists.
    """
    return {
        _flatten(_ITEM_PREFIX, component, filename, group, key): {
            "type": _json_type(value)
        }
        for component, namelists in namelists_by_component.items()
        for filename, group, key, value in _iter_queryable_params(namelists)
    }


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
