"""Paleo STAC extension: geological time for paleoclimate simulations.

A paleoclimate run represents a point (or span) in geological time that
RFC3339 cannot express — its years are 1..9999, while paleo runs reach
millions of years back. This extension records it in the ``paleo:`` namespace,
mirroring STAC core's Date and Time fields::

    paleo:datetime        - nominal geological time
    paleo:start_datetime  - start of a transient run's geological span
    paleo:end_datetime    - end of a transient run's geological span
    paleo:label           - free-text label for the interval (e.g. "LGM")

The datetimes are ISO-8601-like strings with an unbounded (optionally negative)
year, e.g. ``"-21000-01-01T00:00:00"`` for the Last Glacial Maximum, parsed and
formatted by the ``paleodatetime`` library on the consumer side. ``paleo:label``
is a user-chosen name, not a controlled vocabulary.

The values come from the ``general.paleo`` config section (see
``ExperimentMetadata.paleo_config``), whose keys mirror the output fields::

    general:
      paleo:
        datetime: "-21000-01-01T00:00:00"          # a time-slice run
        label: "LGM"                               # optional free-text label
        # start_datetime / end_datetime instead     # a transient run
"""

from __future__ import annotations

from typing import Optional

import pystac
from paleodatetime import PaleoDateTime
from pydantic import BaseModel, ConfigDict
from pystac.summaries import RangeSummary

from esm_catalog.plugins import hookimpl
from esm_catalog.registry import Extension
from esm_catalog.stac_ext import apply_extension

PaleoDatetime = str
"""An ISO-8601-like geological datetime with an unbounded (optionally negative)
year, e.g. '-21000-01-01T00:00:00' for the Last Glacial Maximum."""

PaleoLabel = str
"""A free-text label for a paleo interval, e.g. 'LGM' (not a controlled vocabulary)."""


class PaleoConfig(BaseModel):
    """The ``general.paleo`` config section — every field optional.

    A time-slice run sets ``datetime``; a transient run sets ``start_datetime``
    and ``end_datetime``; ``label`` is an optional free-text name.
    """

    model_config = ConfigDict(extra="allow")

    datetime: Optional[PaleoDatetime] = None
    start_datetime: Optional[PaleoDatetime] = None
    end_datetime: Optional[PaleoDatetime] = None
    label: Optional[PaleoLabel] = None


# The config keys map 1:1 onto the paleo: fields — derived from PaleoConfig so
# the two never drift.
_KEYS = tuple(PaleoConfig.model_fields)


def paleo_item_props(paleo_config: Optional[PaleoConfig]) -> dict:
    """Return the ``paleo:*`` fields set by *paleo_config*.

    The same for every item in an experiment -- a bulk caller should compute
    this once per scan and reuse it, rather than re-validate/re-dump the
    config per item.

    Parameters
    ----------
    paleo_config : PaleoConfig or None
        The ``general.paleo`` config section, or None.

    Returns
    -------
    dict
        The ``paleo:*`` properties; empty when the config is None or sets no
        paleo fields (i.e. not a paleo run).
    """
    if paleo_config is None:
        return {}
    # Accept a dict or a PaleoConfig; normalize to the model at this boundary.
    paleo_config = PaleoConfig.model_validate(paleo_config)
    fields_set = paleo_config.model_dump(exclude_none=True)
    return {f"paleo:{key}": fields_set[key] for key in _KEYS if key in fields_set}


def add_paleo_item_extension(
    item: pystac.Item,
    paleo_config: Optional[PaleoConfig] = None,
    *,
    props: Optional[dict] = None,
    validate: bool = True,
) -> None:
    """Set the ``paleo:*`` geological time on *item* from *paleo_config*.

    Validated against the paleo extension schema.

    Parameters
    ----------
    item : pystac.Item
        The item to annotate in place.
    paleo_config : PaleoConfig or None, optional
        The ``general.paleo`` config section. No-op when it sets no paleo fields.
    props : dict, optional
        The already-computed ``paleo:*`` properties (see :func:`paleo_item_props`),
        when the caller is applying this to many items and has computed it
        once -- every item in an experiment gets the same paleo config, so
        validating and dumping it per item is wasted work. Recomputed from
        *paleo_config* when omitted.
    validate : bool, optional
        Whether to jsonschema-validate the item after applying the extension.
        A bulk caller that already trusts these code paths should pass False
        after the first item.
    """
    if props is None:
        props = paleo_item_props(paleo_config)
    if not props:
        return
    item.properties.update(props)
    apply_extension(item, Extension.paleo, validate=validate)


def _paleo_year_range(props: dict) -> Optional[RangeSummary[int]]:
    """The ``[min, max]`` integer-year span of *props*' geological datetimes.

    ``paleo:datetime``/``start_datetime``/``end_datetime`` are ISO-8601-like
    strings with an unbounded year that RFC3339/CQL2's temporal literals
    can't parse, and plain string comparison silently breaks across
    different year-magnitude digit counts (e.g. "-2000-01-01" sorts *less*
    than "-21000-01-01" as text, even though -2000 is numerically greater).
    This parses them with ``paleodatetime`` into real integers instead, so a
    search UI can filter/sort on them correctly.

    Returns ``None`` when *props* sets no datetime field (a label-only run).
    """
    years = [
        PaleoDateTime.fromisoformat(props[f"paleo:{field}"]).year
        for field in ("datetime", "start_datetime", "end_datetime")
        if f"paleo:{field}" in props
    ]
    if not years:
        return None
    return RangeSummary(min(years), max(years))


# Added to every negative-capable paleo year before it's ever compared in a
# CQL2 filter (see paleo:year_filter below). Confirmed live against a real
# deployment: a bare negative numeric literal in a CQL2 filter against this
# field crashes the server --
#   "operator does not exist: text * text" (UndefinedFunctionError)
# -- the backend's CQL2-to-SQL compiler evidently lowers unary minus to a
# multiply-by-negative-one against a text-typed JSON path, which fails
# outright; positive literals are unaffected. 100 billion safely exceeds any
# real geological timescale (Earth is ~4.6 billion years old) while staying
# far below float64 precision loss. paleo:year (see above) stays the honest,
# unshifted value for display -- only paleo:year_filter is shifted, and only
# because a search UI needs to build a working filter against it.
PALEO_YEAR_FILTER_OFFSET = 100_000_000_000


def add_paleo_collection_extension(
    collection: pystac.Collection, paleo_config: Optional[PaleoConfig] = None
) -> None:
    """Summarize the ``paleo:*`` geological time on *collection* from *paleo_config*.

    The collection-level view of the same fields lives in ``summaries`` (their
    STAC-idiomatic home). Also adds ``paleo:year`` -- a ``{minimum, maximum}``
    range summary of the real integer year(s), for range search/sort (see
    :func:`_paleo_year_range`); the ``paleo:datetime*`` string fields stay as
    the human-readable display value, unchanged.

    Parameters
    ----------
    collection : pystac.Collection
        The collection to annotate in place.
    paleo_config : PaleoConfig or None, optional
        The ``general.paleo`` config section. No-op when it sets no paleo fields.
    """
    props = paleo_item_props(paleo_config)
    if not props:
        return
    for key, value in props.items():
        collection.summaries.add(key, [value])
    year_range = _paleo_year_range(props)
    if year_range is not None:
        collection.summaries.add("paleo:year", year_range)
        collection.summaries.add(
            "paleo:year_filter",
            RangeSummary(
                year_range.minimum + PALEO_YEAR_FILTER_OFFSET,
                year_range.maximum + PALEO_YEAR_FILTER_OFFSET,
            ),
        )
    apply_extension(collection, Extension.paleo)


#: collection_id -> already-computed paleo:* item props, computed and
#: validated once per experiment (see :func:`_paleo_props_once`).
_PROPS_CACHE: dict[str, dict] = {}


def _paleo_props_once(exp_metadata) -> tuple[dict, bool]:
    """The paleo:* item props for *exp_metadata*, computed once per experiment.

    Same reasoning as :func:`esm_catalog.namelist._namelist_props_once`:
    identical for every item in an experiment, so recompute-and-validate once,
    cached by ``collection_id`` (``ExperimentMetadata`` itself isn't hashable,
    and ``experiment_id`` alone is documented as reusable across distinct
    experiments -- ``collection_id`` is what disambiguates them).

    Returns
    -------
    tuple of (dict, bool)
        The props, and whether this call computed them fresh (the caller
        should validate only when it did).
    """
    collection_id = exp_metadata.collection_id
    if collection_id in _PROPS_CACHE:
        return _PROPS_CACHE[collection_id], False
    props = paleo_item_props(exp_metadata.paleo_config)
    _PROPS_CACHE[collection_id] = props
    return props, True


@hookimpl
def apply_to_item(item, file_metadata, exp_metadata, hints) -> None:
    props, validate = _paleo_props_once(exp_metadata)
    add_paleo_item_extension(
        item,
        exp_metadata.paleo_config,
        props=props,
        validate=validate,
    )


@hookimpl
def apply_to_collection(collection, exp_metadata, hints) -> None:
    add_paleo_collection_extension(collection, exp_metadata.paleo_config)
