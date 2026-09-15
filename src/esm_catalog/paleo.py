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
from typing_extensions import TypedDict  # pydantic needs this flavor as a model field

from esm_catalog.registry import Extension
from esm_catalog.stac_ext import apply_extension

PaleoDatetime = str
"""An ISO-8601-like geological datetime with an unbounded (optionally negative)
year, e.g. '-21000-01-01T00:00:00' for the Last Glacial Maximum."""

PaleoLabel = str
"""A free-text label for a paleo interval, e.g. 'LGM' (not a controlled vocabulary)."""


class PaleoConfig(TypedDict, total=False):
    """The ``general.paleo`` config section — every key optional.

    A time-slice run sets ``datetime``; a transient run sets ``start_datetime``
    and ``end_datetime``; ``label`` is an optional free-text name.
    """

    datetime: PaleoDatetime
    start_datetime: PaleoDatetime
    end_datetime: PaleoDatetime
    label: PaleoLabel


# The config keys map 1:1 onto the paleo: fields — derived from PaleoConfig so
# the two never drift.
_KEYS = tuple(PaleoConfig.__annotations__)


def _to_paleo_props(paleo_config: Optional[PaleoConfig]) -> dict:
    """Return the ``paleo:*`` fields set by *paleo_config*.

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
    cfg = paleo_config or {}
    return {f"paleo:{key}": cfg[key] for key in _KEYS if cfg.get(key) is not None}


def add_paleo_item_extension(
    item: pystac.Item, paleo_config: Optional[PaleoConfig] = None
) -> None:
    """Set the ``paleo:*`` geological time on *item* from *paleo_config*.

    Validated against the paleo extension schema.

    Parameters
    ----------
    item : pystac.Item
        The item to annotate in place.
    paleo_config : PaleoConfig or None, optional
        The ``general.paleo`` config section. No-op when it sets no paleo fields.
    """
    props = _to_paleo_props(paleo_config)
    if not props:
        return
    item.properties.update(props)
    apply_extension(item, Extension.paleo)


def add_paleo_collection_extension(
    collection: pystac.Collection, paleo_config: Optional[PaleoConfig] = None
) -> None:
    """Summarize the ``paleo:*`` geological time on *collection* from *paleo_config*.

    The collection-level view of the same fields lives in ``summaries`` (their
    STAC-idiomatic home).

    Parameters
    ----------
    collection : pystac.Collection
        The collection to annotate in place.
    paleo_config : PaleoConfig or None, optional
        The ``general.paleo`` config section. No-op when it sets no paleo fields.
    """
    props = _to_paleo_props(paleo_config)
    if not props:
        return
    for key, value in props.items():
        collection.summaries.add(key, [value])
    apply_extension(collection, Extension.paleo)
