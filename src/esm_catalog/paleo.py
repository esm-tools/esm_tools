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
``CollectionContext.paleo_config``), whose keys mirror the output fields::

    general:
      paleo:
        datetime: "-21000-01-01T00:00:00"          # a time-slice run
        label: "LGM"                               # optional free-text label
        # start_datetime / end_datetime instead     # a transient run
"""

from __future__ import annotations

import pystac

from esm_catalog.registry import EXTENSION_URLS
from esm_catalog.stac_ext import register_extension, validate

_PALEO_URL = EXTENSION_URLS["paleo"]

# The config keys map 1:1 onto the paleo: fields.
_KEYS = ("datetime", "start_datetime", "end_datetime", "label")


def _paleo_props(paleo_config: dict | None) -> dict:
    """Return the ``paleo:*`` fields set by *paleo_config* (empty = not paleo)."""
    cfg = paleo_config or {}
    return {f"paleo:{k}": cfg[k] for k in _KEYS if cfg.get(k) is not None}


def add_paleo_data(item: pystac.Item, paleo_config: dict | None = None) -> None:
    """Set the ``paleo:*`` geological time on *item* from *paleo_config*.

    No-op when *paleo_config* sets no paleo fields. Validated against the paleo
    extension schema.
    """
    props = _paleo_props(paleo_config)
    if not props:
        return
    item.properties.update(props)
    register_extension(item, _PALEO_URL)
    validate(item.to_dict(), "paleo")


def add_paleo_summary(
    collection: pystac.Collection, paleo_config: dict | None = None
) -> None:
    """Summarize the ``paleo:*`` geological time on *collection* from *paleo_config*.

    The collection-level view of the same fields, in ``summaries`` (their
    STAC-idiomatic home). No-op when *paleo_config* sets no paleo fields.
    """
    summaries = {k: [v] for k, v in _paleo_props(paleo_config).items()}
    if not summaries:
        return
    for key, values in summaries.items():
        collection.summaries.add(key, values)
    register_extension(collection, _PALEO_URL)
    validate(collection.to_dict(), "paleo")
