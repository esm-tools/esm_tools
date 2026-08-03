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

import json
from functools import lru_cache

import esm_tools
import jsonschema
import pystac

from esm_catalog.registry import EXTENSION_URLS

_PALEO_URL = EXTENSION_URLS["paleo"]

# The config keys map 1:1 onto the paleo: fields.
_KEYS = ("datetime", "start_datetime", "end_datetime", "label")


@lru_cache(maxsize=None)
def _schema() -> dict:
    """Load the paleo extension schema (once; resolved install-aware)."""
    path = esm_tools.get_config_filepath("stac-extensions/paleo/v1.0.0/schema.json")
    with open(path) as fh:
        return json.load(fh)


def _paleo_props(paleo_config: dict | None) -> dict:
    """Return the ``paleo:*`` fields set by *paleo_config*, or an empty dict.

    ``datetime`` -> a time-slice run; ``start_datetime`` + ``end_datetime`` ->
    a transient one. Empty means not a paleo run.
    """
    cfg = paleo_config or {}
    return {f"paleo:{k}": cfg[k] for k in _KEYS if cfg.get(k) is not None}


def _register(obj) -> None:
    """Add the paleo extension URL to *obj*.stac_extensions, once."""
    if _PALEO_URL not in obj.stac_extensions:
        obj.stac_extensions.append(_PALEO_URL)


@lru_cache(maxsize=None)
def _validate(kind: str, frozen: tuple) -> None:
    """Validate a paleo probe against the schema, memoized by content.

    A scan applies the same config to every item/collection, so memoizing
    collapses validation to once per distinct config. *kind* is ``"Feature"``
    (fields in ``properties``) or ``"Collection"`` (fields in ``summaries``).
    """
    fields = {k: list(v) if kind == "Collection" else v for k, v in frozen}
    instance = {"type": kind, "stac_extensions": [_PALEO_URL]}
    instance["summaries" if kind == "Collection" else "properties"] = fields
    jsonschema.validate(instance=instance, schema=_schema())


def add_paleo_data(item: pystac.Item, paleo_config: dict | None = None) -> None:
    """Copy geological time from *paleo_config* onto *item*, or do nothing.

    Config keys mirror STAC core's Date and Time and map 1:1 onto the
    ``paleo:`` item properties: ``datetime`` for a time-slice run, or
    ``start_datetime`` + ``end_datetime`` for a transient one. No-op when none
    are set. Validated against the paleo extension schema.
    """
    props = _paleo_props(paleo_config)
    if not props:
        return
    _validate("Feature", tuple(sorted(props.items())))
    item.properties.update(props)
    _register(item)


def add_paleo_summary(collection, paleo_config: dict | None = None) -> None:
    """Summarize geological time from *paleo_config* on *collection*, or nothing.

    The collection-level view of the same ``paleo:*`` fields, in ``summaries``
    (their STAC-idiomatic home). Config-driven, so each summary is the single
    configured value. No-op when nothing is set.
    """
    summaries = {k: [v] for k, v in _paleo_props(paleo_config).items()}
    if not summaries:
        return
    _validate("Collection", tuple(sorted((k, tuple(v)) for k, v in summaries.items())))
    for key, values in summaries.items():
        collection.summaries.add(key, values)
    _register(collection)
