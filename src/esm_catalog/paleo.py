"""Paleo STAC extension: geological time for paleoclimate simulations.

A paleoclimate run represents a point (or span) in geological time that
RFC3339 cannot express — its years are 1..9999, while paleo runs reach
millions of years back. This extension records it in the ``paleo:`` namespace,
mirroring STAC core's Date and Time fields::

    paleo:datetime        - nominal geological time
    paleo:start_datetime  - start of a transient run's geological span
    paleo:end_datetime    - end of a transient run's geological span

Values are ISO-8601-like strings with an unbounded (optionally negative) year,
e.g. ``"-21000-01-01T00:00:00"`` for the Last Glacial Maximum, parsed and
formatted by the ``paleodatetime`` library on the consumer side.

The time comes from the ``general.paleo`` config section (see
``CollectionContext.paleo_config``), whose keys mirror the output fields::

    general:
      paleo:
        datetime: "-21000-01-01T00:00:00"          # a time-slice run (LGM)
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

# The config keys map 1:1 onto the paleo: item properties.
_KEYS = ("datetime", "start_datetime", "end_datetime")


@lru_cache(maxsize=None)
def _schema() -> dict:
    """Load the paleo extension schema (once; resolved install-aware)."""
    path = esm_tools.get_config_filepath("stac-extensions/paleo/v1.0.0/schema.json")
    with open(path) as fh:
        return json.load(fh)


@lru_cache(maxsize=None)
def _validate(frozen_props: tuple) -> None:
    """Validate paleo properties against the schema, memoized by content.

    A scan applies the same config to every item, so memoizing collapses
    validation to once per distinct config.
    """
    jsonschema.validate(
        instance={
            "type": "Feature",
            "stac_extensions": [_PALEO_URL],
            "properties": dict(frozen_props),
        },
        schema=_schema(),
    )


def add_paleo_data(item: pystac.Item, paleo_config: dict | None = None) -> None:
    """Copy geological time from *paleo_config* onto *item*, or do nothing.

    Config keys mirror STAC core's Date and Time and map 1:1 onto the
    ``paleo:`` properties: ``datetime`` for a time-slice run, or
    ``start_datetime`` + ``end_datetime`` for a transient one. No-op when none
    are set. The fields are validated against the paleo extension schema.
    """
    cfg = paleo_config or {}
    props = {f"paleo:{k}": cfg[k] for k in _KEYS if cfg.get(k) is not None}
    if not props:
        return

    _validate(tuple(sorted(props.items())))

    item.properties.update(props)
    if _PALEO_URL not in item.stac_extensions:
        item.stac_extensions.append(_PALEO_URL)
