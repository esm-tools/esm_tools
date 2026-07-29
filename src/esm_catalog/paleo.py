"""Paleo STAC extension: geological time for paleoclimate simulations.

A paleoclimate run represents a point (or span) in geological time that a
standard RFC3339 datetime cannot express — RFC3339 years are four digits
(1..9999), while paleo runs reach thousands to millions of years into the
past. This extension records that geological time in the ``paleo:`` namespace,
mirroring the shape of STAC core's Date and Time fields::

    paleo:datetime        - nominal geological time
    paleo:start_datetime  - start of a transient run's geological span
    paleo:end_datetime    - end of a transient run's geological span

Values are ISO-8601-like strings with an unbounded (optionally negative) year,
e.g. ``"-21000-01-01T00:00:00"`` for the Last Glacial Maximum (~21 ka). They
are meant to be parsed and rendered by the ``paleodatetime`` library on the
consumer side — this catalog stores the datum, not a formatted "21 ka" string.
Presentation is the consumer's job.

The geological time comes only from explicit configuration — the
``general.paleo`` section (see ``CollectionContext.paleo_config``), whose keys
mirror the output fields exactly::

    general:
      paleo:
        datetime: "-21000-01-01T00:00:00"          # a time-slice run (LGM)
        # start_datetime / end_datetime instead     # a transient run

It is never inferred from the item's own datetimes: a paleo run's model
calendar is internal and does not approximate its geological age.

This extension is only the geological time. The run's name/classification is
not part of it.
"""

from __future__ import annotations

import re
from typing import Optional

import pystac

from esm_catalog.registry import EXTENSION_URLS

# The paleodatetime ISO form: signed, unbounded year, then -MM-DDTHH:MM:SS.
# Kept in lockstep with the pattern in the extension's schema.json.
_PALEO_DATETIME = re.compile(r"^-?[0-9]+-[0-9]{2}-[0-9]{2}[T ][0-9]{2}:[0-9]{2}:[0-9]{2}$")


def add_paleo_data(item: pystac.Item, paleo_config: Optional[dict] = None) -> None:
    """Copy geological time from *paleo_config* onto *item*, or do nothing.

    Config keys (the ``general.paleo`` section) mirror STAC core's Date and
    Time as separate scalar datetime strings, never an array:

    - ``datetime`` -> ``paleo:datetime`` (a time-slice run, e.g. LGM);
    - ``start_datetime`` + ``end_datetime`` -> ``paleo:start_datetime`` +
      ``paleo:end_datetime`` (a transient run). Like STAC's start/end_datetime,
      the two must be given together.

    Values are passed through unchanged (they are already the paleodatetime
    strings that belong in the item) after a format check. No-op when no
    datetime is configured (not a paleo run).
    """
    cfg = paleo_config or {}

    start, end = cfg.get("start_datetime"), cfg.get("end_datetime")
    if (start is None) != (end is None):
        raise ValueError(
            "paleo start_datetime and end_datetime must be given together "
            f"(got start_datetime={start!r}, end_datetime={end!r})"
        )
    if start is not None:
        item.properties["paleo:start_datetime"] = _checked(start)
        item.properties["paleo:end_datetime"] = _checked(end)
        _register(item)
        return

    dt = cfg.get("datetime")
    if dt is None:
        return
    item.properties["paleo:datetime"] = _checked(dt)
    _register(item)


def _checked(value: str) -> str:
    """Return *value* if it is a paleodatetime ISO string, else raise ValueError.

    Fails loudly on a malformed config value (a typo, "21 ka", a bare year)
    rather than writing a schema-invalid property that ships unchecked.
    """
    if not isinstance(value, str) or not _PALEO_DATETIME.match(value):
        raise ValueError(
            "paleo datetime must be an ISO-8601-like string "
            f"'<year>-MM-DDTHH:MM:SS' (unbounded/negative year allowed); got {value!r}"
        )
    return value


def _register(item: pystac.Item) -> None:
    """Add the paleo extension URL to *item*.stac_extensions, once."""
    url = EXTENSION_URLS["paleo"]
    if url not in item.stac_extensions:
        item.stac_extensions.append(url)
