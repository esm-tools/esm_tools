"""ECHAM's own on-disk output naming: ``<expid>_YYYYMM.DD_<stream>``.

Confirmed live against a real production file
(``historical_c14_init_185001.01_echam``) -- this is the actual Fortran-side
naming convention the model writes, independent of (and more precise than)
the fuzzy, wildcard-bearing ``outdata_sources`` glob pattern declared in
``configs/components/echam/echam.yaml`` (``${start_date!syear}*.${start_date!sday}_STREAM``
-- the literal ``*`` swallows the month, so reverse-compiling *that* template
can only ever recover the year, not the month a monthly-frequency stream
actually needs). Hardcoding the real convention here, gated to ``component ==
"echam"``, recovers full precision instead.
"""

from __future__ import annotations

import re
from datetime import datetime
from typing import Optional

from upath import UPath

from esm_catalog.scan.path_facets import hookimpl

_DATE_RE_TEMPLATE = r"_(?P<year>\d{4})(?P<month>\d{2})\.(?P<day>\d{2})_%STREAM%$"
"""The date immediately preceding the (already-known) stream suffix -- using
*stream* as a literal anchor sidesteps ever having to guess where the expid
ends, since expid itself routinely contains underscores (e.g.
``historical_c14_init``). ``%STREAM%`` is a plain substring placeholder, not
a ``str.format`` field -- the regex's own ``{4}``/``{2}`` would collide with
``.format()``'s brace syntax."""


@hookimpl
def extract_start_datetime(
    path: UPath, component: str, stream: Optional[str]
) -> Optional[datetime]:
    if component != "echam" or not stream:
        return None
    pattern = _DATE_RE_TEMPLATE.replace("%STREAM%", re.escape(stream))
    match = re.search(pattern, path.name)
    if match is None:
        return None
    try:
        return datetime(int(match["year"]), int(match["month"]), int(match["day"]))
    except ValueError:  # noqa: BLE001 -- an out-of-range date is not a match
        return None
