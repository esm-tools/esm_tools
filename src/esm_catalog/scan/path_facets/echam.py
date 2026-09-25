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

Also confirmed live: a real experiment's declared ``outdata_targets`` can be
entirely stale (every path pointing at a file that was never produced -- some
namelist option evidently disabled), meaning the real files (``echam``,
``co2``, ``accw``, ``ism``, ...) are found only through the undeclared
filesystem walk, which carries no stream identity until after a read. The
trailing ``_<stream>`` segment of the filename recovers it directly, so this
extractor works with or without an already-known stream.
"""

from __future__ import annotations

import re
from datetime import datetime
from typing import Optional

from upath import UPath

from esm_catalog.scan.path_facets import hookimpl

_NAME_RE = re.compile(
    r"_(?P<year>\d{4})(?P<month>\d{2})\.(?P<day>\d{2})_(?P<stream>[^_.]+)$"
)
"""The date and stream name, both at the end of the filename -- the expid
prefix is never anchored to (it routinely contains underscores itself, e.g.
``historical_c14_init``), only the trailing ``_YYYYMM.DD_<stream>`` is."""


@hookimpl
def extract_path_facets(
    path: UPath, component: str, stream: Optional[str]
) -> Optional[tuple[str, datetime]]:
    if component != "echam":
        return None
    match = _NAME_RE.search(path.name)
    if match is None:
        return None
    resolved_stream = match["stream"]
    if stream is not None and stream != resolved_stream:
        # A caller-supplied stream identity this filename does not confirm --
        # not confident enough to claim it (could be a different naming
        # convention that happens to also end in "_<word>").
        return None
    try:
        start = datetime(int(match["year"]), int(match["month"]), int(match["day"]))
    except ValueError:  # noqa: BLE001 -- an out-of-range date is not a match
        return None
    return resolved_stream, start
