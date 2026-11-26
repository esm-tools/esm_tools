"""Shared ECHAM ``.codes``-table helpers.

ECHAM writes GRIB1 with ``paramId=0`` for every field, so cfgrib (and raw
eccodes headers) cannot tell the parameters apart without this table. Used
by both ``model_plugins.fastpath.grib.echam`` (a header-only fast read) and
``model_plugins.enrichers.grib.echam`` (fixing up the generic cfgrib
reader's output) -- the same quirk, read two different ways, so the parsing
itself lives once, here.
"""

from __future__ import annotations

import re
from pathlib import Path
from typing import Optional

from loguru import logger
from upath import UPath

#: A ``.codes`` long-name field, optionally trailed by a bracketed unit.
_LONGNAME_UNIT = re.compile(r"(.+?)\s*\[(.+?)\]\s*$")


def codes_path(grib_path: UPath) -> Optional[Path]:
    """The ``<file>.codes`` sidecar beside *grib_path*, if it exists."""
    candidate = Path(str(grib_path) + ".codes")
    return candidate if candidate.exists() else None


def parse_codes(codes_file: Path) -> dict[int, dict]:
    """Parse a ``.codes`` table: ``code levels shortName offset scale long [unit]``.

    Returns ``{code -> {name, long_name, units}}``. Malformed lines are skipped.
    """
    table: dict[int, dict] = {}
    try:
        lines = codes_file.read_text().splitlines()
    except OSError as exc:
        logger.warning("cannot read .codes file {}: {}", codes_file, exc)
        return table
    for line in lines:
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        parts = line.split()
        if len(parts) < 6:
            continue
        try:
            code = int(parts[0])
        except ValueError:
            continue
        tail = " ".join(parts[5:])
        match = _LONGNAME_UNIT.match(tail)
        long_name, units = (match.group(1).strip(), match.group(2)) if match else (tail, "")
        table[code] = {"name": parts[2], "long_name": long_name, "units": units}
    return table
