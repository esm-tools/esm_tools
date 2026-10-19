"""FESOM2 enrichment for the NetCDF reader (a pluggable model extension).

FESOM writes one file per year (``<var>.fesom.YYYY01.01.nc``). A monthly
variable holds 12 steps and a frequency is inferable from spacing; an annual
variable holds a single step stamped mid-December, so the data itself carries
no spacing to infer anything from -- confirmed live against a real AWI-CM3
v3.2 run: 36 such files had no frequency at all. The run's own
``<exp>/config/fesom/namelist.io`` declares each variable's output interval
(``io_list = 'name', n, 'unit', precision, ...``); this enricher reads it and
fills the frequency only when the reader found none, and only for the
unambiguous ``n``/unit combinations below -- anything else is left unset
rather than guessed.
"""

from __future__ import annotations

import re
from functools import lru_cache
from typing import Optional

from esm_catalog.scan.enrichers import hookimpl
from esm_catalog.scan.format import FileFormat

_IO_ENTRY = re.compile(r"'\s*([^']+?)\s*'\s*,\s*(\d+)\s*,\s*'(\w)'\s*,\s*(\d+)")
"""One ``io_list`` entry of FESOM's namelist.io: 'name', n, 'unit', precision."""


@lru_cache(maxsize=8)
def _fesom_io_frequencies(namelist_io: str) -> dict:
    """``{variable: CMIP frequency}`` from a FESOM namelist.io; ``{}`` if unreadable."""
    from upath import UPath

    try:
        text = UPath(namelist_io).read_text()
    except Exception:  # noqa: BLE001 - config not reachable: leave frequency unset
        return {}
    out = {}
    for name, n, unit, _prec in _IO_ENTRY.findall(text):
        n = int(n)
        if unit == "y" and n == 1:
            out[name] = "yr"
        elif unit == "m" and n == 1:
            out[name] = "mon"
        elif unit == "d" and n == 1:
            out[name] = "day"
        elif unit == "h" and n in (1, 3, 6):
            out[name] = f"{n}hr"
    return out


def _fesom_frequency(path) -> Optional[str]:
    """Frequency of a FESOM output file from the run's own namelist.io, else None.

    Needs the layout ``<exp>/outdata/fesom/<var>.fesom.<date>.nc`` next to
    ``<exp>/config/fesom/namelist.io``. Used only when the data itself cannot
    tell: a single-step file has no spacing to infer a frequency from.
    """
    name = path.name
    if ".fesom." not in name:
        return None
    parents = list(path.parents)
    for i, parent in enumerate(parents):
        if parent.name == "outdata" and i + 1 < len(parents):
            nml = parents[i + 1] / "config" / "fesom" / "namelist.io"
            return _fesom_io_frequencies(str(nml)).get(name.split(".fesom.")[0])
    return None


@hookimpl
def enrich_metadata(path, file_format, metadata, datasets):
    if file_format != FileFormat.netcdf:
        return
    if "frequency" not in metadata:
        freq = _fesom_frequency(path)
        if freq:
            metadata["frequency"] = freq
