"""ECHAM enrichment for the GRIB reader (a pluggable model extension).

ECHAM writes GRIB1 with ``paramId=0`` for every field, so cfgrib cannot tell the
parameters apart and collapses them into a single ``unknown`` variable. ECHAM
ships a companion ``<file>.codes`` table mapping the GRIB
``indicatorOfParameter`` to a short name, long name and units; reading each
message's ``indicatorOfParameter`` with eccodes and joining it to that table
recovers the real variable set.

This runs only when the basic reader left an ``unknown`` variable *and* a
``.codes`` file sits beside the data -- otherwise it is a no-op, so it never
disturbs well-formed (e.g. GRIB2) files.
"""

from __future__ import annotations

import re
from pathlib import Path
from typing import Optional

from loguru import logger
from upath import UPath

from esm_catalog.scan.readers.grib import register_enricher
from esm_catalog.types import FileMetadata, ScannedVariable

_UNKNOWN = ("unknown", "")

#: A ``.codes`` long-name field, optionally trailed by a bracketed unit.
_LONGNAME_UNIT = re.compile(r"(.+?)\s*\[(.+?)\]\s*$")


def _codes_path(grib_path: UPath) -> Optional[Path]:
    """The ``<file>.codes`` sidecar beside *grib_path*, if it exists."""
    candidate = Path(str(grib_path) + ".codes")
    return candidate if candidate.exists() else None


def _parse_codes(codes_path: Path) -> dict[int, dict]:
    """Parse a ``.codes`` table: ``code levels shortName offset scale long [unit]``.

    Returns ``{code -> {name, long_name, units}}``. Malformed lines are skipped.
    """
    table: dict[int, dict] = {}
    try:
        lines = codes_path.read_text().splitlines()
    except OSError as exc:
        logger.warning("cannot read .codes file {}: {}", codes_path, exc)
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


def _indicators_present(grib_path: UPath) -> list[int]:
    """The distinct ``indicatorOfParameter`` values across the file's messages.

    In file order, de-duplicated -- this is the real set of parameters cfgrib
    collapsed into ``unknown``.
    """
    import eccodes

    indicators: list[int] = []
    seen: set[int] = set()
    with grib_path.open("rb") as handle:
        while True:
            gid = eccodes.codes_grib_new_from_file(handle)
            if gid is None:
                break
            try:
                indicator = eccodes.codes_get(gid, "indicatorOfParameter")
                if indicator not in seen:
                    seen.add(indicator)
                    indicators.append(indicator)
            except Exception:  # noqa: BLE001 -- a message without the key is skipped
                pass
            finally:
                eccodes.codes_release(gid)
    return indicators


def enrich(path: UPath, metadata: FileMetadata, datasets: list) -> FileMetadata:
    """Replace cfgrib's ``unknown`` ECHAM variables with their real names."""
    variables = metadata.get("variables", [])
    if not any(v.get("name") in _UNKNOWN for v in variables):
        return metadata  # nothing collapsed -> not the ECHAM case

    codes_file = _codes_path(path)
    if codes_file is None:
        return metadata
    table = _parse_codes(codes_file)
    if not table:
        return metadata

    try:
        indicators = _indicators_present(path)
    except ImportError:
        logger.warning(
            "{}: ECHAM .codes present but eccodes missing; leaving GRIB names as-is",
            path,
        )
        return metadata
    except Exception as exc:  # noqa: BLE001 -- enrichment is best-effort
        logger.warning("{}: ECHAM indicator scan failed: {}", path, exc)
        return metadata

    # The collapsed 'unknown' carries the shared grid/time dims; reuse them.
    template = next((v for v in variables if v.get("name") in _UNKNOWN), None)
    dims = list(template.get("dimensions", [])) if template else []

    named: list[ScannedVariable] = []
    for indicator in indicators:
        info = table.get(indicator)
        if info is None:
            continue
        entry: ScannedVariable = {
            "name": info["name"],
            "units": info["units"],
            "long_name": info["long_name"],
            "dimensions": dims,
        }
        named.append(entry)
    if not named:
        return metadata

    kept = [v for v in variables if v.get("name") not in _UNKNOWN]
    metadata["variables"] = kept + named
    metadata["variable"] = named[0]["name"]
    return metadata


register_enricher(enrich)
