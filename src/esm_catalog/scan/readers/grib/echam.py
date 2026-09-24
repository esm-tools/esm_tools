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
from datetime import datetime
from pathlib import Path
from typing import Optional

from loguru import logger
from upath import UPath

from esm_catalog.scan.enrichers import hookimpl as enricher_hookimpl
from esm_catalog.scan.format import FileFormat
from esm_catalog.scan.readers.grib.plugins import hookimpl
from esm_catalog.scan.readers.netcdf.coords import _bbox_to_polygon
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


@enricher_hookimpl
def enrich_metadata(
    path: UPath, file_format: FileFormat, metadata: FileMetadata, datasets: list
) -> None:
    if file_format != FileFormat.grib:
        return
    enrich(path, metadata, datasets)


@hookimpl
def try_model_specific_read(
    path: UPath, file_format: FileFormat
) -> Optional[FileMetadata]:
    """Read an ECHAM GRIB1 file straight from eccodes headers, skipping cfgrib.

    Only when a ``.codes`` sidecar exists (the ECHAM case); returns None for
    anything else, or if the header read cannot confidently produce a result
    (e.g. GRIB2, an unsupported grid) -- the generic cfgrib reader is always
    the fallback.
    """
    codes_path = _codes_path(path)
    if codes_path is None:
        return None
    try:
        return _read_from_headers(path, codes_path)
    except Exception as exc:  # noqa: BLE001 -- best-effort; cfgrib is the fallback
        logger.debug("{}: ECHAM header fast path failed: {}", path, exc)
        return None


def _read_from_headers(path: UPath, codes_path: Path) -> Optional[FileMetadata]:
    """Build FileMetadata from eccodes headers alone: variable names from the
    ``.codes`` table, geometry and time from GRIB keys, one pass over the
    file's messages.

    GRIB1 only (the ECHAM case); returns None on GRIB2 or an unrecognised
    grid rather than guess. Does not populate ``dimensions``/``frequency`` --
    a scope choice, not an omission: the datacube extension is simply a no-op
    for these items until a follow-up adds it.
    """
    import eccodes

    table = _parse_codes(codes_path)
    if not table:
        return None

    indicators: list[int] = []
    seen: set[int] = set()
    dates: set[tuple[int, int]] = set()
    geometry_keys: Optional[dict] = None

    with path.open("rb") as handle:
        while True:
            gid = eccodes.codes_grib_new_from_file(handle)
            if gid is None:
                break
            try:
                if eccodes.codes_get(gid, "edition") != 1:
                    return None
                indicator = eccodes.codes_get(gid, "indicatorOfParameter")
                if indicator not in seen:
                    seen.add(indicator)
                    indicators.append(indicator)
                dates.add(
                    (
                        eccodes.codes_get(gid, "dataDate"),
                        eccodes.codes_get(gid, "dataTime"),
                    )
                )
                if geometry_keys is None:
                    geometry_keys = {
                        key: eccodes.codes_get(gid, key)
                        for key in (
                            "latitudeOfFirstGridPointInDegrees",
                            "longitudeOfFirstGridPointInDegrees",
                            "latitudeOfLastGridPointInDegrees",
                            "longitudeOfLastGridPointInDegrees",
                        )
                    }
            finally:
                eccodes.codes_release(gid)

    if not indicators or geometry_keys is None or not dates:
        return None

    named: list[dict] = []
    for indicator in indicators:
        info = table.get(indicator)
        if info is None:
            continue
        named.append(
            {
                "name": info["name"],
                "units": info["units"],
                "long_name": info["long_name"],
            }
        )
    if not named:
        return None

    bbox = _grib_bbox(geometry_keys)
    dt_start = _grib_datetime(*min(dates))
    dt_end = _grib_datetime(*max(dates))

    return {
        "variable": named[0]["name"],
        "variables": named,
        "format": "grib",
        "bbox": bbox,
        "geometry": _bbox_to_polygon(bbox),
        "datetime_start": dt_start,
        "datetime_end": dt_end,
        "datetime_str": dt_start.strftime("%Y%m"),
    }


def _grib_bbox(geometry_keys: dict) -> list[float]:
    """The ``[west, south, east, north]`` bbox from GRIB grid-definition keys."""
    lat_min = min(
        geometry_keys["latitudeOfFirstGridPointInDegrees"],
        geometry_keys["latitudeOfLastGridPointInDegrees"],
    )
    lat_max = max(
        geometry_keys["latitudeOfFirstGridPointInDegrees"],
        geometry_keys["latitudeOfLastGridPointInDegrees"],
    )
    # GRIB often uses a 0..360 longitude convention; fold to -180..180.
    lon_min = geometry_keys["longitudeOfFirstGridPointInDegrees"]
    lon_max = geometry_keys["longitudeOfLastGridPointInDegrees"]
    if lon_min > 180.0:
        lon_min -= 360.0
    if lon_max > 180.0:
        lon_max -= 360.0
    if lon_min > lon_max:
        lon_min, lon_max = lon_max, lon_min
    return [lon_min, lat_min, lon_max, lat_max]


def _grib_datetime(data_date: int, data_time: int) -> datetime:
    """Parse GRIB's ``dataDate`` (``YYYYMMDD``) + ``dataTime`` (``HHMM``)."""
    hour, minute = divmod(data_time, 100)
    return datetime.strptime(f"{data_date:08d}", "%Y%m%d").replace(
        hour=hour, minute=minute
    )
