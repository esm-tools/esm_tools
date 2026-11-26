"""ECHAM GRIB fast path: read eccodes headers directly, skipping cfgrib.

ECHAM writes GRIB1 with ``paramId=0`` for every field, so generic cfgrib
cannot tell parameters apart without the companion ``<file>.codes`` table
(see ``model_plugins.helpers.grib.echam`` for the table parser, shared with
the enricher in ``model_plugins.enrichers.grib.echam``, which fixes up
cfgrib's output the same way for files this fast path didn't claim).
"""

from __future__ import annotations

from datetime import datetime
from pathlib import Path
from typing import Optional

from loguru import logger
from upath import UPath

from esm_catalog.scan.model_plugins.helpers.grib.echam import codes_path, parse_codes
from esm_catalog.scan.readers.grib.plugins import hookimpl
from esm_catalog.scan.readers.netcdf.coords import _bbox_to_polygon
from esm_catalog.types import FileMetadata


@hookimpl
def try_model_specific_read(
    path: UPath, file_format
) -> Optional[FileMetadata]:
    """Read an ECHAM GRIB1 file straight from eccodes headers, skipping cfgrib.

    Only when a ``.codes`` sidecar exists (the ECHAM case); returns None for
    anything else, or if the header read cannot confidently produce a result
    (e.g. GRIB2, an unsupported grid) -- the generic cfgrib reader is always
    the fallback.
    """
    codes_file = codes_path(path)
    if codes_file is None:
        return None
    try:
        return _read_from_headers(path, codes_file)
    except Exception as exc:  # noqa: BLE001 -- best-effort; cfgrib is the fallback
        logger.debug("{}: ECHAM header fast path failed: {}", path, exc)
        return None


def _read_from_headers(path: UPath, codes_file: Path) -> Optional[FileMetadata]:
    """Build FileMetadata from eccodes headers alone: variable names from the
    ``.codes`` table, geometry and time from GRIB keys, one pass over the
    file's messages.

    GRIB1 only (the ECHAM case); returns None on GRIB2 or an unrecognised
    grid rather than guess. Does not populate ``dimensions``/``frequency`` --
    a scope choice, not an omission: the datacube extension is simply a no-op
    for these items until a follow-up adds it.
    """
    import eccodes

    table = parse_codes(codes_file)
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
                    try:
                        geometry_keys = {
                            key: eccodes.codes_get(gid, key)
                            for key in (
                                "latitudeOfFirstGridPointInDegrees",
                                "longitudeOfFirstGridPointInDegrees",
                                "latitudeOfLastGridPointInDegrees",
                                "longitudeOfLastGridPointInDegrees",
                            )
                        }
                    except Exception:  # noqa: BLE001 -- confirmed live: ECHAM
                        # mixes spectral ('sh') messages with gridpoint ones in
                        # the same file; spectral messages carry no lat/lon
                        # geometry at all. One such message must not abort the
                        # whole fast path (it did: fell back to cfgrib's slow
                        # generic path for a 543 MB file) -- just try the next
                        # message, which may be a gridpoint one.
                        pass
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
