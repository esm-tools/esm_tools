"""Scan ECHAM GRIB files with proper codes table support.

ECHAM output files use non-standard GRIB encoding where paramId=0 and the
actual parameter is stored in indicatorOfParameter. This module handles
that case by reading the companion .codes file and mapping indicators to
variable metadata.

This is separate from generic grib.py because:
1. ECHAM has specific filename conventions (expid_YYYYMM.NN_stream)
2. ECHAM always requires a .codes file for proper variable naming
3. ECHAM uses indicatorOfParameter instead of standard paramId
"""

from __future__ import annotations

import re
from collections import defaultdict
from datetime import datetime, timezone
from pathlib import Path
from typing import TYPE_CHECKING, Union

from loguru import logger

if TYPE_CHECKING:
    from upath import UPath


def _parse_codes_file(codes_path: Path) -> dict[int, dict]:
    """Parse an ECHAM-style .codes file.

    Format: paramId levels shortName offset scale long_name [units]

    Example line:
        130    1 st      0.      1. surface temperature [K]

    Returns:
        Dictionary mapping paramId -> {shortName, levels, longName, units}
    """
    params = {}
    try:
        with open(codes_path) as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith('#'):
                    continue
                parts = line.split()
                if len(parts) < 6:
                    continue
                try:
                    param_id = int(parts[0])
                    levels = int(parts[1])
                    short_name = parts[2]
                    long_name_with_units = " ".join(parts[5:])
                    match = re.match(r"(.+?)\s*\[(.+?)\]$", long_name_with_units)
                    if match:
                        long_name = match.group(1).strip()
                        units = match.group(2)
                    else:
                        long_name = long_name_with_units
                        units = ""
                    params[param_id] = {
                        "shortName": short_name,
                        "levels": levels,
                        "longName": long_name,
                        "units": units,
                    }
                except (ValueError, IndexError):
                    continue
        logger.debug("Loaded {} parameter definitions from {}", len(params), codes_path.name)
    except Exception as e:
        logger.warning("Failed to parse codes file {}: {}", codes_path, e)
    return params


def _extract_stream_type(path: Path) -> str:
    """Extract ECHAM stream type from filename.

    Examples:
        basic-001_185001.01_echam -> echam
        basic-001_185001.01_co2 -> co2
        basic-001_185001.01_accw -> accw
        basic-001_185002.01_echam_18500201-18500228 -> echam

    Note: ECHAM filenames contain dots that aren't extensions (e.g., .01),
    so we use .name not .stem to avoid truncation.
    """
    name = path.name
    # Strip date range suffix if present
    name = re.sub(r"_\d{6,8}-\d{6,8}$", "", name)
    # Get the last part after underscore (stream type)
    parts = name.rsplit("_", 1)
    if len(parts) == 2:
        return parts[1]
    return name


def _find_codes_file(grib_path: Path) -> Path | None:
    """Find the associated .codes file for an ECHAM GRIB file.

    Search order:
    1. {grib_filename}.codes in same directory
    2. {grib_filename_without_date_range}.codes (strips _YYYYMMDD-YYYYMMDD suffix)
    """
    # Try exact match first
    codes_path = grib_path.parent / (grib_path.name + ".codes")
    if codes_path.exists():
        return codes_path

    # Try stripping date range suffix: _YYYYMMDD-YYYYMMDD or _YYYYMM-YYYYMM
    base_name = re.sub(r"_\d{6,8}-\d{6,8}$", "", grib_path.name)
    if base_name != grib_path.name:
        codes_path = grib_path.parent / (base_name + ".codes")
        if codes_path.exists():
            return codes_path

    return None


def _scan_echam_messages(file_path: Path) -> list[dict]:
    """Scan all GRIB messages and extract per-message metadata.

    For ECHAM files, uses indicatorOfParameter as the parameter ID since
    paramId is typically 0.

    Returns:
        List of message metadata dicts with keys:
        - indicator: indicatorOfParameter (used as param ID)
        - grid_type, level_type
        - data_date, data_time
        - ni, nj (grid dimensions)
    """
    import eccodes

    messages = []
    with open(file_path, "rb") as f:
        msg_num = 0
        while True:
            gid = eccodes.codes_grib_new_from_file(f)
            if gid is None:
                break
            msg_num += 1
            try:
                grid_type = eccodes.codes_get(gid, "gridType")
                level_type = eccodes.codes_get(gid, "typeOfLevel")
                param_id = eccodes.codes_get(gid, "paramId")

                # ECHAM uses indicatorOfParameter when paramId=0
                try:
                    indicator = eccodes.codes_get(gid, "indicatorOfParameter")
                except Exception:
                    indicator = param_id  # Fallback to paramId

                # Use indicator as the effective param ID for ECHAM
                effective_id = indicator if param_id == 0 else param_id

                try:
                    data_date = eccodes.codes_get(gid, "dataDate")
                    data_time = eccodes.codes_get(gid, "dataTime")
                except Exception:
                    data_date = data_time = None

                try:
                    ni = eccodes.codes_get(gid, "Ni")
                    nj = eccodes.codes_get(gid, "Nj")
                except Exception:
                    ni = nj = None

                messages.append({
                    "msg_num": msg_num,
                    "indicator": effective_id,
                    "param_id": param_id,
                    "grid_type": grid_type,
                    "level_type": level_type,
                    "data_date": data_date,
                    "data_time": data_time,
                    "ni": ni,
                    "nj": nj,
                })
            finally:
                eccodes.codes_release(gid)

    logger.debug("{}: {} GRIB messages", file_path.name, len(messages))
    return messages


def _build_variables(
    messages: list[dict],
    codes_table: dict[int, dict],
) -> list[dict]:
    """Build variable metadata list from GRIB messages and codes table.

    Each unique indicator becomes a variable, with metadata from the codes table.
    """
    # Group messages by indicator
    by_indicator: dict[int, list[dict]] = defaultdict(list)
    for msg in messages:
        by_indicator[msg["indicator"]].append(msg)

    variables = []
    for indicator in sorted(by_indicator.keys()):
        msg_list = by_indicator[indicator]
        first_msg = msg_list[0]

        # Look up in codes table
        code_info = codes_table.get(indicator)
        if code_info:
            entry = {
                "name": code_info["shortName"],
                "long_name": code_info["longName"],
                "units": code_info["units"],
                "metadata_source": "codes_table",
            }
        else:
            entry = {
                "name": f"var{indicator}",
                "long_name": f"Unknown parameter {indicator}",
                "units": "",
                "metadata_source": "fallback",
            }
            logger.warning("No codes entry for indicator {}", indicator)

        entry["indicator"] = indicator
        entry["grid_type"] = first_msg["grid_type"]
        entry["level_type"] = first_msg["level_type"]
        entry["message_count"] = len(msg_list)

        # Add standard_name mapping if available
        cf_name = _indicator_to_cf(indicator, entry["name"])
        if cf_name:
            entry["standard_name"] = cf_name

        variables.append(entry)

    return variables


def _extract_datetime_range(messages: list[dict]) -> tuple[datetime | None, datetime | None]:
    """Extract datetime range from GRIB messages."""
    dates = []
    for msg in messages:
        data_date = msg.get("data_date")
        data_time = msg.get("data_time")
        if data_date:
            try:
                year = data_date // 10000
                month = (data_date % 10000) // 100
                day = data_date % 100
                hour = (data_time or 0) // 100
                minute = (data_time or 0) % 100
                dt = datetime(year, month, day, hour, minute, tzinfo=timezone.utc)
                dates.append(dt)
            except (ValueError, TypeError):
                pass

    if not dates:
        return None, None
    return min(dates), max(dates)


def _extract_bbox(messages: list[dict]) -> list[float]:
    """Extract bounding box - ECHAM is global."""
    # ECHAM output is typically global
    return [-180.0, -90.0, 180.0, 90.0]


def _bbox_to_polygon(bbox: list) -> dict:
    lon_min, lat_min, lon_max, lat_max = bbox
    return {
        "type": "Polygon",
        "coordinates": [[
            [lon_min, lat_min], [lon_max, lat_min],
            [lon_max, lat_max], [lon_min, lat_max],
            [lon_min, lat_min],
        ]],
    }


def _cf_parameters(variables: list[dict]) -> list[dict]:
    """Build cf:parameter list from variables."""
    params = []
    for v in variables:
        if "standard_name" in v:
            p = {"name": v["standard_name"], "variable": v["name"]}
            if v.get("units"):
                p["unit"] = v["units"]
            params.append(p)
    return params


# Mapping of ECHAM indicator/shortName to CF standard names
_ECHAM_CF_MAP = {
    "st": "surface_temperature",
    "t": "air_temperature",
    "temp2": "air_temperature",
    "tsurf": "surface_temperature",
    "q": "specific_humidity",
    "u": "eastward_wind",
    "v": "northward_wind",
    "aprl": "large_scale_precipitation_flux",
    "aprc": "convective_precipitation_flux",
    "aprs": "stratiform_precipitation_flux",
    "runoff": "surface_runoff_flux",
    "drainage": "subsurface_runoff_flux",
    "snow_melt": "surface_snow_melt_flux",
}


def _indicator_to_cf(indicator: int, short_name: str) -> str | None:
    """Map ECHAM indicator/shortName to CF standard_name."""
    return _ECHAM_CF_MAP.get(short_name.lower())


def is_echam_file(path: Path) -> bool:
    """Check if a file appears to be an ECHAM GRIB file.

    Detection based on:
    1. Has a companion .codes file
    2. Filename matches ECHAM pattern: expid_YYYYMM.NN_stream
    """
    # Check for codes file
    codes_file = _find_codes_file(path)
    if codes_file:
        return True

    # Check filename pattern
    name = path.name
    # Pattern: something_YYYYMM.NN_stream
    if re.match(r".+_\d{6}\.\d{2}_[a-z]+", name, re.IGNORECASE):
        return True

    return False


def scan_echam(path: "Union[Path, UPath, str]") -> dict:
    """Scan an ECHAM GRIB file and return STAC-ready metadata.

    This scanner is specifically designed for ECHAM output which uses:
    - Non-standard paramId=0 encoding
    - indicatorOfParameter for actual parameter identification
    - Companion .codes files for variable metadata

    Args:
        path: Path to ECHAM GRIB file

    Returns:
        Dict with STAC metadata including stream, variables, bbox, datetime, etc.

    Raises:
        ValueError: If no codes file found or no GRIB messages
    """
    if isinstance(path, str):
        from esm_catalog.scan.upath import parse_uri
        path = parse_uri(path)

    # Check for remote files
    if hasattr(path, "protocol") and path.protocol and path.protocol != "file":
        raise ValueError(
            f"ECHAM GRIB scanning requires local file access: {path}\n"
            "Download the file locally first."
        )

    path = Path(path)
    logger.debug("Scanning ECHAM GRIB: {}", path)

    # Find and parse codes file (required for ECHAM)
    codes_file = _find_codes_file(path)
    if not codes_file:
        raise ValueError(
            f"No .codes file found for ECHAM file: {path}\n"
            f"Expected: {path.name}.codes in same directory"
        )

    codes_table = _parse_codes_file(codes_file)
    if not codes_table:
        raise ValueError(f"Empty or invalid codes file: {codes_file}")

    # Scan GRIB messages
    messages = _scan_echam_messages(path)
    if not messages:
        raise ValueError(f"No GRIB messages found in: {path}")

    # Build variable list from messages + codes table
    variables = _build_variables(messages, codes_table)

    # Extract metadata
    dt_start, dt_end = _extract_datetime_range(messages)
    bbox = _extract_bbox(messages)
    stream_type = _extract_stream_type(path)

    return {
        "stream": stream_type,
        "variables": variables,
        "cf_parameters": _cf_parameters(variables),
        "dimensions": {},
        "bbox": bbox,
        "geometry": _bbox_to_polygon(bbox),
        "datetime_start": dt_start,
        "datetime_end": dt_end,
        "datetime_str": dt_start.strftime("%Y%m") if dt_start else "000000",
        "file_size": path.stat().st_size,
        "conventions": "CF-1.6",
        "format": "grib",
        "echam_stream": stream_type,  # Explicit ECHAM stream marker
    }
