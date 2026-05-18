"""Paleoclimate STAC extension: geological time representation.

Adds properties for paleoclimate simulations where model years represent
geological time periods (e.g., Last Glacial Maximum at 20,000 years ago,
Eocene at 50 million years ago).

The paleo extension maps model datetime to geological years and provides
human-readable formatting (e.g., "20.0 ka", "66.0 Ma").

Properties added:
    paleo:year          - Geological year (negative = BCE/past, e.g., -20000)
    paleo:display       - Human-readable format (e.g., "20.0 ka", "66.0 Ma")
    paleo:reference_year - Reference year for "years ago" calculation
    paleo:epoch         - Optional geological epoch (e.g., "Pleistocene")
    paleo:period        - Optional geological period (e.g., "Quaternary")

Configuration:
    The paleo offset can be specified in ESM-Tools config:

        general:
            paleo:
                reference_year: -20000  # LGM
                epoch: "Pleistocene"

    Or passed directly to add_paleo_extension().
"""

from __future__ import annotations

from datetime import datetime
from typing import TYPE_CHECKING

from loguru import logger

from esm_catalog.stac.extensions.registry import EXTENSION_URLS

if TYPE_CHECKING:
    pass


def add_paleo_extension(
    item: dict,
    config: dict | None = None,
    paleo_year: int | None = None,
    reference_year: int = 2024,
) -> dict:
    """Inject paleoclimate extension fields into a STAC item.

    The geological year can be determined in three ways (in priority order):
    1. Explicit `paleo_year` parameter
    2. ESM-Tools config `general.paleo.reference_year`
    3. Derived from item datetime if year < 0 or year > 9999

    Args:
        item: STAC item dict to modify.
        config: Optional ESM-Tools config dict.
        paleo_year: Explicit geological year (overrides config).
        reference_year: Reference year for "years ago" calculation.

    Returns:
        Modified item dict with paleo properties.
    """
    props = item["properties"]

    # Determine the geological year for full paleo extension fields
    geo_year = _resolve_paleo_year(item, config, paleo_year)

    if geo_year is None:
        # Not an explicitly configured paleoclimate simulation
        return item

    # Add full paleo properties
    props["paleo:year"] = geo_year
    props["paleo:display"] = _format_geological(geo_year, reference_year)
    props["paleo:reference_year"] = reference_year

    # Add epoch/period from config if available
    paleo_config = _get_paleo_config(config)
    if paleo_config:
        if "epoch" in paleo_config:
            props["paleo:epoch"] = paleo_config["epoch"]
        if "period" in paleo_config:
            props["paleo:period"] = paleo_config["period"]

    # Register extension
    url = EXTENSION_URLS.get("paleo")
    if url and url not in item.get("stac_extensions", []):
        item.setdefault("stac_extensions", []).append(url)

    logger.debug(
        "Added paleo extension: year={}, display={}",
        geo_year,
        props["paleo:display"],
    )

    return item


def _resolve_paleo_year(
    item: dict,
    config: dict | None,
    explicit_year: int | None,
) -> int | None:
    """Determine the geological year for an item.

    Priority:
    1. Explicit paleo_year parameter
    2. Config general.paleo.reference_year
    3. Item datetime if year is extreme (< 0 or > 9999)
    """
    # 1. Explicit parameter
    if explicit_year is not None:
        return explicit_year

    # 2. Config-based
    paleo_config = _get_paleo_config(config)
    if paleo_config and "reference_year" in paleo_config:
        return paleo_config["reference_year"]

    # 3. From item datetime (only if extreme year)
    props = item.get("properties", {})
    dt_str = props.get("datetime") or props.get("start_datetime")
    if dt_str:
        try:
            # Handle ISO format with potentially large/negative years
            year = _parse_year_from_iso(dt_str)
            if year is not None and (year < 0 or year > 9999):
                return year
        except Exception:
            pass

    return None


def _get_paleo_config(config: dict | None) -> dict | None:
    """Extract paleo configuration from ESM-Tools config."""
    if not config:
        return None
    general = config.get("general", {})
    return general.get("paleo")


def _parse_year_from_iso(dt_str: str) -> int | None:
    """Parse year from ISO datetime string, handling large/negative years.

    Standard datetime.fromisoformat() doesn't handle years < 1 or > 9999.
    """
    if not dt_str:
        return None

    # Handle negative years: "-65000000-01-01T00:00:00"
    if dt_str.startswith("-"):
        parts = dt_str[1:].split("-", 1)
        if parts:
            try:
                return -int(parts[0])
            except ValueError:
                pass
    else:
        parts = dt_str.split("-", 1)
        if parts:
            try:
                return int(parts[0])
            except ValueError:
                pass

    return None


def _format_geological(year: int, reference_year: int = 2024) -> str:
    """Format a geological year as human-readable string.

    Uses Ma (millions of years ago) for dates > 1 million years ago,
    ka (thousands of years ago) for dates > 10,000 years ago,
    and CE/BCE for more recent dates.

    Args:
        year: Geological year (negative = past).
        reference_year: Reference year for "years ago" calculation.

    Returns:
        Formatted string like "66.0 Ma", "20.0 ka", "500 BCE", "1492 CE".
    """
    years_ago = reference_year - year

    if abs(years_ago) >= 1_000_000:
        ma = years_ago / 1_000_000
        return f"{ma:.1f} Ma"
    elif abs(years_ago) >= 10_000:
        ka = years_ago / 1_000
        return f"{ka:.1f} ka"
    elif year <= 0:
        # BCE dates (astronomical year numbering: year 0 = 1 BCE)
        bce_year = 1 - year
        return f"{bce_year} BCE"
    else:
        return f"{year} CE"


def detect_paleo_simulation(config: dict | None) -> bool:
    """Check if an experiment is a paleoclimate simulation.

    Looks for paleo configuration in the ESM-Tools config.

    Args:
        config: ESM-Tools config dict.

    Returns:
        True if this appears to be a paleoclimate simulation.
    """
    paleo_config = _get_paleo_config(config)
    return paleo_config is not None and "reference_year" in paleo_config
