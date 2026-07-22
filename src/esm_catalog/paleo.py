"""Paleoclimate STAC extension: geological time and experiment classification.

Adds properties for paleoclimate simulations where model years represent
geological time periods (e.g., Last Glacial Maximum at 20,000 years ago,
Eocene at 50 million years ago), plus a coarse experiment_type
classification for every item.

Properties added by add_paleo_extension (paleo simulations only):
    paleo:year           - Geological year (negative = past, e.g., -20000)
    paleo:display        - Human-readable format (e.g., "22.0 ka", "66.0 Ma")
    paleo:reference_year - Reference year for "years ago" calculation
    paleo:epoch          - Optional geological epoch (e.g., "Pleistocene")
    paleo:period         - Optional geological period (e.g., "Quaternary")

Properties added by add_experiment_type (every item):
    experiment_type      - "paleo" | "control" | "historical"
    paleo:years_bp       - Years before present (present = 1950 CE);
                           only when experiment_type is "paleo".

The paleo configuration is the ``general.paleo`` section of the ESM-Tools
config, passed down as a plain dict (see CollectionContext.paleo_config)::

    reference_year: -20000  # LGM
    epoch: "Pleistocene"
    period: "Quaternary"
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Optional

from esm_catalog.registry import EXTENSION_URLS

if TYPE_CHECKING:
    import pystac


def add_paleo_extension(
    item: "pystac.Item",
    paleo_config: Optional[dict] = None,
    paleo_year: Optional[int] = None,
    reference_year: int = 2024,
) -> None:
    """Inject paleoclimate extension fields into *item*.

    The geological year is determined in priority order:
    1. Explicit *paleo_year* parameter
    2. ``reference_year`` from *paleo_config*
    3. Derived from item start_datetime/datetime if the year is extreme
       (< 0 or > 9999; such values only arrive as pre-formatted ISO strings)

    No-op when none of these yield a year (not a paleo simulation).
    """
    geo_year = _resolve_paleo_year(item, paleo_config, paleo_year)
    if geo_year is None:
        return

    item.properties["paleo:year"] = geo_year
    item.properties["paleo:display"] = _format_geological(geo_year, reference_year)
    item.properties["paleo:reference_year"] = reference_year

    if paleo_config:
        # Gate on truthiness, not presence: a bare `epoch:`/`period:` in YAML is
        # present-but-None and would otherwise write a schema-invalid null.
        if paleo_config.get("epoch"):
            item.properties["paleo:epoch"] = paleo_config["epoch"]
        if paleo_config.get("period"):
            item.properties["paleo:period"] = paleo_config["period"]

    _register(item)


def add_experiment_type(item: "pystac.Item") -> None:
    """Derive experiment_type (and paleo:years_bp for paleo runs) for *item*.

    Classification by start year:
    - year < 1800  → "paleo"      (deep-time or pre-industrial paleo)
    - 1800-1950    → "control"    (pre-industrial control / spinup)
    - year > 1950  → "historical"
    - unknown      → "control"

    Start year priority: the paleo:year property (set by add_paleo_extension
    for explicitly configured paleo runs, where the model calendar may be
    meaningless), then the item's own start_datetime/datetime.

    paleo:years_bp = 1950 - start_year, added only for "paleo" items.
    """
    start_year = item.properties.get("paleo:year")
    if start_year is None:
        start_year = _resolve_start_year(item)

    if start_year is None:
        item.properties["experiment_type"] = "control"
        return

    if start_year < 1800:
        exp_type = "paleo"
    elif start_year <= 1950:
        exp_type = "control"
    else:
        exp_type = "historical"

    item.properties["experiment_type"] = exp_type
    if exp_type == "paleo":
        item.properties["paleo:years_bp"] = 1950 - start_year
        _register(item)


def _register(item: "pystac.Item") -> None:
    url = EXTENSION_URLS["paleo"]
    if url not in item.stac_extensions:
        item.stac_extensions.append(url)


def _resolve_paleo_year(
    item: "pystac.Item",
    paleo_config: Optional[dict],
    explicit_year: Optional[int],
) -> Optional[int]:
    """Determine the geological year for an item, or None if not paleo."""
    if explicit_year is not None:
        return explicit_year

    if paleo_config and "reference_year" in paleo_config:
        return paleo_config["reference_year"]

    year = _resolve_start_year(item)
    if year is not None and (year < 0 or year > 9999):
        return year
    return None


def _resolve_start_year(item: "pystac.Item") -> Optional[int]:
    """Return the item's start year from its datetime properties.

    Checks the start_datetime/datetime property strings first (these can
    carry deep-time years that datetime objects cannot represent), then the
    item.datetime object.
    """
    dt_str = item.properties.get("start_datetime") or item.properties.get("datetime")
    if dt_str:
        year = _parse_year_from_iso(dt_str)
        if year is not None:
            return year
    if item.datetime is not None:
        return item.datetime.year
    return None


def _parse_year_from_iso(dt_str: str) -> Optional[int]:
    """Parse the year from an ISO datetime string, handling large/negative years.

    Standard datetime.fromisoformat() cannot handle years < 1 or > 9999.
    """
    if not dt_str:
        return None

    negative = dt_str.startswith("-")
    body = dt_str[1:] if negative else dt_str
    year_part = body.split("-", 1)[0]
    try:
        year = int(year_part)
    except ValueError:
        return None
    return -year if negative else year


def _format_geological(year: int, reference_year: int = 2024) -> str:
    """Format a geological year as a human-readable string.

    Uses Ma (millions of years ago) for dates >= 1 million years ago,
    ka (thousands of years ago) for dates >= 10,000 years ago,
    and CE/BCE for more recent dates.
    """
    years_ago = reference_year - year

    if abs(years_ago) >= 1_000_000:
        return f"{years_ago / 1_000_000:.1f} Ma"
    if abs(years_ago) >= 10_000:
        return f"{years_ago / 1_000:.1f} ka"
    if year <= 0:
        # BCE dates (astronomical year numbering: year 0 = 1 BCE)
        return f"{1 - year} BCE"
    return f"{year} CE"
