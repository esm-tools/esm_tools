"""In-memory DuckDB storage for paleo time period presets.

Provides commonly simulated paleo time periods (LGM, Mid-Holocene, etc.)
that users can reference when filtering climate model output.

The presets are stored in an in-memory DuckDB table that persists for
the lifetime of the API server. Users can add custom presets via the API.
"""

from __future__ import annotations

from dataclasses import dataclass

try:
    import duckdb
except ImportError:
    duckdb = None  # type: ignore[assignment]
from paleodatetime import PaleoDateTime

# In-memory database for paleo presets (survives API lifetime)
_presets_db: duckdb.DuckDBPyConnection | None = None

REFERENCE_YEAR: int = 1950


@dataclass(frozen=True)
class PaleoDatetimePreset:
    id: str
    name: str
    display: str
    value: PaleoDateTime
    description: str
    user_added: bool = False

    @property
    def years_bp(self) -> float:
        """Years before present (1950 CE), derived from the PaleoDateTime value."""
        return float(-self.value.year)

    @classmethod
    def from_dict(cls, data: dict) -> "PaleoDatetimePreset":
        return cls(
            id=data["id"],
            name=data["name"],
            display=data["display"],
            value=PaleoDateTime(year=-data["years_bp"]),
            description=data["description"],
            user_added=data.get("user_added", False),
        )

    def to_dict(self) -> dict:
        """Serialize to a JSON-safe dict for API responses."""
        return {
            "id": self.id,
            "name": self.name,
            "display": self.display,
            "years_bp": self.years_bp,
            "description": self.description,
            "user_added": self.user_added,
        }


def _get_db() -> duckdb.DuckDBPyConnection:
    """Get or create the in-memory database connection."""
    global _presets_db
    if _presets_db is None:
        _presets_db = duckdb.connect(":memory:")
        _init_presets_db(_presets_db)
    return _presets_db


def _init_presets_db(db: duckdb.DuckDBPyConnection) -> None:
    """Initialize paleo time presets table with common periods."""
    db.execute(
        """
        CREATE TABLE IF NOT EXISTS paleo_presets (
            id TEXT PRIMARY KEY,
            name TEXT NOT NULL,
            display TEXT NOT NULL,
            years_bp DOUBLE,
            description TEXT,
            user_added BOOLEAN DEFAULT FALSE
        )
    """
    )

    # Default presets (commonly simulated periods in climate modeling)
    # Note: years_bp is "years before present" (1950 CE)
    # Negative values indicate CE years after 1950
    defaults = [
        PaleoDatetimePreset(
            id="lgm",
            name="Last Glacial Maximum",
            display="21.0 ka",
            value=PaleoDateTime(year=-21_000),
            description="Peak ice extent ~21,000 years ago (MIS 2)",
        ),
        PaleoDatetimePreset(
            id="mid_holocene",
            name="Mid-Holocene",
            display="6.0 ka",
            value=PaleoDateTime(year=-6_000),
            description="Warm period ~6,000 years ago (MIS 1)",
        ),
        PaleoDatetimePreset(
            id="eemian",
            name="Last Interglacial (Eemian)",
            display="125.0 ka",
            value=PaleoDateTime(year=-125_000),
            description="Previous warm period, MIS 5e",
        ),
        PaleoDatetimePreset(
            id="lig",
            name="Last Interglacial",
            display="130.0 ka",
            value=PaleoDateTime(year=-130_000),
            description="MIS 5e warm period, ~130 ka",
        ),
        PaleoDatetimePreset(
            id="mis3",
            name="MIS 3",
            display="50.0 ka",
            value=PaleoDateTime(year=-50_000),
            description="Marine Isotope Stage 3, interstadial period",
        ),
        PaleoDatetimePreset(
            id="pliocene",
            name="Mid-Pliocene Warm Period",
            display="3.0 Ma",
            value=PaleoDateTime(year=-3_000_000),
            description="Warm Pliocene period, ~3 million years ago",
        ),
        PaleoDatetimePreset(
            id="miocene",
            name="Late Miocene",
            display="10.0 Ma",
            value=PaleoDateTime(year=-10_000_000),
            description="Late Miocene, ~10 million years ago",
        ),
        PaleoDatetimePreset(
            id="preindustrial",
            name="Pre-Industrial",
            display="1850 CE",
            value=PaleoDateTime(year=-100),  # ~100 years before 1950
            description="Pre-industrial baseline (1850 CE)",
        ),
        PaleoDatetimePreset(
            id="historical",
            name="Historical Period",
            display="1850-2014 CE",
            value=PaleoDateTime(year=32),  # midpoint ~1982, or -32 years from 1950
            description="CMIP6 historical period",
        ),
    ]

    for preset in defaults:
        db.execute(
            """
            INSERT OR IGNORE INTO paleo_presets (id, name, display, years_bp, description, user_added)
            VALUES (?, ?, ?, ?, ?, FALSE)
            """,
            [preset.id, preset.name, preset.display, preset.years_bp, preset.description],
        )


def get_presets() -> list[PaleoDatetimePreset]:
    """Return all paleo presets sorted by age (oldest first).

    Returns:
        List of preset dicts with keys: id, name, display, years_bp, description, user_added
    """
    db = _get_db()
    rows = db.execute(
        """
        SELECT id, name, display, years_bp, description, user_added
        FROM paleo_presets
        ORDER BY years_bp DESC
        """
    ).fetchall()
    return [
        PaleoDatetimePreset.from_dict(
            {
                "id": r[0],
                "name": r[1],
                "display": r[2],
                "years_bp": r[3],
                "description": r[4],
                "user_added": r[5],
            }
        )
        for r in rows
    ]


def get_preset(preset_id: str) -> PaleoDatetimePreset | None:
    """Get a single preset by ID.

    Args:
        preset_id: The preset identifier

    Returns:
        Preset dict or None if not found
    """
    db = _get_db()
    rows = db.execute(
        """
        SELECT id, name, display, years_bp, description, user_added
        FROM paleo_presets
        WHERE id = ?
        """,
        [preset_id],
    ).fetchall()
    if not rows:
        return None
    r = rows[0]
    return PaleoDatetimePreset.from_dict(
        {
            "id": r[0],
            "name": r[1],
            "display": r[2],
            "years_bp": r[3],
            "description": r[4],
            "user_added": r[5],
        }
    )


def add_preset(
    preset_id: str,
    name: str,
    display: str,
    years_bp: float,
    description: str = "",
) -> PaleoDatetimePreset:
    """Add a user-defined preset.

    Args:
        preset_id: Unique identifier for the preset
        name: Human-readable name
        display: Display string (e.g., "21.0 ka", "6.0 ka", "1850 CE")
        years_bp: Years before present (1950 CE)
        description: Optional description

    Returns:
        The created preset dict
    """
    db = _get_db()
    db.execute(
        """
        INSERT OR REPLACE INTO paleo_presets (id, name, display, years_bp, description, user_added)
        VALUES (?, ?, ?, ?, ?, TRUE)
        """,
        [preset_id, name, display, years_bp, description],
    )
    return PaleoDatetimePreset.from_dict(
        {
            "id": preset_id,
            "name": name,
            "display": display,
            "years_bp": years_bp,
            "description": description,
            "user_added": True,
        }
    )


def delete_preset(preset_id: str) -> bool:
    """Delete a user-added preset.

    Only user-added presets can be deleted. Built-in presets are protected.

    Args:
        preset_id: The preset identifier to delete

    Returns:
        True if deleted, False if not found or not user-added
    """
    db = _get_db()
    result = db.execute(
        """
        DELETE FROM paleo_presets
        WHERE id = ? AND user_added = TRUE
        """,
        [preset_id],
    )
    return result.rowcount > 0 if hasattr(result, "rowcount") else True


def years_bp_to_datetime(years_bp: float) -> str:
    """Convert years before present to ISO datetime string.

    Args:
        years_bp: Years before present (1950 CE)

    Returns:
        ISO datetime string (approximate, year-only precision)
    """
    # 1950 is the "present" in BP convention
    year = 1950 - int(years_bp)
    if year < 1:
        # Handle BCE dates (year 0 doesn't exist in ISO 8601)
        # Use proleptic Gregorian calendar representation
        return f"{year:05d}-01-01T00:00:00Z"
    return f"{year:04d}-01-01T00:00:00Z"


def datetime_to_years_bp(datetime_str: str) -> float | None:
    """Convert ISO datetime string to years before present.

    Args:
        datetime_str: ISO datetime string

    Returns:
        Years before present (1950 CE), or None if parsing fails
    """
    try:
        # Extract year from ISO string
        year_str = (
            datetime_str[:5] if datetime_str.startswith("-") else datetime_str[:4]
        )
        year = int(year_str)
        return 1950 - year
    except (ValueError, IndexError):
        return None
