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

# In-memory database for paleo presets (survives API lifetime)
_presets_db = None

REFERENCE_YEAR: int = 1950


@dataclass(frozen=True)
class PaleoDatetimePreset:
    id: str
    name: str
    display: str
    _years_bp: float
    description: str
    user_added: bool = False

    @property
    def years_bp(self) -> float:
        """Years before present (1950 CE)."""
        return self._years_bp

    @classmethod
    def from_dict(cls, data: dict) -> "PaleoDatetimePreset":
        return cls(
            id=data["id"],
            name=data["name"],
            display=data["display"],
            _years_bp=float(data["years_bp"]),
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


def _get_db():
    """Get or create the in-memory database connection."""
    global _presets_db
    if _presets_db is None:
        _presets_db = duckdb.connect(":memory:")
        _init_presets_db(_presets_db)
    return _presets_db


def _init_presets_db(db) -> None:
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
    # years_bp is "years before present" (1950 CE); positive = past
    defaults = [
        ("lgm",           "Last Glacial Maximum",           "21.0 ka",   21_000,       "Peak ice extent ~21,000 years ago (MIS 2)"),
        ("mid_holocene",  "Mid-Holocene",                   "6.0 ka",    6_000,        "Warm period ~6,000 years ago (MIS 1)"),
        ("eemian",        "Last Interglacial (Eemian)",     "125.0 ka",  125_000,      "Previous warm period, MIS 5e"),
        ("lig",           "Last Interglacial",              "130.0 ka",  130_000,      "MIS 5e warm period, ~130 ka"),
        ("mis3",          "MIS 3",                          "50.0 ka",   50_000,       "Marine Isotope Stage 3, interstadial period"),
        ("pliocene",      "Mid-Pliocene Warm Period",       "3.0 Ma",    3_000_000,    "Warm Pliocene period, ~3 million years ago"),
        ("miocene",       "Late Miocene",                   "10.0 Ma",   10_000_000,   "Late Miocene, ~10 million years ago"),
        ("preindustrial", "Pre-Industrial",                 "1850 CE",   100,          "Pre-industrial baseline (1850 CE)"),
        ("historical",    "Historical Period",              "1850-2014 CE", -32,       "CMIP6 historical period"),
    ]

    for (pid, pname, pdisplay, pyears_bp, pdesc) in defaults:
        db.execute(
            """
            INSERT OR IGNORE INTO paleo_presets (id, name, display, years_bp, description, user_added)
            VALUES (?, ?, ?, ?, ?, FALSE)
            """,
            [pid, pname, pdisplay, pyears_bp, pdesc],
        )


def _row_to_preset(r) -> PaleoDatetimePreset:
    return PaleoDatetimePreset.from_dict({
        "id": r[0], "name": r[1], "display": r[2],
        "years_bp": r[3], "description": r[4], "user_added": r[5],
    })


def get_presets() -> list[PaleoDatetimePreset]:
    """Return all paleo presets sorted by age (oldest first)."""
    db = _get_db()
    rows = db.execute(
        "SELECT id, name, display, years_bp, description, user_added "
        "FROM paleo_presets ORDER BY years_bp DESC"
    ).fetchall()
    return [_row_to_preset(r) for r in rows]


def get_preset(preset_id: str) -> PaleoDatetimePreset | None:
    """Get a single preset by ID."""
    db = _get_db()
    rows = db.execute(
        "SELECT id, name, display, years_bp, description, user_added "
        "FROM paleo_presets WHERE id = ?",
        [preset_id],
    ).fetchall()
    return _row_to_preset(rows[0]) if rows else None


def add_preset(
    preset_id: str,
    name: str,
    display: str,
    years_bp: float,
    description: str = "",
) -> PaleoDatetimePreset:
    """Add a user-defined preset."""
    db = _get_db()
    db.execute(
        "INSERT OR REPLACE INTO paleo_presets "
        "(id, name, display, years_bp, description, user_added) "
        "VALUES (?, ?, ?, ?, ?, TRUE)",
        [preset_id, name, display, years_bp, description],
    )
    return PaleoDatetimePreset.from_dict({
        "id": preset_id, "name": name, "display": display,
        "years_bp": years_bp, "description": description, "user_added": True,
    })


def delete_preset(preset_id: str) -> bool:
    """Delete a user-added preset. Returns True if deleted, False if not found/built-in."""
    db = _get_db()
    row = db.execute(
        "SELECT id FROM paleo_presets WHERE id = ? AND user_added = TRUE",
        [preset_id],
    ).fetchone()
    if not row:
        return False
    db.execute(
        "DELETE FROM paleo_presets WHERE id = ? AND user_added = TRUE",
        [preset_id],
    )
    return True


def years_bp_to_datetime(years_bp: float) -> str:
    """Convert years before present to ISO datetime string."""
    year = 1950 - int(years_bp)
    if year < 1:
        return f"{year:05d}-01-01T00:00:00Z"
    return f"{year:04d}-01-01T00:00:00Z"


def datetime_to_years_bp(datetime_str: str) -> float | None:
    """Convert ISO datetime string to years before present."""
    try:
        year_str = datetime_str[:5] if datetime_str.startswith("-") else datetime_str[:4]
        return float(1950 - int(year_str))
    except (ValueError, IndexError):
        return None
