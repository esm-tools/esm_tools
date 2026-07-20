from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import ClassVar


@dataclass
class CollectionContext:
    """Identity + pre-scanned context for building a collection's items."""

    PRODUCTION_REQUIRED: ClassVar[list[str]] = [
        "description",
        "data_license",
    ]

    experiment_id: str
    component: str
    collection_id: str
    production: bool = False
    description: str | None = None
    data_license: str | None = None
    experiment_path: Path | None = None
    namelists_by_component: dict = field(default_factory=dict)

    def __post_init__(self):
        """Validate production-required fields immediately after construction."""
        if self.production:
            self._check_production_fields()

    def _check_production_fields(self):
        """Raise ValueError if any PRODUCTION_REQUIRED field is empty or None."""
        missing = [
            f for f in self.PRODUCTION_REQUIRED if getattr(self, f) in (None, "")
        ]
        if missing:
            raise ValueError(
                f"Production context requires non-empty fields: {', '.join(missing)}"
            )
