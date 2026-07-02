from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import ClassVar

from esm_tools.error_handling import user_error


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
        if self.production:
            self._check_production_fields()

    def _check_production_fields(self):
        missing = [f for f in self.PRODUCTION_REQUIRED if getattr(self, f) in (None, "")]
        if missing:
            user_error(
                "Production context",
                f"The following fields are required for production and cannot be empty: {', '.join(missing)}",
            )
