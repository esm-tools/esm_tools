from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path


@dataclass
class CollectionContext:
    """Identity + pre-scanned context for building a collection's items."""

    experiment_id: str
    component: str
    collection_id: str
    experiment_path: Path | None = None
    namelists_by_component: dict = field(default_factory=dict)
