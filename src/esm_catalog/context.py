"""Shared CollectionContext value object.

Relocated from esm_catalog.scan.context so that the STAC model (stac/) can use
it without importing the scan layer. The scan layer now imports it from here
and is responsible for populating `namelists_by_component` before STAC items
are built (this is what breaks the former scan<->stac import cycle).
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional


@dataclass
class CollectionContext:
    """Identity + pre-scanned context for building a collection's items.

    Attributes:
        experiment_id: Experiment name (e.g. "exp-alpha").
        component: Model component for the current scan (e.g. "echam").
        collection_id: STAC collection id (Option A: == experiment_id).
        experiment_path: Optional path to the experiment root.
        namelists_by_component: Pre-scanned namelists, mapping
            component name -> {filename -> {group -> {key -> value}}}.
            Populated by the scan layer; the STAC model only reads it.
    """

    experiment_id: str
    component: str
    collection_id: str
    experiment_path: Optional[Path] = None
    namelists_by_component: dict = field(default_factory=dict)
