"""Bridges esm_catalog's real scan-sourcing layer into game data structures.

Nothing here re-derives what belongs in the catalog -- that would defeat the
point of gamifying the *real* tool. :func:`load` simply calls
:func:`esm_catalog.scan.sourcing.source_experiment` and
:func:`esm_catalog.scan.sourcing.source_files` against a real experiment root
and reshapes their result into the flat, path-keyed lookup the game plays
against.
"""

from __future__ import annotations

import random
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

from upath import UPath

from esm_catalog.scan.sourcing import SourcingError, source_experiment, source_files

__all__ = ["SourcingError", "CatalogTarget", "ExperimentCatalog", "load"]


@dataclass(frozen=True)
class CatalogTarget:
    """One real file esm_catalog's sourcing layer says belongs in the catalog.

    ``component`` and ``stream`` are exactly what a real ``esm-catalog scan``
    would record on the file's STAC Item -- the classification the player is
    asked to reconstruct by eye.
    """

    path: Path
    component: str
    stream: str
    role: str  # "data" | "restart"

    @property
    def label(self) -> str:
        """The player-facing classification string, e.g. 'fesom / fesom_temp'."""
        if self.role == "restart":
            return f"{self.component} / restart"
        return f"{self.component} / {self.stream}"


@dataclass(frozen=True)
class ExperimentCatalog:
    """The ground truth for one experiment, as seen by esm_catalog's sourcing layer."""

    experiment_id: str
    root: Path
    components: tuple[str, ...]
    targets_by_path: dict[Path, CatalogTarget] = field(default_factory=dict)

    @property
    def total(self) -> int:
        return len(self.targets_by_path)

    def target_for(self, path: Path) -> Optional[CatalogTarget]:
        return self.targets_by_path.get(path)

    def has_uncaptured_under(self, dir_path: Path, captured: set[Path]) -> bool:
        """Whether any catalog target under *dir_path* has not yet been captured
        -- used to draw a wayfinding marker on unexplored directories."""
        for path in self.targets_by_path:
            if path not in captured and dir_path in path.parents:
                return True
        return False

    def distractor_labels(self, correct: CatalogTarget, count: int = 3) -> list[str]:
        """*count* plausible-but-wrong classification labels, drawn from this
        experiment's own real components/streams -- never invented strings, so a
        wrong choice is always a real mix-up (e.g. the right stream, wrong
        component) rather than an obviously fake option.
        """
        pool = {t.label for t in self.targets_by_path.values() if t.label != correct.label}
        pool.discard(correct.label)
        return random.sample(sorted(pool), k=min(count, len(pool)))


def load(exp_root: Path) -> ExperimentCatalog:
    """Run esm_catalog's real sourcing layer against *exp_root*.

    Raises :class:`SourcingError` (re-exported from
    ``esm_catalog.scan.sourcing``) under exactly the conditions
    ``esm-catalog scan`` itself would refuse to run -- e.g. no
    ``finished_config`` under ``<exp_root>/config``.
    """
    resolved = exp_root.resolve()
    uroot = UPath(str(resolved))
    metadata = source_experiment(uroot)
    files = source_files(uroot)

    targets: dict[Path, CatalogTarget] = {}
    for output_file in files:
        path = Path(str(output_file.path)).resolve()
        targets[path] = CatalogTarget(
            path=path,
            component=output_file.component,
            stream=output_file.stream or "(undeclared)",
            role=output_file.role,
        )

    return ExperimentCatalog(
        experiment_id=metadata.experiment_id,
        root=resolved,
        components=tuple(metadata.components),
        targets_by_path=targets,
    )
