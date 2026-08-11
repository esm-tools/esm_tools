"""The CLI workspace state at ``<exp_root>/catalog/esm-catalog.json``.

``scanned`` maps a time-varying file's path to the md5 it was catalogued at, so
a re-scan skips it when unchanged. Time-invariant (fx) files are deliberately
never recorded here — they are re-read every scan (cheap, there are few) because
their datetime is the experiment run span, which grows when the run is extended,
so ``fx.parquet`` is rewritten each scan.

This is CLI bookkeeping, not published STAC — ``push`` skips it.
"""

from __future__ import annotations

import json
from typing import Optional

from pydantic import BaseModel
from upath import UPath

from esm_catalog.scan.types import Md5
from esm_catalog.types import ExperimentId

CATALOG_DIRNAME = "catalog"
"""The catalog subdirectory created under an experiment root."""

STATE_FILENAME = "esm-catalog.json"
"""The workspace-state file inside the catalog directory."""

ScannedPath = str
"""A scanned output file's path (as text) — the key its catalogued md5 is stored under."""


class WorkspaceState(BaseModel):
    """The persisted ``esm-catalog.json`` — init config plus scan bookkeeping."""

    experiment_id: ExperimentId
    server_url: Optional[str] = None
    scanned: dict[ScannedPath, Md5] = {}


def catalog_dir(exp_root: UPath) -> UPath:
    """The catalog directory for *exp_root* (``<exp_root>/catalog``)."""
    return exp_root / CATALOG_DIRNAME


def state_path(catalog: UPath) -> UPath:
    """The workspace-state path (``<catalog>/esm-catalog.json``)."""
    return catalog / STATE_FILENAME


def load_state(catalog: UPath) -> Optional[WorkspaceState]:
    """Load the workspace state, or None if the experiment has not been inited/scanned."""
    path = state_path(catalog)
    if not path.exists():
        return None
    return WorkspaceState.model_validate_json(path.read_text())


def save_state(catalog: UPath, state: WorkspaceState) -> None:
    """Write *state* to ``<catalog>/esm-catalog.json`` (creating the dir)."""
    catalog.mkdir(parents=True, exist_ok=True)
    state_path(catalog).write_text(state.model_dump_json(indent=2))
