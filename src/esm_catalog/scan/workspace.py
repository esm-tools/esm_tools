"""The CLI workspace state at ``<exp_root>/catalog/esm-catalog.json``.

``scanned`` maps a time-varying file's path to the md5 it was catalogued at, so
a re-scan skips it when unchanged. Time-invariant (fx) files are deliberately
never recorded here — they are re-read every scan (cheap, there are few) because
their datetime is the experiment run span, which grows when the run is extended,
so ``fx.parquet`` is rewritten each scan.

``schema_by_stream`` persists a ``(component, stream)``'s frozen schema
*across* scans, not just within one -- confirmed live: re-deriving the same
handful of streams' variables/dims/geometry/format on every single incremental
re-scan of a growing experiment is pure waste once it was already established
once. Populated the first time any scan actually reads a stream's schema;
consulted by every later scan (and by the single-file ``add``/``validate`` CLI
commands, which share the same lookup) before deciding a file needs a real
read at all. ``occurrences_since_check`` is the matching cross-scan
trust-but-verify counter (see :data:`esm_catalog.scan.ingest._REVALIDATE_EVERY`)
-- only ``scan`` advances or resets it; the single-file commands never
participate (a one-off human correction has no "occurrence count" to speak of,
and always simply trusts or bypasses the cache as asked).

This is CLI bookkeeping, not published STAC — ``push`` skips it.
"""

from __future__ import annotations

import json
from typing import Optional

from pydantic import BaseModel
from upath import UPath

from esm_catalog.scan.types import Md5
from esm_catalog.types import ExperimentId, FileMetadata

CATALOG_DIRNAME = "catalog"
"""The catalog subdirectory created under an experiment root."""

STATE_FILENAME = "esm-catalog.json"
"""The workspace-state file inside the catalog directory."""

QUERYABLES_FILENAME = "queryables.json"
"""The catalog's namelist queryables (a ``pypgstac load-queryables`` file), so
an operator can register them for the STAC Browser filter UI."""

ScannedPath = str
"""A scanned output file's path (as text) — the key its catalogued md5 is stored under."""

StreamKey = str
"""A ``(component, stream)`` pair joined as ``"{component}|{stream}"`` -- JSON
object keys must be strings, so the tuple is flattened for persistence. See
:func:`stream_key`."""


def stream_key(component: Optional[str], stream: Optional[str]) -> StreamKey:
    """The persisted-state key for a ``(component, stream)`` pair."""
    return f"{component}|{stream}"


class WorkspaceState(BaseModel):
    """The persisted ``esm-catalog.json`` — scan bookkeeping for incremental re-scans."""

    experiment_id: ExperimentId
    scanned: dict[ScannedPath, Md5] = {}
    schema_by_stream: dict[StreamKey, FileMetadata] = {}
    occurrences_since_check: dict[StreamKey, int] = {}


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
