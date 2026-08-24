"""Push STAC objects to a catalog: collections, items, and geoparquet shards.

``push <path>...`` is imperative — you name what goes up. Each path is one of:

- a ``*.json`` **Collection** — upserted (create-or-update),
- a ``*.json`` **Item** — upserted into its collection,
- a ``*.parquet`` / ``*.geoparquet`` **stac-geoparquet shard** — its Items are
  read (via the scanner's own :func:`~esm_catalog.storage.geoparquet.read_shard`),
  grouped by collection, and bulk-upserted in chunks,
- a **directory** — expanded to the files above, collections first so item
  targets exist before the items land.

All writes go through the STAC API (authenticated, role-gated) and use *upsert*
semantics, so re-pushing the same object is harmless. Nothing is ever deleted.
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable, Iterable, Literal, Optional

from stac_geoparquet.arrow import stac_table_to_items
from upath import UPath

from esm_catalog.client import StacClient, StacObject
from esm_catalog.storage.geoparquet import read_shard

#: Items per bulk_items request. pgstac loads each batch server-side.
CHUNK_SIZE = 500

#: What a single path resolves to.
PathKind = Literal["collection", "item", "shard", "unknown"]

_SHARD_SUFFIXES = {".parquet", ".geoparquet"}


@dataclass
class PushSummary:
    """Counts of what a push shipped."""

    collections: int = 0
    items: int = 0
    shards: int = 0
    errors: list[str] = field(default_factory=list)


#: Called as ``on_progress(advance, detail)`` to drive a progress display.
ProgressHook = Callable[[int, str], None]


def classify_file(path: Path) -> PathKind:
    """Classify a single (non-directory) path by suffix and, for JSON, content."""
    if path.suffix.lower() in _SHARD_SUFFIXES:
        return "shard"
    if path.suffix.lower() == ".json":
        try:
            obj = json.loads(path.read_text())
        except (ValueError, OSError):
            return "unknown"
        return _stac_kind(obj)
    return "unknown"


def _stac_kind(obj: StacObject) -> PathKind:
    """Classify a parsed STAC object by its ``type`` field."""
    t = str(obj.get("type", "")).lower()
    if t == "collection":
        return "collection"
    if t == "feature":
        return "item"
    return "unknown"


def expand_paths(paths: Iterable[Path]) -> list[Path]:
    """Flatten directories to files, collections-first, then items, then shards.

    Ordering matters: a collection must exist before its items (single or bulk)
    can be written. Within a directory we therefore emit ``*.json`` Collections
    first, then ``*.json`` Items, then shards.
    """
    files: list[Path] = []
    for path in paths:
        if path.is_dir():
            files.extend(sorted(path.glob("*.json")))
            for suffix in _SHARD_SUFFIXES:
                files.extend(sorted(path.glob(f"*{suffix}")))
        else:
            files.append(path)
    # Stable-sort files so collections precede items precede shards.
    order = {"collection": 0, "item": 1, "shard": 2, "unknown": 3}
    return sorted(files, key=lambda p: order[classify_file(p)])


def count_items(path: Path) -> int:
    """Number of Items a path contributes (1 for an Item JSON, N for a shard)."""
    kind = classify_file(path)
    if kind == "item":
        return 1
    if kind == "shard":
        return read_shard(UPath(path)).num_rows
    return 0


def shard_items_by_collection(path: Path) -> dict[str, list[StacObject]]:
    """Read a shard and group its Items by collection id."""
    table = read_shard(UPath(path))
    grouped: dict[str, list[StacObject]] = {}
    for item in stac_table_to_items(table):
        grouped.setdefault(item["collection"], []).append(item)
    return grouped


def push_paths(
    paths: Iterable[Path],
    client: StacClient,
    on_progress: Optional[ProgressHook] = None,
) -> PushSummary:
    """Push everything under *paths* through *client*; return a summary."""
    summary = PushSummary()
    progress = on_progress or (lambda advance, detail: None)

    for path in expand_paths(paths):
        kind = classify_file(path)
        try:
            if kind == "collection":
                client.upsert_collection(json.loads(path.read_text()))
                summary.collections += 1
                progress(1, f"collection {path.name}")
            elif kind == "item":
                client.upsert_item(json.loads(path.read_text()))
                summary.items += 1
                progress(1, f"item {path.name}")
            elif kind == "shard":
                n = _push_shard(path, client, progress)
                summary.items += n
                summary.shards += 1
            else:
                summary.errors.append(f"skipped {path.name}: not a STAC file")
        except Exception as exc:  # noqa: BLE001 — collect, keep pushing the rest
            summary.errors.append(f"{path.name}: {exc}")

    return summary


def _push_shard(path: Path, client: StacClient, progress: ProgressHook) -> int:
    """Bulk-upsert one shard's Items, chunked per collection; return item count."""
    pushed = 0
    for collection_id, items in shard_items_by_collection(path).items():
        for start in range(0, len(items), CHUNK_SIZE):
            batch = items[start : start + CHUNK_SIZE]
            client.bulk_items(collection_id, batch, method="upsert")
            pushed += len(batch)
            progress(len(batch), f"{path.name} -> {collection_id} ({pushed})")
    return pushed
