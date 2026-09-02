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
from pathlib import Path
from typing import Iterable, Literal, Optional, Protocol

from pydantic import BaseModel, Field
from stac_geoparquet.arrow import stac_table_to_items
from upath import UPath

from esm_catalog.client import CollectionId, StacClient, StacObject
from esm_catalog.scan.workspace import QUERYABLES_FILENAME, STATE_FILENAME
from esm_catalog.storage.geoparquet import read_shard

#: Items per bulk_items request. pgstac loads each batch server-side.
CHUNK_SIZE = 500

#: What a single path resolves to.
PathKind = Literal["collection", "item", "shard", "unknown"]

_SHARD_SUFFIXES = {".parquet", ".geoparquet"}

#: Catalog sidecars that are not STAC objects and must never be pushed.
_SKIP_FILES = {STATE_FILENAME, QUERYABLES_FILENAME}


class PushSummary(BaseModel):
    """Counts of what a push shipped."""

    collections: int = 0
    items: int = 0
    shards: int = 0
    errors: list[str] = Field(default_factory=list)


class ProgressHook(Protocol):
    """Called as ``hook(advance, detail)`` to drive a progress display."""

    def __call__(self, advance: int, detail: str) -> None: ...


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
    can be written, so the returned list is Collections, then Items, then shards.

    Directories are searched **recursively** — the scanner writes shards under
    ``items/`` while ``collection.json`` sits at the catalog root — and the
    catalog sidecars (the ``esm-catalog.json`` workspace state and the
    ``queryables.json`` file) are skipped: they are bookkeeping, not STAC
    objects, so they must not count as failed pushes.
    """
    files: list[Path] = []
    for path in paths:
        if path.is_dir():
            candidates = sorted(path.rglob("*.json"))
            for suffix in _SHARD_SUFFIXES:
                candidates += sorted(path.rglob(f"*{suffix}"))
            files.extend(f for f in candidates if f.name not in _SKIP_FILES)
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


def shard_items_by_collection(path: Path) -> dict[CollectionId, list[StacObject]]:
    """Read a shard and group its Items by collection id."""
    table = read_shard(UPath(path))
    grouped: dict[CollectionId, list[StacObject]] = {}
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
                summary.items += _push_shard(path, client, progress)
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


#: The delta sidecar naming queryables present in the catalog but not yet
#: registered on the server (a ``pypgstac load-queryables`` file).
QUERYABLES_DELTA_FILENAME = "queryables-delta.json"


def registered_queryables(api_url: str, verify_tls: bool) -> set[str]:
    """The property names the server currently advertises as queryables."""
    import httpx

    resp = httpx.get(f"{api_url}/queryables", verify=verify_tls, timeout=30)
    resp.raise_for_status()
    return set(resp.json().get("properties", {}))


def queryable_delta(
    catalog_dir: Path, api_url: str, verify_tls: bool
) -> Optional[Path]:
    """Diff the catalog's ``queryables.json`` against the server; write the delta.

    Returns the path to a written ``queryables-delta.json`` (its ``properties``
    are the queryables present in the catalog but not yet registered on the
    server), or ``None`` when there is nothing to register. If the server cannot
    be reached for the diff, the *full* set is emitted so registration is never
    silently skipped.
    """
    source = catalog_dir / QUERYABLES_FILENAME
    if not source.exists():
        return None
    properties = json.loads(source.read_text()).get("properties", {})
    if not properties:
        return None
    try:
        already = registered_queryables(api_url, verify_tls)
    except Exception:  # noqa: BLE001 — unreachable server -> emit the full set
        already = set()
    new = {name: definition for name, definition in properties.items() if name not in already}
    if not new:
        return None
    delta_path = catalog_dir / QUERYABLES_DELTA_FILENAME
    delta_path.write_text(json.dumps({"properties": new}, indent=2))
    return delta_path
