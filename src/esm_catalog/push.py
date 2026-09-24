"""Push STAC objects to a catalog: collections, items, and geoparquet shards.

``push <path>...`` is imperative — you name what goes up. Each path is one of:

- a ``*.json`` **Collection** — upserted (create-or-update),
- a ``*.json`` **Item** — upserted into its collection,
- a ``*.parquet`` / ``*.geoparquet`` **stac-geoparquet shard** — its rows are
  read (via the scanner's own :func:`~esm_catalog.storage.geoparquet.read_shard`),
  grouped by Item id (many rows can share an id -- each is a single-asset view
  of a growing stream, see :mod:`esm_catalog.item`), and merged into whatever
  the server already holds for that id before being upserted,
- a **directory** — expanded to the files above, collections first so item
  targets exist before the items land.

All writes go through the STAC API (authenticated, role-gated) and use *upsert*
semantics, so re-pushing the same object is harmless. Nothing is ever deleted.
Re-pushing the same shard is also idempotent for a growing Item specifically:
asset keys are deterministic (see :mod:`esm_catalog.item`), so merging the
same asset in twice overwrites the same key with the same value.
"""

from __future__ import annotations

import json
import traceback as _traceback
from pathlib import Path
from typing import Iterable, Literal, Optional, Protocol

from pydantic import BaseModel, Field
from stac_geoparquet.arrow import stac_table_to_items
from upath import UPath

from esm_catalog.client import CollectionId, StacClient, StacClientError, StacObject
from esm_catalog.scan.workspace import QUERYABLES_FILENAME, STATE_FILENAME
from esm_catalog.storage.geoparquet import read_shard

#: What a single path resolves to.
PathKind = Literal["collection", "item", "shard", "unknown"]

_SHARD_SUFFIXES = {".parquet", ".geoparquet"}

#: The delta sidecar naming queryables present in the catalog but not yet
#: registered on the server (a ``pypgstac load-queryables`` file).
QUERYABLES_DELTA_FILENAME = "queryables-delta.json"

#: Catalog sidecars that are not STAC objects and must never be pushed.
_SKIP_FILES = {STATE_FILENAME, QUERYABLES_FILENAME, QUERYABLES_DELTA_FILENAME}


class PushError(BaseModel):
    """One path's failure, structured for --json instead of a flattened string.

    ``exception_type`` is None for a skip (not a STAC file) -- there was no
    exception, just a path push_paths declined to touch. ``status`` is the
    HTTP status code, only ever set for our own StacClientError (a network
    library's ConnectError/TimeoutException etc. has no status to report).
    ``traceback`` is only populated when push_paths is asked for it
    (--verbose --json together) -- always available, but verbose enough
    that it shouldn't be in every error by default.
    """

    path: str
    exception_type: Optional[str] = None
    message: str
    status: Optional[int] = None
    traceback: Optional[list[str]] = None

    def __str__(self) -> str:
        """Render the same text shape the old plain-string errors had.

        A skip (exception_type is None) rendered as "skipped {path}: {message}";
        a real failure rendered as "{path}: {message}" with no such prefix.
        """
        if self.exception_type is None:
            return f"skipped {self.path}: {self.message}"
        return f"{self.path}: {self.message}"


class PushSummary(BaseModel):
    """Counts of what a push shipped."""

    collections: int = 0
    items: int = 0
    shards: int = 0
    errors: list[PushError] = Field(default_factory=list)


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
    catalog sidecars (the ``esm-catalog.json`` workspace state, the
    ``queryables.json`` file, and ``queryables-delta.json``, itself written by
    a previous push) are skipped: they are bookkeeping, not STAC objects, so
    they must not count as failed pushes.
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


def _merge_collection_extent(
    existing: Optional[StacObject], incoming: StacObject
) -> StacObject:
    """Widen *incoming*'s extent to also cover *existing*'s, if any.

    *incoming*'s own extent only reflects what the pushing scan run saw
    locally (see :mod:`esm_catalog.collection`); without this, repeatedly
    pushing new runs of a long experiment would keep narrowing the server's
    stored extent to just the latest run instead of accumulating it.
    """
    if existing is None or "extent" not in existing or "extent" not in incoming:
        return incoming
    merged = dict(incoming)
    merged["extent"] = {
        "spatial": {
            "bbox": [
                _merge_bbox(
                    existing["extent"]["spatial"]["bbox"][0],
                    incoming["extent"]["spatial"]["bbox"][0],
                )
            ]
        },
        "temporal": {
            "interval": [
                _merge_interval(
                    existing["extent"]["temporal"]["interval"][0],
                    incoming["extent"]["temporal"]["interval"][0],
                )
            ]
        },
    }
    return merged


def _merge_bbox(first: list, second: list) -> list:
    """The smallest bbox containing both inputs."""
    return [
        min(first[0], second[0]),
        min(first[1], second[1]),
        max(first[2], second[2]),
        max(first[3], second[3]),
    ]


def _merge_interval(first: list, second: list) -> list:
    """The widest [start, end] interval covering both inputs (None = open)."""
    starts = [_parse_datetime(v) for v in (first[0], second[0])]
    ends = [_parse_datetime(v) for v in (first[1], second[1])]
    starts = [s for s in starts if s is not None]
    ends = [e for e in ends if e is not None]
    start = min(starts).isoformat() if starts else None
    end = max(ends).isoformat() if ends else None
    return [start, end]


def _group_by_item_id(items: list[StacObject]) -> dict[str, list[StacObject]]:
    """Group a collection's shard rows by Item id.

    A shard row is a single-asset *view* of a growing Item (see
    :mod:`esm_catalog.item`) -- many rows across many scans can share one id.
    Grouping here means every row for the same id merges in one push, not
    once per row.
    """
    grouped: dict[str, list[StacObject]] = {}
    for item in items:
        grouped.setdefault(item["id"], []).append(item)
    return grouped


def _parse_datetime(value: Optional[str]):
    from datetime import datetime

    if value is None:
        return None
    return datetime.fromisoformat(value.replace("Z", "+00:00"))


def _item_span(item: StacObject) -> tuple:
    """This item's (start, end) datetime, from start_datetime/end_datetime or
    the single instant datetime -- whichever the shard row set."""
    props = item.get("properties", {})
    start = _parse_datetime(props.get("start_datetime") or props.get("datetime"))
    end = _parse_datetime(props.get("end_datetime") or props.get("datetime"))
    return start, end


def merge_item(existing: Optional[StacObject], incoming: list[StacObject]) -> StacObject:
    """Merge *incoming* single-asset shard rows (all sharing one Item id) into
    *existing* (the server's current state for that id, or None if this is
    the first time this stream has ever been pushed).

    Assets merge by key (append-only in practice -- see item.py's asset-key
    scheme, which makes re-pushing the same file idempotent rather than
    duplicating it). start_datetime/end_datetime widen to cover every asset
    now on the item, existing and incoming alike.
    """
    # Base on an incoming row, never on existing -- existing is a server
    # response of unknown/possibly-partial shape, while every incoming row is
    # a freshly-built Item guaranteed to carry id/collection/type. Only its
    # assets and temporal span get widened with whatever existing adds.
    base = dict(incoming[0])
    assets = dict(existing.get("assets", {})) if existing is not None else {}
    starts, ends = [], []
    if existing is not None:
        s, e = _item_span(existing)
        if s:
            starts.append(s)
        if e:
            ends.append(e)
    for item in incoming:
        assets.update(item.get("assets", {}))
        s, e = _item_span(item)
        if s:
            starts.append(s)
        if e:
            ends.append(e)

    base["assets"] = assets
    if starts and ends:
        start, end = min(starts), max(ends)
        base.setdefault("properties", {})
        base["properties"]["start_datetime"] = start.isoformat()
        base["properties"]["end_datetime"] = end.isoformat()
        if start == end:
            base["datetime"] = start.isoformat()
            base["properties"]["datetime"] = start.isoformat()
        else:
            base["datetime"] = None
            base["properties"]["datetime"] = None
    return base


def push_paths(
    paths: Iterable[Path],
    client: StacClient,
    on_progress: Optional[ProgressHook] = None,
    include_traceback: bool = False,
) -> PushSummary:
    """Push everything under *paths* through *client*; return a summary.

    *include_traceback* attaches the full formatted traceback to each
    PushError -- off by default (it's verbose), meant for --verbose --json.
    """
    summary = PushSummary()
    progress = on_progress or (lambda advance, detail: None)

    for path in expand_paths(paths):
        kind = classify_file(path)
        try:
            if kind == "collection":
                collection = json.loads(path.read_text())
                existing = client.get_collection(collection["id"])
                client.upsert_collection(_merge_collection_extent(existing, collection))
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
                summary.errors.append(
                    PushError(path=path.name, message="not a STAC file")
                )
        except Exception as exc:  # noqa: BLE001 — collect, keep pushing the rest
            summary.errors.append(
                PushError(
                    path=path.name,
                    exception_type=f"{type(exc).__module__}.{type(exc).__name__}",
                    message=str(exc),
                    status=exc.status if isinstance(exc, StacClientError) else None,
                    traceback=(
                        _traceback.format_exception(type(exc), exc, exc.__traceback__)
                        if include_traceback
                        else None
                    ),
                )
            )

    return summary


def _push_shard(path: Path, client: StacClient, progress: ProgressHook) -> int:
    """Merge-and-push one shard's Items; return the number of asset rows pushed.

    Each shard row is a single-asset view of a growing (Item = stream) id
    (see :mod:`esm_catalog.item`). For every distinct id in this shard: fetch
    whatever the server already has for it (None the first time this stream
    is ever pushed), merge in every row from this shard sharing that id
    (:func:`merge_item`), then upsert the merged result as one Item. Not a
    bulk_items batch -- the Bulk Transactions extension replaces an Item
    wholesale on conflict, which would silently drop assets accumulated by
    earlier pushes; a real per-id merge needs the single-item upsert path.
    """
    pushed = 0
    for collection_id, items in shard_items_by_collection(path).items():
        for item_id, rows in _group_by_item_id(items).items():
            existing = client.get_item(collection_id, item_id)
            client.upsert_item(merge_item(existing, rows))
            pushed += len(rows)
            progress(len(rows), f"{path.name} -> {collection_id}/{item_id} ({pushed})")
    return pushed


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
