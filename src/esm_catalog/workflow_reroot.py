"""Rewrite every asset href under a local catalog after the experiment moved on disk.

Scoped to asset hrefs only (the Collection's own assets, and every Item's
assets in every local shard) -- never a blind substring replace across the
whole document, which would risk corrupting unrelated text that happens to
contain the same string (e.g. a description quoting the old path).

Local-shard mode only: this reads/rewrites the local catalog directory (as
``scan`` produced it), the same ``--exp-root``/``--catalog-dir`` convention
``scan``/``asset`` already use -- not a server-only mode that paginates
everything off the server with no local copy. If the local shards are gone,
this command is not the tool for that (yet).
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field

import pystac
from stac_geoparquet.arrow import stac_table_to_items
from upath import UPath

from esm_catalog.client import StacClient
from esm_catalog.push import push_paths
from esm_catalog.storage.geoparquet import read_shard, write_shard


@dataclass
class RerootReport:
    """What changed (or would change, under ``--dry-run``)."""

    collection_assets_changed: int = 0
    item_assets_changed: int = 0
    shards_touched: list[UPath] = field(default_factory=list)

    @property
    def total_changed(self) -> int:
        return self.collection_assets_changed + self.item_assets_changed


def _rewrite_hrefs(assets: dict, old_root: str, new_root: str) -> int:
    """Exact-substring replace on every asset's ``href``; returns the count touched."""
    changed = 0
    for asset in assets.values():
        href = asset.get("href")
        if isinstance(href, str) and old_root in href:
            asset["href"] = href.replace(old_root, new_root)
            changed += 1
    return changed


def reroot_experiment(
    catalog_dir: UPath, old_root: str, new_root: str, *, dry_run: bool = False
) -> RerootReport:
    """Rewrite *old_root* -> *new_root* in every asset href under *catalog_dir*."""
    report = RerootReport()

    collection_path = catalog_dir / "collection.json"
    if collection_path.exists():
        collection = json.loads(collection_path.read_text())
        report.collection_assets_changed = _rewrite_hrefs(
            collection.get("assets", {}), old_root, new_root
        )
        if not dry_run and report.collection_assets_changed:
            collection_path.write_text(json.dumps(collection, indent=2))

    items_dir = catalog_dir / "items"
    if items_dir.exists():
        for shard_path in sorted(items_dir.glob("*.parquet")):
            table = read_shard(shard_path)
            items = list(stac_table_to_items(table))
            shard_changed = sum(
                _rewrite_hrefs(item.get("assets", {}), old_root, new_root)
                for item in items
            )
            if shard_changed:
                report.item_assets_changed += shard_changed
                report.shards_touched.append(shard_path)
                if not dry_run:
                    write_shard([pystac.Item.from_dict(item) for item in items], shard_path)

    return report


def push_rerooted(catalog_dir: UPath, report: RerootReport, client: StacClient):
    """Push the rewritten collection.json and touched shards through *client*.

    Returns ``None`` (nothing to push) if the report shows no changes --
    callers should not call this under ``--dry-run``.
    """
    paths: list[UPath] = []
    collection_path = catalog_dir / "collection.json"
    if report.collection_assets_changed and collection_path.exists():
        paths.append(collection_path)
    paths.extend(report.shards_touched)
    if not paths:
        return None
    return push_paths(paths, client)
