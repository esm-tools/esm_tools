"""In-memory ingest: walk a run directory and build STAC collections + items.

This is the storage-free pipeline (DuckDB persistence arrives in PR-A2). It ties
together: list files -> detect+scan -> resolve context -> build STAC.
"""

from __future__ import annotations

from pathlib import Path

from loguru import logger

from esm_catalog.scan.context import (
    CollectionContextError,
    RestartFileError,
    resolve_context,
    scan_all_namelists,
)
from esm_catalog.scan.detect import UnsupportedFormatError, scan_file
from esm_catalog.scan.namelist import (
    extract_fesom_mesh_info,
    get_namelist_config_path,
    scan_namelist_directory,
)
from esm_catalog.scan.upath import list_files
from esm_catalog.stac.collection import make_collection
from esm_catalog.stac.item import make_item


def scan_tree(root, config: dict | None = None) -> dict:
    """Scan *root* recursively and return {"collections": [...], "items": [...]}."""
    collections: dict[str, dict] = {}
    namelists_cache: dict = {}
    items: list[dict] = []

    for path in list_files(root):
        try:
            ctx = resolve_context(path, config=config)
        except (RestartFileError, CollectionContextError) as exc:
            logger.debug("Skipping {}: {}", path, exc)
            continue

        if ctx.experiment_path not in namelists_cache:
            namelists_cache[ctx.experiment_path] = scan_all_namelists(ctx.experiment_path)
        ctx.namelists_by_component = namelists_cache[ctx.experiment_path]

        if ctx.collection_id not in collections:
            collections[ctx.collection_id] = _build_collection(ctx)
        elif ctx.component not in collections[ctx.collection_id].get("components", []):
            collections[ctx.collection_id]["components"].append(ctx.component)

        try:
            metadata = scan_file(path)
        except UnsupportedFormatError as exc:
            logger.debug("Unsupported {}: {}", path, exc)
            continue
        items.append(make_item(path, metadata, ctx, config))

    return {"collections": list(collections.values()), "items": items}


def _build_collection(ctx) -> dict:
    namelists = None
    fesom_info = None
    if ctx.experiment_path is not None:
        cfg = get_namelist_config_path(ctx.experiment_path, ctx.component)
        if cfg is not None:
            namelists = scan_namelist_directory(cfg, ctx.component)
            if ctx.component.lower() in ("fesom", "fesom2"):
                fesom_info = extract_fesom_mesh_info(cfg)
    return make_collection(ctx, namelists=namelists, fesom_info=fesom_info)
