"""DuckDB-backed stac-fastapi core client.

Implements :class:`~stac_fastapi.types.core.BaseCoreClient` against one or more
``catalog.duckdb`` files.  Multiple catalogs (one per experiment) are queried
together; results are merged and returned as standard STAC responses.

Usage::

    from esm_catalog.api.client import DuckDBCatalogClient
    client = DuckDBCatalogClient(catalogs=["/work/exp1/catalog.duckdb",
                                           "/work/exp2/catalog.duckdb"])
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

import attr
from fastapi import HTTPException
from stac_fastapi.types import stac
from stac_fastapi.types.core import BaseCoreClient
from stac_fastapi.types.search import BaseSearchPostRequest

from esm_catalog.storage.duckdb import CatalogDB


def _parse_datetime_filter(datetime_str: str | None) -> dict:
    """Convert STAC datetime parameter to filter_props entries.

    Handles:
    - Single timestamp:  ``"2000-01-01T00:00:00Z"``
    - Open-ended range:  ``"2000-01-01T00:00:00Z/.."`` or ``"../2005-12-31"``
    - Closed range:      ``"2000-01-01/2005-12-31"``
    """
    if not datetime_str:
        return {}
    if "/" in datetime_str:
        parts = datetime_str.split("/", 1)
        start, end = parts[0], parts[1]
        filt: dict = {}
        if start and start != "..":
            filt["datetime"] = (">=", start)
        if end and end != "..":
            filt["datetime_end"] = ("<=", end)
        return filt
    return {"datetime": ("=", datetime_str)}


def _make_item_collection(
    items: list[dict], total: int, limit: int
) -> stac.ItemCollection:
    """Wrap items list in an ItemCollection dict."""
    return stac.ItemCollection(
        type="FeatureCollection",
        features=items,
        links=[],
        context={
            "returned": len(items),
            "limit": limit,
            "matched": total,
        },
    )


@attr.s
class DuckDBCatalogClient(BaseCoreClient):
    """stac-fastapi core client backed by one or more DuckDB catalog files.

    Attributes:
        catalogs: Paths to ``catalog.duckdb`` files to serve.  At least one
                  must be provided; an empty list raises :exc:`ValueError`.
    """

    catalogs: List[Union[str, Path]] = attr.ib(factory=list)

    def __attrs_post_init__(self):
        if not self.catalogs:
            raise ValueError("DuckDBCatalogClient requires at least one catalog path")
        # Validate all paths exist; warn if not (server may start before first run)
        from loguru import logger
        for p in self.catalogs:
            if not Path(p).exists():
                logger.warning("Catalog not found (will be served when created): {}", p)

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _open_catalogs(self) -> list[CatalogDB]:
        """Open all configured catalogs, skipping missing files."""
        dbs = []
        for p in self.catalogs:
            if Path(p).exists():
                dbs.append(CatalogDB(p))
        return dbs

    def _all_collections_from_dbs(self, dbs: list[CatalogDB]) -> list[dict]:
        collections = []
        seen: set[str] = set()
        for db in dbs:
            for col in db.iter_collections():
                if col["id"] not in seen:
                    collections.append(col)
                    seen.add(col["id"])
        return collections

    # ------------------------------------------------------------------
    # BaseCoreClient abstract methods
    # ------------------------------------------------------------------

    def all_collections(self, **kwargs) -> stac.Collections:
        """GET /collections — return all collections across all catalogs."""
        request = kwargs.get("request")
        # Support simple ?q= filter via query params (collection-search)
        q_params: dict = {}
        if request is not None:
            for key, val in request.query_params.items():
                if key not in ("limit", "offset", "token"):
                    q_params[key] = val

        dbs = self._open_catalogs()
        try:
            all_cols: list[dict] = []
            seen: set[str] = set()
            for db in dbs:
                matched, _ = db.search_collections(
                    filter_props=q_params if q_params else None
                )
                for col in matched:
                    if col["id"] not in seen:
                        all_cols.append(col)
                        seen.add(col["id"])
        finally:
            for db in dbs:
                db.close()

        return stac.Collections(
            collections=all_cols,
            links=[],
            numberMatched=len(all_cols),
            numberReturned=len(all_cols),
        )

    def get_collection(self, collection_id: str, **kwargs) -> stac.Collection:
        """GET /collections/{collection_id}"""
        dbs = self._open_catalogs()
        try:
            for db in dbs:
                col = db.get_collection(collection_id)
                if col is not None:
                    return stac.Collection(**col)
        finally:
            for db in dbs:
                db.close()
        raise HTTPException(status_code=404, detail=f"Collection '{collection_id}' not found")

    def item_collection(
        self,
        collection_id: str,
        bbox: Optional[Any] = None,
        datetime: Optional[str] = None,
        limit: int = 10,
        token: Optional[str] = None,
        **kwargs,
    ) -> stac.ItemCollection:
        """GET /collections/{collection_id}/items"""
        offset = int(token) if token and token.isdigit() else 0
        filter_props: dict = {"collection": collection_id}
        if bbox:
            filter_props["bbox"] = bbox
        filter_props.update(_parse_datetime_filter(datetime))

        items: list[dict] = []
        total = 0
        dbs = self._open_catalogs()
        try:
            for db in dbs:
                db_items, db_total = db.search_items(filter_props, limit=limit, offset=offset)
                items.extend(db_items)
                total += db_total
        finally:
            for db in dbs:
                db.close()

        return _make_item_collection(items[:limit], total, limit)

    def get_item(self, item_id: str, collection_id: str, **kwargs) -> stac.Item:
        """GET /collections/{collection_id}/items/{item_id}"""
        dbs = self._open_catalogs()
        try:
            for db in dbs:
                results, _ = db.search_items(
                    {"collection": collection_id, "id": item_id}, limit=1
                )
                if results:
                    return stac.Item(**results[0])
        finally:
            for db in dbs:
                db.close()
        raise HTTPException(
            status_code=404,
            detail=f"Item '{item_id}' not found in collection '{collection_id}'",
        )

    def get_search(
        self,
        collections: Optional[List[str]] = None,
        ids: Optional[List[str]] = None,
        bbox: Optional[Any] = None,
        intersects: Optional[Any] = None,
        datetime: Optional[str] = None,
        limit: Optional[int] = 10,
        **kwargs,
    ) -> stac.ItemCollection:
        """GET /search"""
        filter_props: dict = {}
        if collections and len(collections) == 1:
            filter_props["collection"] = collections[0]
        if ids and len(ids) == 1:
            filter_props["id"] = ids[0]
        filter_props.update(_parse_datetime_filter(datetime))
        return self._run_search(filter_props, limit or 10)

    def post_search(
        self, search_request: BaseSearchPostRequest, **kwargs
    ) -> stac.ItemCollection:
        """POST /search"""
        filter_props: dict = {}
        if search_request.collections and len(search_request.collections) == 1:
            filter_props["collection"] = search_request.collections[0]
        if search_request.ids and len(search_request.ids) == 1:
            filter_props["id"] = search_request.ids[0]
        filter_props.update(_parse_datetime_filter(search_request.datetime))
        limit = search_request.limit or 10
        return self._run_search(filter_props, limit)

    # ------------------------------------------------------------------
    # Internal search
    # ------------------------------------------------------------------

    def _run_search(self, filter_props: dict, limit: int) -> stac.ItemCollection:
        items: list[dict] = []
        total = 0
        dbs = self._open_catalogs()
        try:
            for db in dbs:
                db_items, db_total = db.search_items(filter_props, limit=limit)
                items.extend(db_items)
                total += db_total
        finally:
            for db in dbs:
                db.close()
        return _make_item_collection(items[:limit], total, limit)
