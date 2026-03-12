"""DuckDB-backed stac-fastapi core client.

Implements :class:`~stac_fastapi.types.core.BaseCoreClient` against one or more
``catalog.duckdb`` files.  Multiple catalogs (one per experiment) are queried
together; results are merged and returned as standard STAC responses.

Usage::

    from esm_catalog.api.client import DuckDBCatalogClient
    from esm_catalog.api.pool import CatalogPool
    from esm_catalog.api.registry import CatalogRegistry

    # With registry (dynamic catalog management)
    registry = CatalogRegistry(initial_catalogs=["/work/exp1/catalog.duckdb"])
    pool = CatalogPool()
    client = DuckDBCatalogClient(registry=registry, pool=pool)

    # Legacy mode (static catalog list)
    client = DuckDBCatalogClient(catalogs=["/work/exp1/catalog.duckdb"])
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import TYPE_CHECKING, Any, Dict, List, Optional, Union

import attr
from fastapi import HTTPException
from loguru import logger
from pydantic import Field
from stac_fastapi.types import stac
from stac_fastapi.types.core import BASE_CONFORMANCE_CLASSES, BaseCoreClient
from stac_fastapi.types.search import BaseSearchPostRequest

if TYPE_CHECKING:
    from esm_catalog.api.cache import CollectionCache
    from esm_catalog.api.pool import CatalogPool
    from esm_catalog.api.registry import CatalogRegistry
    from esm_catalog.storage.duckdb import CatalogDB

# Extra conformance classes beyond the stac-fastapi defaults.
# STAC Browser 5 checks for the OGC CQL2 classes to decide whether to show
# the "Additional filters" builder, in addition to the STAC-API filter class.
_EXTRA_CONFORMANCE = [
    "https://api.stacspec.org/v1.0.0/collection-search",
    "https://api.stacspec.org/v1.0.0/collection-search#filter",
    "https://api.stacspec.org/v1.0.0/item-search#filter",
    # OGC API - Features Part 3 (CQL2) conformance classes
    "http://www.opengis.net/spec/ogcapi-features-3/1.0/conf/filter",
    "http://www.opengis.net/spec/ogcapi-features-3/1.0/conf/features-filter",
    "http://www.opengis.net/spec/cql2/1.0/conf/cql2-text",
    "http://www.opengis.net/spec/cql2/1.0/conf/cql2-json",
]

# OGC rel for queryables link - STAC Browser checks for this exact URI
_OGC_QUERYABLES_REL = "http://www.opengis.net/def/rel/ogc/1.0/queryables"

# Operator mapping from CQL2-JSON op names to SQL operators
_CQL2_OP_MAP: dict[str, str] = {
    "eq": "=", "=": "=",
    "neq": "!=", "!=": "!=", "<>": "!=",
    "lt": "<", "<": "<",
    "lte": "<=", "<=": "<=",
    "gt": ">", ">": ">",
    "gte": ">=", ">=": ">=",
    "like": "LIKE",
}
_CQL2_OP_INVERSE: dict[str, str] = {
    "<": ">", "<=": ">=", ">": "<", ">=": "<=", "=": "=", "!=": "!=", "LIKE": "LIKE",
}


def _cql2_value(val: Any) -> Any:
    """Unwrap a CQL2 literal object into a plain Python value.

    CQL2-JSON represents temporal literals as ``{"timestamp": "..."}`` or
    ``{"date": "..."}`` dicts rather than bare strings.  Return the inner
    string so the DB layer receives a value it can bind as TIMESTAMPTZ.
    """
    if isinstance(val, dict):
        if "timestamp" in val:
            return val["timestamp"]
        if "date" in val:
            return val["date"]
    return val


def _parse_cql2_json(expr: dict | None) -> dict:
    """Parse a CQL2-JSON filter expression into a flat ``filter_props`` dict.

    Handles AND combinations and comparison operators. OR/NOT are logged
    and handled with limited support (OR uses first clause only, NOT ignored).

    Returns:
        Dict mapping field name -> (sql_op, value) suitable for
        :meth:`~esm_catalog.storage.duckdb.CatalogDB.search_items`.
    """
    if not expr:
        return {}
    op = str(expr.get("op", "")).lower()
    args = expr.get("args", [])

    if op == "and":
        result: dict = {}
        for arg in args:
            result.update(_parse_cql2_json(arg))
        return result

    if op == "or":
        logger.warning(
            "CQL2 OR operator not fully supported; using first clause only. "
            "Filter: {}", expr
        )
        if args:
            return _parse_cql2_json(args[0])
        return {}

    if op == "not":
        logger.warning(
            "CQL2 NOT operator not supported; filter ignored. Filter: {}", expr
        )
        return {}

    sql_op = _CQL2_OP_MAP.get(op)
    if sql_op and len(args) == 2:
        left, right = args
        if isinstance(left, dict) and "property" in left:
            return {left["property"]: (sql_op, _cql2_value(right))}
        if isinstance(right, dict) and "property" in right:
            # Reversed - invert the operator
            inv = _CQL2_OP_INVERSE.get(sql_op, sql_op)
            return {right["property"]: (inv, _cql2_value(left))}

    # Spatial ops or unknown - log and ignore
    if op:
        logger.debug("CQL2 operator '{}' not supported; filter ignored", op)
    return {}


class FilteredSearchPostRequest(BaseSearchPostRequest):
    """POST /search request model extended with CQL2 filter fields.

    stac-fastapi's base model drops unknown fields, so ``filter`` and
    ``filter-lang`` from STAC Browser are silently discarded unless we
    capture them here.
    """

    # Use model_fields alias because JSON key contains a hyphen
    filter: Optional[Dict[str, Any]] = Field(default=None)
    filter_lang: Optional[str] = Field(default=None, alias="filter-lang")
    # Opaque pagination token - encodes the integer offset of the next page.
    # STAC Browser follows the ``next`` link body which includes this field.
    token: Optional[str] = Field(default=None)

    model_config = {"populate_by_name": True}


def _inject_item_links(item: dict, base_url: str) -> dict:
    """Return a copy of *item* with absolute self, root, and collection links.

    Items stored in the DB have only a fragment ``collection`` link
    (``href: "#collection-id"``).  STAC Browser needs absolute URLs to render
    item cards and navigate to the parent collection.
    """
    item = dict(item)
    cid = item.get("collection", "")
    iid = item.get("id", "")
    item["links"] = [
        lnk for lnk in item.get("links", [])
        if lnk.get("rel") not in ("self", "root", "collection")
    ]
    item["links"].extend([
        {"rel": "self", "type": "application/geo+json",
         "href": f"{base_url}/collections/{cid}/items/{iid}"},
        {"rel": "root", "type": "application/json", "href": f"{base_url}/"},
        {"rel": "collection", "type": "application/json",
         "href": f"{base_url}/collections/{cid}"},
    ])
    return item


def _inject_collection_links(col: dict, base_url: str) -> dict:
    """Return a copy of *col* with self, root, and items links set.

    stac-fastapi stores collections with only a ``parent`` link.
    STAC Browser needs ``items`` to know where to fetch items for a collection.
    """
    col = dict(col)
    cid = col["id"]
    # Remove any stale self/root/items links from stored JSON, then re-add
    col["links"] = [
        lnk for lnk in col.get("links", [])
        if lnk.get("rel") not in ("self", "root", "items")
    ]
    col["links"].extend([
        {"rel": "self", "type": "application/json",
         "href": f"{base_url}/collections/{cid}"},
        {"rel": "root", "type": "application/json", "href": f"{base_url}/"},
        {"rel": "items", "type": "application/geo+json",
         "href": f"{base_url}/collections/{cid}/items", "title": "Items"},
    ])
    return col


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
    items: list[dict],
    total: int,
    limit: int,
    offset: int = 0,
    base_url: str = "",
    method: str = "GET",
    search_body: dict | None = None,
    search_path: str = "/search",
) -> stac.ItemCollection:
    """Wrap items list in an ItemCollection dict with pagination links."""
    returned = len(items)
    links: list[dict] = []

    if base_url:
        href_base = f"{base_url}{search_path}"

        # first
        if method == "POST" and search_body is not None:
            first_body = {k: v for k, v in search_body.items() if k != "token"}
            links.append({"rel": "first", "type": "application/geo+json",
                          "method": "POST", "href": href_base, "body": first_body})
        else:
            links.append({"rel": "first", "type": "application/geo+json",
                          "href": href_base})

        # prev
        if offset > 0:
            prev_offset = max(0, offset - limit)
            if method == "POST" and search_body is not None:
                prev_body = {k: v for k, v in search_body.items() if k != "token"}
                if prev_offset > 0:
                    prev_body["token"] = str(prev_offset)
                links.append({"rel": "prev", "type": "application/geo+json",
                              "method": "POST", "href": href_base, "body": prev_body})
            else:
                prev_href = (f"{href_base}?token={prev_offset}&limit={limit}"
                             if prev_offset > 0 else href_base)
                links.append({"rel": "prev", "type": "application/geo+json",
                              "href": prev_href})

        # next
        next_offset = offset + returned
        if next_offset < total:
            if method == "POST" and search_body is not None:
                next_body = {
                    **{k: v for k, v in search_body.items() if k != "token"},
                    "token": str(next_offset),
                }
                links.append({"rel": "next", "type": "application/geo+json",
                              "method": "POST", "href": href_base, "body": next_body})
            else:
                links.append({"rel": "next", "type": "application/geo+json",
                              "href": f"{href_base}?token={next_offset}&limit={limit}"})

    return stac.ItemCollection(
        type="FeatureCollection",
        features=items,
        links=links,
        numberMatched=total,
        numberReturned=returned,
        context={
            "returned": returned,
            "limit": limit,
            "matched": total,
        },
    )


@attr.s
class DuckDBCatalogClient(BaseCoreClient):
    """stac-fastapi core client backed by one or more DuckDB catalog files.

    Supports two modes:
    1. **Registry mode** (recommended): Use a CatalogRegistry and CatalogPool
       for dynamic catalog management and connection pooling.
    2. **Legacy mode**: Provide a static list of catalog paths.

    Attributes:
        catalogs: Static list of catalog paths (legacy mode).
        registry: CatalogRegistry for dynamic catalog management.
        pool: CatalogPool for connection pooling.
    """

    # Legacy mode: static catalog list
    catalogs: List[Union[str, Path]] = attr.ib(factory=list)

    # Registry mode: dynamic catalog management with pooling
    registry: Optional["CatalogRegistry"] = attr.ib(default=None)
    pool: Optional["CatalogPool"] = attr.ib(default=None)

    # Optional collection cache for landing page performance
    collection_cache: Optional["CollectionCache"] = attr.ib(default=None)

    base_conformance_classes: List[str] = attr.ib(
        factory=lambda: BASE_CONFORMANCE_CLASSES + _EXTRA_CONFORMANCE
    )

    def __attrs_post_init__(self) -> None:
        # If using legacy mode, validate that we have at least one catalog
        # Registry mode allows starting with zero catalogs (added via API)
        if self.registry is None and not self.catalogs:
            logger.warning(
                "DuckDBCatalogClient initialized with no catalogs. "
                "Use registry mode or provide catalog paths."
            )

        # In legacy mode, warn about missing catalogs
        if self.registry is None:
            for p in self.catalogs:
                if not Path(p).exists():
                    logger.warning(
                        "Catalog not found (will be served when created): {}", p
                    )

    # ------------------------------------------------------------------
    # Catalog access helpers
    # ------------------------------------------------------------------

    @property
    def _catalog_paths(self) -> list[str]:
        """Return current catalog paths from registry or static list."""
        if self.registry is not None:
            return self.registry.get_paths()
        return [str(p) for p in self.catalogs]

    def _open_catalogs(self) -> list["CatalogDB"]:
        """Open all configured catalogs, using pool if available.

        In registry mode with a pool, connections are cached and reused.
        In legacy mode, connections are opened fresh each time.
        """
        from esm_catalog.storage.duckdb import CatalogDB

        dbs: list[CatalogDB] = []
        paths = self._catalog_paths

        if self.pool is not None:
            # Pool mode: use cached connections
            for p in paths:
                db = self.pool.get(p)
                if db is not None:
                    dbs.append(db)
        else:
            # Legacy mode: open fresh connections
            for p in paths:
                if Path(p).exists():
                    dbs.append(CatalogDB(p))

        return dbs

    def _close_catalogs(self, dbs: list["CatalogDB"]) -> None:
        """Close catalogs if not using pool (legacy mode only)."""
        if self.pool is None:
            for db in dbs:
                db.close()

    def _get_all_collection_summaries(self) -> list[dict]:
        """Get summary info (id, title) for all collections across all catalogs.

        Uses collection_cache if available to avoid repeated database queries.
        Returns a list of dicts with 'id' and 'title' keys.
        """
        def fetch_summaries() -> dict:
            dbs = self._open_catalogs()
            try:
                summaries: list[dict] = []
                seen: set[str] = set()
                for db in dbs:
                    for col in db.iter_collections():
                        if col["id"] not in seen:
                            summaries.append({
                                "id": col["id"],
                                "title": col.get("title", col["id"]),
                            })
                            seen.add(col["id"])
                return {"summaries": summaries}
            finally:
                self._close_catalogs(dbs)

        if self.collection_cache is not None:
            result = self.collection_cache.get_or_compute("summaries", fetch_summaries)
            return result["summaries"]
        return fetch_summaries()["summaries"]

    # ------------------------------------------------------------------
    # Federated search with correct pagination
    # ------------------------------------------------------------------

    def _federated_search(
        self,
        filter_props: dict,
        limit: int,
        offset: int = 0,
    ) -> tuple[list[dict], int]:
        """Execute a federated search across multiple catalogs with correct pagination.

        This implements a two-phase query approach:
        1. Get total counts from each catalog
        2. Calculate which catalogs contribute to the requested page and query
           only those with correct local offsets

        Args:
            filter_props: Filter parameters for search_items.
            limit: Maximum items to return.
            offset: Global offset into combined result set.

        Returns:
            Tuple of (items_list, total_count).
        """
        dbs = self._open_catalogs()
        if not dbs:
            return [], 0

        try:
            # Phase 1: Get total counts from each catalog
            counts: list[int] = []
            for db in dbs:
                _, count = db.search_items(filter_props, limit=0, offset=0)
                counts.append(count)

            total = sum(counts)

            if offset >= total:
                return [], total

            # Phase 2: Determine which catalogs contribute to this page
            items: list[dict] = []
            global_offset = offset
            remaining_limit = limit

            for db, count in zip(dbs, counts):
                if remaining_limit <= 0:
                    break

                if global_offset >= count:
                    # Skip this entire catalog
                    global_offset -= count
                    continue

                # This catalog contributes to the result
                local_offset = global_offset
                local_limit = min(remaining_limit, count - local_offset)

                db_items, _ = db.search_items(
                    filter_props, limit=local_limit, offset=local_offset
                )
                items.extend(db_items)

                remaining_limit -= len(db_items)
                global_offset = 0  # Subsequent catalogs start at offset 0

            return items, total

        finally:
            self._close_catalogs(dbs)

    # ------------------------------------------------------------------
    # Landing page - override to add child links for STAC Browser Browse
    # ------------------------------------------------------------------

    def landing_page(self, **kwargs) -> stac.LandingPage:
        """Extend the default landing page with per-collection child links.

        STAC Browser Browse mode navigates via ``child`` links.  stac-fastapi's
        default landing page only includes a ``data`` link to ``/collections``;
        without explicit ``child`` links the Browse view is empty.

        Uses collection_cache if available to avoid repeated database queries
        on every landing page request.
        """
        lp = super().landing_page(**kwargs)
        request = kwargs.get("request")
        base_url = str(request.base_url).rstrip("/") if request else ""

        # Queryables link - STAC Browser checks for the full OGC rel URI
        lp["links"].append({
            "rel": _OGC_QUERYABLES_REL,
            "type": "application/schema+json",
            "title": "Queryables",
            "href": f"{base_url}/queryables",
        })

        # Add child links for each collection (uses cache if available)
        for col_summary in self._get_all_collection_summaries():
            lp["links"].append({
                "rel": "child",
                "type": "application/json",
                "title": col_summary["title"],
                "href": f"{base_url}/collections/{col_summary['id']}",
            })

        return lp

    # ------------------------------------------------------------------
    # BaseCoreClient abstract methods
    # ------------------------------------------------------------------

    def all_collections(self, **kwargs) -> stac.Collections:
        """GET /collections - return all collections across all catalogs.

        Supports CQL2-JSON filtering via the ``filter`` query parameter so
        STAC Browser "Search for Collections -> Additional filters" works.
        """
        request = kwargs.get("request")
        base_url = str(request.base_url).rstrip("/") if request else ""

        filter_props: dict = {}
        if request is not None:
            qp = request.query_params
            # CQL2-JSON via ?filter=<json>&filter-lang=cql2-json
            raw_filter = qp.get("filter")
            filter_lang = qp.get("filter-lang", "cql2-json")
            if raw_filter:
                if filter_lang == "cql2-json":
                    try:
                        filter_props = _parse_cql2_json(json.loads(raw_filter))
                    except json.JSONDecodeError as e:
                        logger.warning(
                            "Invalid CQL2-JSON filter syntax, ignoring filter: {}",
                            str(e)[:100],
                        )
                    except Exception as e:
                        logger.warning(
                            "Failed to parse CQL2 filter, ignoring: {} - {}",
                            type(e).__name__,
                            str(e)[:100],
                        )

        dbs = self._open_catalogs()
        try:
            all_cols: list[dict] = []
            seen: set[str] = set()
            for db in dbs:
                matched, _ = db.search_collections(
                    filter_props=filter_props if filter_props else None
                )
                for col in matched:
                    if col["id"] not in seen:
                        all_cols.append(_inject_collection_links(col, base_url))
                        seen.add(col["id"])
        finally:
            self._close_catalogs(dbs)

        return stac.Collections(
            collections=all_cols,
            links=[],
            numberMatched=len(all_cols),
            numberReturned=len(all_cols),
        )

    def get_collection(self, collection_id: str, **kwargs) -> stac.Collection:
        """GET /collections/{collection_id}"""
        request = kwargs.get("request")
        base_url = str(request.base_url).rstrip("/") if request else ""

        dbs = self._open_catalogs()
        try:
            for db in dbs:
                col = db.get_collection(collection_id)
                if col is not None:
                    col = _inject_collection_links(col, base_url)
                    return stac.Collection(**col)
        finally:
            self._close_catalogs(dbs)

        raise HTTPException(
            status_code=404, detail=f"Collection '{collection_id}' not found"
        )

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

        request = kwargs.get("request")
        base_url = str(request.base_url).rstrip("/") if request else ""

        items, total = self._federated_search(filter_props, limit, offset)
        patched = [_inject_item_links(it, base_url) for it in items]

        return _make_item_collection(
            patched, total, limit,
            offset=offset,
            base_url=base_url,
            search_path=f"/collections/{collection_id}/items",
        )

    def get_item(self, item_id: str, collection_id: str, **kwargs) -> stac.Item:
        """GET /collections/{collection_id}/items/{item_id}"""
        request = kwargs.get("request")
        base_url = str(request.base_url).rstrip("/") if request else ""

        dbs = self._open_catalogs()
        try:
            for db in dbs:
                results, _ = db.search_items(
                    {"collection": collection_id, "id": item_id}, limit=1
                )
                if results:
                    return stac.Item(**_inject_item_links(results[0], base_url))
        finally:
            self._close_catalogs(dbs)

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

        # Support single or multiple collections
        if collections:
            if len(collections) == 1:
                filter_props["collection"] = collections[0]
            else:
                filter_props["collection"] = ("IN", collections)

        # Support single or multiple IDs
        if ids:
            if len(ids) == 1:
                filter_props["id"] = ids[0]
            else:
                filter_props["id"] = ("IN", ids)

        filter_props.update(_parse_datetime_filter(datetime))

        request = kwargs.get("request")
        base_url = str(request.base_url).rstrip("/") if request else ""
        token = kwargs.get("token", "")
        offset = int(token) if token and str(token).isdigit() else 0

        return self._run_search(filter_props, limit or 10, base_url, offset=offset)

    def post_search(
        self, search_request: BaseSearchPostRequest, **kwargs
    ) -> stac.ItemCollection:
        """POST /search - handles standard fields plus CQL2 ``filter``."""
        filter_props: dict = {}

        # Support single or multiple collections
        if search_request.collections:
            if len(search_request.collections) == 1:
                filter_props["collection"] = search_request.collections[0]
            else:
                filter_props["collection"] = ("IN", search_request.collections)

        # Support single or multiple IDs
        if search_request.ids:
            if len(search_request.ids) == 1:
                filter_props["id"] = search_request.ids[0]
            else:
                filter_props["id"] = ("IN", search_request.ids)

        filter_props.update(_parse_datetime_filter(search_request.datetime))

        offset = 0
        search_body: dict | None = None
        if isinstance(search_request, FilteredSearchPostRequest):
            # CQL2-JSON filter from STAC Browser "Additional filters" builder
            if search_request.filter:
                filter_props.update(_parse_cql2_json(search_request.filter))
            # Pagination token encodes the integer offset
            if search_request.token and search_request.token.isdigit():
                offset = int(search_request.token)
            # Build a serializable body for pagination next/prev links
            search_body = {}
            if search_request.filter:
                search_body["filter"] = search_request.filter
                search_body["filter-lang"] = search_request.filter_lang or "cql2-json"
            if search_request.collections:
                search_body["collections"] = list(search_request.collections)
            if search_request.datetime:
                search_body["datetime"] = search_request.datetime

        request = kwargs.get("request")
        base_url = str(request.base_url).rstrip("/") if request else ""
        limit = search_request.limit or 10
        if search_body is not None:
            search_body["limit"] = limit

        return self._run_search(
            filter_props, limit, base_url,
            offset=offset, method="POST", search_body=search_body,
        )

    # ------------------------------------------------------------------
    # Internal search
    # ------------------------------------------------------------------

    def _run_search(
        self,
        filter_props: dict,
        limit: int,
        base_url: str = "",
        offset: int = 0,
        method: str = "GET",
        search_body: dict | None = None,
    ) -> stac.ItemCollection:
        """Execute a search with correct federated pagination."""
        items, total = self._federated_search(filter_props, limit, offset)
        patched = [_inject_item_links(it, base_url) for it in items]

        return _make_item_collection(
            patched, total, limit,
            offset=offset, base_url=base_url, method=method, search_body=search_body,
        )
