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
from pydantic import Field
from stac_fastapi.types import stac
from stac_fastapi.types.core import BASE_CONFORMANCE_CLASSES, BaseCoreClient
from stac_fastapi.types.search import BaseSearchPostRequest

from esm_catalog.storage.duckdb import CatalogDB

# Extra conformance classes beyond the stac-fastapi defaults.
# STAC Browser 5 checks for the OGC CQL2 classes to decide whether to show
# the "Additional filters" builder, in addition to the STAC-API filter class.
_EXTRA_CONFORMANCE = [
    "https://api.stacspec.org/v1.0.0/collection-search",
    "https://api.stacspec.org/v1.0.0/collection-search#filter",
    "https://api.stacspec.org/v1.0.0/item-search#filter",
    # OGC API – Features Part 3 (CQL2) conformance classes
    "http://www.opengis.net/spec/ogcapi-features-3/1.0/conf/filter",
    "http://www.opengis.net/spec/ogcapi-features-3/1.0/conf/features-filter",
    "http://www.opengis.net/spec/cql2/1.0/conf/cql2-text",
    "http://www.opengis.net/spec/cql2/1.0/conf/cql2-json",
]

# OGC rel for queryables link — STAC Browser checks for this exact URI
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

    Handles AND combinations and comparison operators.  OR/NOT are silently
    ignored (treated as no-op) because our storage layer only supports AND.

    Returns:
        Dict mapping field name → (sql_op, value) suitable for
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

    sql_op = _CQL2_OP_MAP.get(op)
    if sql_op and len(args) == 2:
        left, right = args
        if isinstance(left, dict) and "property" in left:
            return {left["property"]: (sql_op, _cql2_value(right))}
        if isinstance(right, dict) and "property" in right:
            # Reversed — invert the operator
            inv = _CQL2_OP_INVERSE.get(sql_op, sql_op)
            return {right["property"]: (inv, _cql2_value(left))}

    # OR, NOT, spatial ops — return empty (no-op filter)
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
    # Opaque pagination token — encodes the integer offset of the next page.
    # STAC Browser follows the ``next`` link body which includes this field.
    token: Optional[str] = Field(default=None)

    model_config = {"populate_by_name": True}


def _inject_item_links(item: dict, base_url: str) -> dict:
    """Return a copy of *item* with absolute self, root, parent and collection links.

    Items stored in the DB have only a fragment ``collection`` link
    (``href: "#collection-id"``).  STAC Browser needs absolute URLs to render
    item cards and navigate to the parent collection.  A ``parent`` link
    (= the collection) is added so STAC validation passes.

    Asset hrefs that are bare filesystem paths (starting with ``/``) are
    prefixed with ``file://`` so they satisfy the ``iri-reference`` format
    required by the STAC JSON schema.
    """
    item = dict(item)
    cid = item.get("collection", "")
    iid = item.get("id", "")

    # Inject collection ID as a keyword so STAC Browser renders it as a
    # colored chip/badge on item cards.  This keeps the item ID as the
    # primary heading while still showing which collection an item belongs
    # to — useful when viewing cross-collection search results.
    props = item.get("properties", {})
    if cid:
        item["properties"] = dict(props)
        existing = list(props.get("keywords", []) or [])
        if cid not in existing:
            existing.insert(0, cid)
        item["properties"]["keywords"] = existing

    item["links"] = [
        lnk for lnk in item.get("links", [])
        if lnk.get("rel") not in ("self", "root", "parent", "collection")
    ]
    item["links"].extend([
        {"rel": "self",       "type": "application/geo+json", "href": f"{base_url}/collections/{cid}/items/{iid}"},
        {"rel": "root",       "type": "application/json",     "href": f"{base_url}/"},
        {"rel": "parent",     "type": "application/json",     "href": f"{base_url}/collections/{cid}"},
        {"rel": "collection", "type": "application/json",     "href": f"{base_url}/collections/{cid}"},
    ])
    # Fix asset hrefs: bare absolute paths must be proper file:// URIs
    if item.get("assets"):
        fixed_assets = {}
        for key, asset in item["assets"].items():
            href = asset.get("href", "")
            if href.startswith("/"):
                asset = dict(asset)
                asset["href"] = "file://" + href
            fixed_assets[key] = asset
        item["assets"] = fixed_assets

    # Rewrite the HPC extension URL to our local schema endpoint so STAC
    # Browser can actually fetch and validate against it.
    _HPC_GITHUB_URL = "https://esm-tools.github.io/stac-hpc-extension/v0.1.0/schema.json"
    _hpc_local = f"{base_url}/stac-extensions/hpc/v0.1.0/schema.json"
    if base_url and _HPC_GITHUB_URL in item.get("stac_extensions", []):
        item["stac_extensions"] = [
            _hpc_local if u == _HPC_GITHUB_URL else u
            for u in item["stac_extensions"]
        ]

    return item


def _inject_collection_links(col: dict, base_url: str) -> dict:
    """Return a copy of *col* with self, root, parent, and items links set.

    Collections stored in DuckDB have only a fragment ``parent`` link
    (e.g. ``href: "#basic-001"``).  Replace it with the absolute root URL so
    the STAC Browser "Up" button navigates to the landing page and STAC
    validation passes (fragment-only hrefs fail the iri-reference format check).
    """
    col = dict(col)
    cid = col["id"]
    # Strip all links we manage so stale / fragment hrefs don't leak through
    col["links"] = [
        lnk for lnk in col.get("links", [])
        if lnk.get("rel") not in ("self", "root", "parent", "items")
    ]
    col["links"].extend([
        {"rel": "self",   "type": "application/json",     "href": f"{base_url}/collections/{cid}"},
        {"rel": "root",   "type": "application/json",     "href": f"{base_url}/"},
        {"rel": "parent", "type": "application/json",     "href": f"{base_url}/"},
        {"rel": "items",  "type": "application/geo+json", "href": f"{base_url}/collections/{cid}/items",
         "title": "Items"},
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
                prev_href = f"{href_base}?token={prev_offset}&limit={limit}" if prev_offset > 0 else href_base
                links.append({"rel": "prev", "type": "application/geo+json", "href": prev_href})

        # next
        next_offset = offset + returned
        if next_offset < total:
            if method == "POST" and search_body is not None:
                next_body = {**{k: v for k, v in search_body.items() if k != "token"},
                             "token": str(next_offset)}
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

    Attributes:
        catalogs: Paths to ``catalog.duckdb`` files to serve.  At least one
                  must be provided; an empty list raises :exc:`ValueError`.
    """

    catalogs: List[Union[str, Path]] = attr.ib(factory=list)
    base_conformance_classes: List[str] = attr.ib(
        factory=lambda: BASE_CONFORMANCE_CLASSES + _EXTRA_CONFORMANCE
    )

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
    # Landing page — override to add child links for STAC Browser Browse
    # ------------------------------------------------------------------

    def landing_page(self, **kwargs) -> stac.LandingPage:
        """Extend the default landing page with per-collection child links.

        STAC Browser Browse mode navigates via ``child`` links.  stac-fastapi's
        default landing page only includes a ``data`` link to ``/collections``;
        without explicit ``child`` links the Browse view is empty.
        """
        lp = super().landing_page(**kwargs)
        request = kwargs.get("request")
        base_url = str(request.base_url).rstrip("/") if request else ""

        # Queryables link — STAC Browser checks for the full OGC rel URI
        lp["links"].append({
            "rel": _OGC_QUERYABLES_REL,
            "type": "application/schema+json",
            "title": "Queryables",
            "href": f"{base_url}/queryables",
        })

        dbs = self._open_catalogs()
        try:
            seen: set[str] = set()
            for db in dbs:
                for col in db.iter_collections():
                    if col["id"] in seen:
                        continue
                    seen.add(col["id"])
                    lp["links"].append({
                        "rel": "child",
                        "type": "application/json",
                        "title": col.get("title", col["id"]),
                        "href": f"{base_url}/collections/{col['id']}",
                    })
        finally:
            for db in dbs:
                db.close()
        return lp

    # ------------------------------------------------------------------
    # BaseCoreClient abstract methods
    # ------------------------------------------------------------------

    def all_collections(self, **kwargs) -> stac.Collections:
        """GET /collections — return all collections across all catalogs.

        Supports CQL2-JSON filtering via the ``filter`` query parameter so
        STAC Browser "Search for Collections → Additional filters" works.
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
                    except Exception:
                        pass

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
        request = kwargs.get("request")
        base_url = str(request.base_url).rstrip("/") if request else ""

        # stac-fastapi does not forward unknown query params via the method
        # signature, so read token (and limit) directly from the request.
        if request is not None:
            qp = request.query_params
            if token is None:
                token = qp.get("token")
            raw_limit = qp.get("limit")
            if raw_limit and raw_limit.isdigit():
                limit = int(raw_limit)

        offset = int(token) if token and str(token).isdigit() else 0
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

        patched = [_inject_item_links(it, base_url) for it in items[:limit]]
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
        request = kwargs.get("request")
        base_url = str(request.base_url).rstrip("/") if request else ""
        token = kwargs.get("token", "")
        offset = int(token) if token and str(token).isdigit() else 0
        return self._run_search(filter_props, limit or 10, base_url, offset=offset)

    def post_search(
        self, search_request: BaseSearchPostRequest, **kwargs
    ) -> stac.ItemCollection:
        """POST /search — handles standard fields plus CQL2 ``filter``."""
        filter_props: dict = {}
        if search_request.collections and len(search_request.collections) == 1:
            filter_props["collection"] = search_request.collections[0]
        if search_request.ids and len(search_request.ids) == 1:
            filter_props["id"] = search_request.ids[0]
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
                search_body["collections"] = search_request.collections
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
        patched = [_inject_item_links(it, base_url) for it in items[:limit]]
        return _make_item_collection(
            patched, total, limit,
            offset=offset, base_url=base_url, method=method, search_body=search_body,
        )
