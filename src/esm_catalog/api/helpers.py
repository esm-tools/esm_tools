"""Common helper functions for the ESM-Catalog STAC API.

This module provides reusable utilities for link injection, datetime parsing,
and pagination across the STAC API endpoints.

Functions:
    inject_item_links: Add self, root, and collection links to STAC items.
    inject_collection_links: Add self, root, and items links to STAC collections.
    parse_datetime_filter: Convert STAC datetime parameter to filter dict.
    build_pagination_links: Build first, prev, next pagination links.

Example:
    >>> from esm_catalog.api.helpers import inject_item_links, parse_datetime_filter
    >>> item = {"id": "item1", "collection": "my-collection", "links": []}
    >>> patched = inject_item_links(item, "https://api.example.com")
    >>> [lnk["rel"] for lnk in patched["links"]]
    ['self', 'root', 'collection']
"""

from __future__ import annotations

from typing import Any


def inject_item_links(item: dict, base_url: str) -> dict:
    """Inject absolute self, root, and collection links into a STAC item.

    Items stored in the database typically have only fragment links
    (e.g., ``href: "#collection-id"``). STAC clients like STAC Browser
    need absolute URLs to render item cards and navigate to parent collections.

    Args:
        item: A STAC item dictionary. Must contain ``id`` and ``collection`` keys.
        base_url: The base URL of the STAC API (e.g., ``"https://api.example.com"``).
            Should not have a trailing slash.

    Returns:
        A shallow copy of the item with updated links. Existing self, root,
        and collection links are replaced with absolute URLs.

    Example:
        >>> item = {
        ...     "id": "tas_daily_2000",
        ...     "collection": "climate-data",
        ...     "links": [{"rel": "license", "href": "https://example.com/license"}]
        ... }
        >>> result = inject_item_links(item, "https://api.example.com")
        >>> result["links"]  # doctest: +NORMALIZE_WHITESPACE
        [{'rel': 'license', 'href': 'https://example.com/license'},
         {'rel': 'self', 'type': 'application/geo+json',
          'href': 'https://api.example.com/collections/climate-data/items/tas_daily_2000'},
         {'rel': 'root', 'type': 'application/json',
          'href': 'https://api.example.com/'},
         {'rel': 'collection', 'type': 'application/json',
          'href': 'https://api.example.com/collections/climate-data'}]
    """
    item = dict(item)
    collection_id = item.get("collection", "")
    item_id = item.get("id", "")

    # Remove any existing self, root, collection links (will be replaced)
    item["links"] = [
        lnk
        for lnk in item.get("links", [])
        if lnk.get("rel") not in ("self", "root", "collection")
    ]

    # Add absolute links
    item["links"].extend(
        [
            {
                "rel": "self",
                "type": "application/geo+json",
                "href": f"{base_url}/collections/{collection_id}/items/{item_id}",
            },
            {
                "rel": "root",
                "type": "application/json",
                "href": f"{base_url}/",
            },
            {
                "rel": "collection",
                "type": "application/json",
                "href": f"{base_url}/collections/{collection_id}",
            },
        ]
    )

    return item


def inject_collection_links(collection: dict, base_url: str) -> dict:
    """Inject absolute self, root, and items links into a STAC collection.

    stac-fastapi stores collections with minimal links. STAC Browser needs
    an ``items`` link to know where to fetch items for a collection.

    Args:
        collection: A STAC collection dictionary. Must contain an ``id`` key.
        base_url: The base URL of the STAC API (e.g., ``"https://api.example.com"``).
            Should not have a trailing slash.

    Returns:
        A shallow copy of the collection with updated links. Existing self,
        root, and items links are replaced with absolute URLs.

    Example:
        >>> collection = {
        ...     "id": "climate-data",
        ...     "title": "Climate Data Collection",
        ...     "links": [{"rel": "parent", "href": "../"}]
        ... }
        >>> result = inject_collection_links(collection, "https://api.example.com")
        >>> result["links"]  # doctest: +NORMALIZE_WHITESPACE
        [{'rel': 'parent', 'href': '../'},
         {'rel': 'self', 'type': 'application/json',
          'href': 'https://api.example.com/collections/climate-data'},
         {'rel': 'root', 'type': 'application/json',
          'href': 'https://api.example.com/'},
         {'rel': 'items', 'type': 'application/geo+json',
          'href': 'https://api.example.com/collections/climate-data/items',
          'title': 'Items'}]
    """
    collection = dict(collection)
    collection_id = collection["id"]

    # Remove any existing self, root, items links (will be replaced)
    collection["links"] = [
        lnk
        for lnk in collection.get("links", [])
        if lnk.get("rel") not in ("self", "root", "items")
    ]

    # Add absolute links
    collection["links"].extend(
        [
            {
                "rel": "self",
                "type": "application/json",
                "href": f"{base_url}/collections/{collection_id}",
            },
            {
                "rel": "root",
                "type": "application/json",
                "href": f"{base_url}/",
            },
            {
                "rel": "items",
                "type": "application/geo+json",
                "href": f"{base_url}/collections/{collection_id}/items",
                "title": "Items",
            },
        ]
    )

    return collection


def parse_datetime_filter(datetime_str: str | None) -> dict[str, tuple[str, str]]:
    """Convert a STAC datetime parameter to filter property entries.

    The STAC API datetime parameter supports single timestamps and ranges.
    This function converts these into a dictionary suitable for database
    filtering with comparison operators.

    Args:
        datetime_str: A STAC datetime string. Can be:
            - ``None`` or empty string: Returns empty dict.
            - Single timestamp: ``"2000-01-01T00:00:00Z"``
            - Open-ended range: ``"2000-01-01T00:00:00Z/.."`` (start only)
              or ``"../2005-12-31T23:59:59Z"`` (end only)
            - Closed range: ``"2000-01-01/2005-12-31"``

    Returns:
        A dictionary with filter entries. Keys are ``"datetime"`` for start
        constraints and ``"datetime_end"`` for end constraints. Values are
        tuples of ``(operator, value)``.

    Example:
        >>> parse_datetime_filter(None)
        {}

        >>> parse_datetime_filter("2000-01-01T00:00:00Z")
        {'datetime': ('=', '2000-01-01T00:00:00Z')}

        >>> parse_datetime_filter("2000-01-01T00:00:00Z/..")
        {'datetime': ('>=', '2000-01-01T00:00:00Z')}

        >>> parse_datetime_filter("../2005-12-31T23:59:59Z")
        {'datetime_end': ('<=', '2005-12-31T23:59:59Z')}

        >>> result = parse_datetime_filter("2000-01-01/2005-12-31")
        >>> result['datetime']
        ('>=', '2000-01-01')
        >>> result['datetime_end']
        ('<=', '2005-12-31')
    """
    if not datetime_str:
        return {}

    if "/" in datetime_str:
        parts = datetime_str.split("/", 1)
        start, end = parts[0], parts[1]
        result: dict[str, tuple[str, str]] = {}

        if start and start != "..":
            result["datetime"] = (">=", start)
        if end and end != "..":
            result["datetime_end"] = ("<=", end)

        return result

    # Single timestamp - exact match
    return {"datetime": ("=", datetime_str)}


def build_pagination_links(
    total: int,
    limit: int,
    offset: int,
    base_url: str,
    search_path: str,
    method: str = "GET",
    search_body: dict[str, Any] | None = None,
) -> list[dict[str, Any]]:
    """Build pagination links for STAC search results.

    Creates ``first``, ``prev``, and ``next`` links following the STAC API
    pagination specification. Supports both GET (query params) and POST
    (body with token) methods.

    Args:
        total: Total number of matched items across all results.
        limit: Maximum items per page.
        offset: Current offset into the result set.
        base_url: The base URL of the STAC API (e.g., ``"https://api.example.com"``).
            Should not have a trailing slash.
        search_path: The path to the search endpoint (e.g., ``"/search"`` or
            ``"/collections/my-collection/items"``).
        method: HTTP method - ``"GET"`` for query parameter pagination or
            ``"POST"`` for body-based pagination. Defaults to ``"GET"``.
        search_body: For POST requests, the search body dict to include in
            pagination links. The ``token`` field will be managed automatically.
            Ignored for GET requests.

    Returns:
        A list of link dictionaries with ``rel``, ``type``, ``href``, and
        optionally ``method`` and ``body`` keys.

    Example:
        >>> # GET pagination
        >>> links = build_pagination_links(
        ...     total=100, limit=10, offset=20,
        ...     base_url="https://api.example.com",
        ...     search_path="/search"
        ... )
        >>> [lnk["rel"] for lnk in links]
        ['first', 'prev', 'next']

        >>> # First page - no prev link
        >>> links = build_pagination_links(
        ...     total=100, limit=10, offset=0,
        ...     base_url="https://api.example.com",
        ...     search_path="/search"
        ... )
        >>> [lnk["rel"] for lnk in links]
        ['first', 'next']

        >>> # Last page - no next link
        >>> links = build_pagination_links(
        ...     total=100, limit=10, offset=90,
        ...     base_url="https://api.example.com",
        ...     search_path="/search"
        ... )
        >>> [lnk["rel"] for lnk in links]
        ['first', 'prev']

        >>> # POST pagination with body
        >>> links = build_pagination_links(
        ...     total=50, limit=10, offset=10,
        ...     base_url="https://api.example.com",
        ...     search_path="/search",
        ...     method="POST",
        ...     search_body={"collections": ["data"], "limit": 10}
        ... )
        >>> links[0]["method"]
        'POST'
        >>> "body" in links[0]
        True
    """
    if not base_url:
        return []

    links: list[dict[str, Any]] = []
    href_base = f"{base_url}{search_path}"

    # First link
    if method == "POST" and search_body is not None:
        first_body = {k: v for k, v in search_body.items() if k != "token"}
        links.append(
            {
                "rel": "first",
                "type": "application/geo+json",
                "method": "POST",
                "href": href_base,
                "body": first_body,
            }
        )
    else:
        links.append(
            {
                "rel": "first",
                "type": "application/geo+json",
                "href": href_base,
            }
        )

    # Prev link (only if not on first page)
    if offset > 0:
        prev_offset = max(0, offset - limit)
        if method == "POST" and search_body is not None:
            prev_body = {k: v for k, v in search_body.items() if k != "token"}
            if prev_offset > 0:
                prev_body["token"] = str(prev_offset)
            links.append(
                {
                    "rel": "prev",
                    "type": "application/geo+json",
                    "method": "POST",
                    "href": href_base,
                    "body": prev_body,
                }
            )
        else:
            if prev_offset > 0:
                prev_href = f"{href_base}?token={prev_offset}&limit={limit}"
            else:
                prev_href = href_base
            links.append(
                {
                    "rel": "prev",
                    "type": "application/geo+json",
                    "href": prev_href,
                }
            )

    # Next link (only if more items remain)
    # Note: We calculate based on offset, not items returned, since caller
    # should pass the current offset and we assume limit items were requested
    next_offset = offset + limit
    if next_offset < total:
        if method == "POST" and search_body is not None:
            next_body = {
                **{k: v for k, v in search_body.items() if k != "token"},
                "token": str(next_offset),
            }
            links.append(
                {
                    "rel": "next",
                    "type": "application/geo+json",
                    "method": "POST",
                    "href": href_base,
                    "body": next_body,
                }
            )
        else:
            links.append(
                {
                    "rel": "next",
                    "type": "application/geo+json",
                    "href": f"{href_base}?token={next_offset}&limit={limit}",
                }
            )

    return links
