"""A thin STAC Transaction/Bulk-Transaction client over httpx.

Writes go through the STAC API (and therefore its auth proxy), so they are
authenticated and role-gated. Two granularities:

- :meth:`StacClient.upsert_collection` / :meth:`StacClient.upsert_item` — single
  objects, create-or-update (``POST``; on 409 conflict, ``PUT``).
- :meth:`StacClient.bulk_items` — a batch of Items for one collection via the
  Bulk Transactions extension (``POST .../bulk_items``), which pgstac bulk-loads
  server-side. This is the efficient path for stac-geoparquet shards.
"""

from __future__ import annotations

from types import TracebackType
from typing import Any, Literal, Optional

import httpx

from esm_catalog.auth import AccessToken
from esm_catalog.config import Url

#: A STAC object (Collection or Item) as a plain JSON dict.
StacObject = dict[str, Any]

#: A STAC Collection id. A documented alias (not a NewType): ids surface out of
#: ``StacObject`` dict reads, where a NewType would only add cast() noise.
CollectionId = str

#: How ``bulk_items`` treats existing ids: fail on conflict, or overwrite.
BulkMethod = Literal["insert", "upsert"]


class StacClientError(RuntimeError):
    """A STAC API write returned a non-success status."""

    def __init__(self, action: str, status: int, body: str) -> None:
        super().__init__(f"{action} failed (HTTP {status}): {body}")
        self.status = status


class StacClient:
    """Authenticated client for STAC transaction endpoints.

    Parameters
    ----------
    api_url:
        STAC API base, e.g. ``https://stac-dev.dmawi.de/api`` (no trailing slash).
    token:
        OAuth access token; sent as ``Authorization: Bearer``.
    verify_tls:
        Verify the server certificate (disable only for dev self-signed).
    """

    def __init__(
        self,
        api_url: Url,
        token: AccessToken,
        verify_tls: bool = True,
        transport: Optional[httpx.BaseTransport] = None,
    ) -> None:
        # Absolute URLs are built from this base; we deliberately do not use
        # httpx's base_url, whose RFC-3986 join drops the path prefix for
        # leading-slash request paths (…/api would silently vanish).
        self._base = api_url.rstrip("/")
        self._client = httpx.Client(
            headers={"Authorization": f"Bearer {token}"},
            verify=verify_tls,
            timeout=60,
            transport=transport,
        )

    def __enter__(self) -> "StacClient":
        return self

    def __exit__(
        self,
        exc_type: Optional[type[BaseException]],
        exc: Optional[BaseException],
        tb: Optional[TracebackType],
    ) -> None:
        self.close()

    def close(self) -> None:
        self._client.close()

    # ----------------------------------------------------------------- #
    # Single-object upserts (POST, then PUT on 409).
    # ----------------------------------------------------------------- #

    def upsert_collection(self, collection: StacObject) -> None:
        """Create *collection*, or update it if it already exists."""
        cid = collection["id"]
        self._upsert("collection", collection, "/collections", f"/collections/{cid}")

    def upsert_item(self, item: StacObject) -> None:
        """Create *item*, or update it if it already exists.

        The Item must carry a ``collection`` id; its target collection must
        already exist on the server.
        """
        cid = item.get("collection")
        if not cid:
            raise ValueError(f"item {item.get('id')!r} has no 'collection' field")
        iid = item["id"]
        self._upsert(
            "item",
            item,
            f"/collections/{cid}/items",
            f"/collections/{cid}/items/{iid}",
        )

    def _check(self, resp: httpx.Response, action: str) -> None:
        """Raise :class:`StacClientError` unless *resp* is a 200/201 success."""
        if resp.status_code not in (200, 201):
            raise StacClientError(action, resp.status_code, resp.text)

    def _upsert(self, kind: str, body: StacObject, post_path: str, put_path: str) -> None:
        resp = self._client.post(self._base + post_path, json=body)
        if resp.status_code == 409:
            resp = self._client.put(self._base + put_path, json=body)
        self._check(resp, f"upsert {kind} {body.get('id')!r}")

    # ----------------------------------------------------------------- #
    # Bulk items (Bulk Transactions extension).
    # ----------------------------------------------------------------- #

    def bulk_items(
        self,
        collection_id: CollectionId,
        items: list[StacObject],
        method: BulkMethod = "upsert",
    ) -> None:
        """Upsert a batch of Items into *collection_id* in one request.

        Items are keyed by their id, as the Bulk Transactions extension expects.
        The collection must already exist on the server.
        """
        if not items:
            return
        payload = {"items": {item["id"]: item for item in items}, "method": method}
        resp = self._client.post(
            f"{self._base}/collections/{collection_id}/bulk_items", json=payload
        )
        self._check(resp, f"bulk_items into {collection_id!r} ({len(items)} items)")
