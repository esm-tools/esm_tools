"""Dynamic catalog management REST API routes.

Provides CRUD endpoints for managing which catalogs are served by the API.
Catalogs can be added, listed, updated, refreshed, and removed at runtime.

Endpoints::

    GET    /catalogs           - List all registered catalogs
    GET    /catalogs/{id}      - Get single catalog info
    POST   /catalogs           - Register a new catalog
    PATCH  /catalogs/{id}      - Update catalog metadata
    POST   /catalogs/{id}/refresh - Refresh catalog connection
    DELETE /catalogs/{id}      - Unregister a catalog

Example usage with curl::

    # List catalogs
    curl http://localhost:8000/catalogs

    # Add a new catalog
    curl -X POST http://localhost:8000/catalogs \\
        -H "Content-Type: application/json" \\
        -d '{"path": "/path/to/catalog.duckdb", "name": "Experiment 1"}'

    # Update catalog metadata
    curl -X PATCH http://localhost:8000/catalogs/abc123 \\
        -H "Content-Type: application/json" \\
        -d '{"name": "Experiment 1 (Updated)"}'

    # Refresh after external modification
    curl -X POST http://localhost:8000/catalogs/abc123/refresh

    # Remove catalog
    curl -X DELETE http://localhost:8000/catalogs/abc123
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from fastapi import APIRouter, HTTPException, Request, Response
from pydantic import BaseModel

from loguru import logger

from esm_catalog.api.registry import (
    CatalogCreateRequest,
    CatalogInfo,
    CatalogRegistry,
    CatalogUpdateRequest,
)
from esm_catalog.api.validation import ValidationError, validate_catalog_path

if TYPE_CHECKING:
    from esm_catalog.api.auth import Authenticator
    from esm_catalog.api.cache import QueryablesCache
    from esm_catalog.api.pool import CatalogPool


class CatalogListResponse(BaseModel):
    """Response for GET /catalogs."""

    catalogs: list[CatalogInfo]
    total: int


class RefreshResponse(BaseModel):
    """Response for POST /catalogs/{id}/refresh."""

    id: str
    path: str
    refreshed: bool
    message: str


def create_catalog_router(
    registry: CatalogRegistry,
    pool: "CatalogPool",
    authenticator: "Authenticator | None" = None,
    queryables_cache: "QueryablesCache | None" = None,
) -> APIRouter:
    """Create the catalog management router with injected dependencies.

    Args:
        registry: Catalog registry instance.
        pool: Connection pool for catalog access.
        authenticator: Optional authenticator for access control.
        queryables_cache: Optional queryables cache to invalidate on changes.

    Returns:
        FastAPI router with catalog CRUD endpoints.
    """
    router = APIRouter(prefix="/catalogs", tags=["Catalog Management"])

    def _invalidate_caches() -> None:
        """Invalidate queryables cache when catalog data changes."""
        if queryables_cache is not None:
            queryables_cache.invalidate()
            logger.debug("Invalidated queryables cache due to catalog change")

    async def _check_permission(
        request: Request, action: str, raise_on_fail: bool = True
    ) -> bool:
        """Check if the current request has permission for the action.

        Args:
            request: HTTP request.
            action: Action being performed (read, write, delete).
            raise_on_fail: If True, raise HTTPException on failure.

        Returns:
            True if permitted.

        Raises:
            HTTPException: If authentication/authorization fails and raise_on_fail is True.
        """
        if authenticator is None:
            return True

        if not authenticator.requires_auth("catalogs", action):
            return True

        user = await authenticator.authenticate(request)
        if user is None:
            if raise_on_fail:
                raise HTTPException(status_code=401, detail="Authentication required")
            return False

        permissions = authenticator.get_permissions(user, "catalogs")
        if action not in permissions:
            if raise_on_fail:
                raise HTTPException(status_code=403, detail="Permission denied")
            return False

        return True

    @router.get("", response_model=CatalogListResponse)
    async def list_catalogs(request: Request) -> CatalogListResponse:
        """List all registered catalogs with their current status.

        Status is refreshed on each request to reflect whether the
        catalog file currently exists on disk.
        """
        await _check_permission(request, "read")
        catalogs = registry.list_catalogs()
        return CatalogListResponse(catalogs=catalogs, total=len(catalogs))

    @router.get("/{catalog_id}", response_model=CatalogInfo)
    async def get_catalog(catalog_id: str, request: Request) -> CatalogInfo:
        """Get a specific catalog by ID.

        Returns the catalog metadata and current status.
        """
        await _check_permission(request, "read")
        info = registry.get_catalog(catalog_id)
        if info is None:
            raise HTTPException(
                status_code=404, detail=f"Catalog '{catalog_id}' not found"
            )
        return info

    @router.post("", response_model=CatalogInfo, status_code=201)
    async def add_catalog(
        body: CatalogCreateRequest, request: Request
    ) -> CatalogInfo:
        """Register a new catalog to be served by the API.

        The catalog file does not need to exist immediately; it will be
        served once it becomes available. Status indicates whether the
        file currently exists.

        Request body:
            - path: Path to the DuckDB catalog file (required)
            - name: Human-readable name (optional, defaults to filename)
            - description: Description text (optional)
        """
        await _check_permission(request, "write")
        try:
            # Validate and normalize the path
            validated_path = validate_catalog_path(body.path)
            # Update the request with the validated path
            body_with_validated_path = CatalogCreateRequest(
                path=validated_path,
                name=body.name,
                description=body.description,
            )
            result = registry.add_catalog(body_with_validated_path)
            _invalidate_caches()
            return result
        except ValidationError as e:
            logger.warning("Invalid catalog path: {}", e)
            raise HTTPException(status_code=400, detail=str(e))
        except ValueError as e:
            raise HTTPException(status_code=409, detail=str(e))

    @router.patch("/{catalog_id}", response_model=CatalogInfo)
    async def update_catalog(
        catalog_id: str, body: CatalogUpdateRequest, request: Request
    ) -> CatalogInfo:
        """Update a catalog's metadata.

        Any field can be updated independently. If the path is changed,
        the old connection will be closed and a new one opened for the
        new path.

        Request body (all optional):
            - path: New path to DuckDB file
            - name: New human-readable name
            - description: New description
        """
        await _check_permission(request, "write")

        # Validate and normalize the path if provided
        validated_body = body
        if body.path is not None:
            try:
                validated_path = validate_catalog_path(body.path)
                validated_body = CatalogUpdateRequest(
                    path=validated_path,
                    name=body.name,
                    description=body.description,
                )
            except ValidationError as e:
                logger.warning("Invalid catalog path: {}", e)
                raise HTTPException(status_code=400, detail=str(e))

        # If path is changing, close old connection first
        if validated_body.path is not None:
            old_path = registry.get_path(catalog_id)
            if old_path and old_path != validated_body.path:
                pool.close(old_path)

        try:
            info = registry.update_catalog(catalog_id, validated_body)
        except ValueError as e:
            raise HTTPException(status_code=409, detail=str(e))

        if info is None:
            raise HTTPException(
                status_code=404, detail=f"Catalog '{catalog_id}' not found"
            )
        _invalidate_caches()
        return info

    @router.post("/{catalog_id}/refresh", response_model=RefreshResponse)
    async def refresh_catalog(catalog_id: str, request: Request) -> RefreshResponse:
        """Force reconnection to a catalog file.

        Use this when the underlying DuckDB file has been modified externally
        (e.g., new items added via CLI scan) and you need the API to pick up
        the changes.

        This closes the pooled connection and invalidates the status cache;
        the next query will open a fresh connection to the file.
        """
        await _check_permission(request, "write")

        info = registry.get_catalog(catalog_id)
        if info is None:
            raise HTTPException(
                status_code=404, detail=f"Catalog '{catalog_id}' not found"
            )

        # Close pooled connection and invalidate status cache
        was_open = pool.close(info.path)
        registry.invalidate_status(catalog_id)

        if was_open:
            message = "Connection closed; will reopen on next query"
        else:
            message = "No active connection; will open on next query"

        _invalidate_caches()
        return RefreshResponse(
            id=catalog_id,
            path=info.path,
            refreshed=True,
            message=message,
        )

    @router.delete("/{catalog_id}", status_code=204)
    async def remove_catalog(catalog_id: str, request: Request) -> Response:
        """Unregister a catalog from the API.

        This does not delete the underlying DuckDB file; it only removes
        it from the set of catalogs served by this API instance.

        The pooled connection is closed if one exists.
        """
        await _check_permission(request, "delete")

        # Close pooled connection first
        info = registry.get_catalog(catalog_id)
        if info:
            pool.close(info.path)

        if not registry.remove_catalog(catalog_id):
            raise HTTPException(
                status_code=404, detail=f"Catalog '{catalog_id}' not found"
            )

        _invalidate_caches()
        return Response(status_code=204)

    return router
