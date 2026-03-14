"""STAC API application factory.

Builds a ``stac-fastapi`` application backed by DuckDB catalog files.

Typical use::

    # From the CLI (esm-catalog serve ...)
    from esm_catalog.api.app import create_app
    app = create_app(catalogs=["/work/exp/catalog.duckdb"])

    # With dynamic catalog management
    from esm_catalog.api.app import create_app
    app = create_app(
        catalogs=["/work/exp/catalog.duckdb"],
        registry_persist_path="/var/lib/esm-catalog/registry.json",
    )

    # Direct uvicorn run:
    #   uvicorn esm_catalog.api.app:app
    # (uses ESM_CATALOG_DB env var or the default path)
"""

from __future__ import annotations

import os
from contextlib import asynccontextmanager
from pathlib import Path
from typing import TYPE_CHECKING, List, Union

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from stac_fastapi.api.app import StacApi
from stac_fastapi.types.config import ApiSettings
from starlette.middleware import Middleware
from starlette.requests import Request

from esm_catalog.api.auth import Authenticator, NoAuthenticator
from esm_catalog.api.cache import CollectionCache, QueryablesCache
from esm_catalog.api.catalog_routes import create_catalog_router
from esm_catalog.api.client import (
    DuckDBCatalogClient,
    FilteredSearchPostRequest,
    ItemCollectionUriWithToken,
)
from esm_catalog.api.pool import CatalogPool
from esm_catalog.api.queryables import get_collection_queryables, get_queryables
from esm_catalog.api.registry import CatalogRegistry

if TYPE_CHECKING:
    pass

_DEFAULT_TITLE = "ESM-Tools STAC Catalog"
_DEFAULT_DESCRIPTION = (
    "STAC API for ESM-Tools experiment output. "
    "Backed by DuckDB per-experiment catalogs."
)
_DEFAULT_VERSION = "1.0"


def create_app(
    catalogs: List[Union[str, Path]] | None = None,
    registry_persist_path: str | Path | None = None,
    authenticator: Authenticator | None = None,
    title: str = _DEFAULT_TITLE,
    description: str = _DEFAULT_DESCRIPTION,
    version: str = _DEFAULT_VERSION,
    cors_origins: List[str] | None = None,
) -> StacApi:
    """Create and return a configured :class:`~stac_fastapi.api.app.StacApi` instance.

    Args:
        catalogs: Paths to ``catalog.duckdb`` files to serve initially.
        registry_persist_path: Optional JSON file for persisting dynamic catalog
            registrations. If not provided, catalog changes are in-memory only.
        authenticator: Optional authenticator for access control on catalog
            management endpoints. Defaults to NoAuthenticator (open access).
        title: Landing-page title.
        description: Landing-page description.
        version: API version string.
        cors_origins: List of allowed CORS origins. Defaults to ``["*"]``
            (allow all) which is needed for STAC Browser access.

    Returns:
        A :class:`StacApi` instance ready to be passed to uvicorn.
    """
    if cors_origins is None:
        cors_origins = ["*"]

    if catalogs is None:
        catalogs = []

    # Create shared components
    registry = CatalogRegistry(
        initial_catalogs=[str(c) for c in catalogs],
        persist_path=registry_persist_path,
    )
    pool = CatalogPool()
    auth = authenticator or NoAuthenticator()
    queryables_cache = QueryablesCache(ttl_seconds=300)  # 5 minute cache
    collection_cache = CollectionCache(ttl_seconds=300)  # 5 minute cache

    settings = ApiSettings(
        stac_fastapi_title=title,
        stac_fastapi_description=description,
        stac_fastapi_version=version,
        stac_fastapi_landing_id="esm-tools-stac",
    )

    # Create client with registry and pool for dynamic catalog management
    client = DuckDBCatalogClient(
        registry=registry, pool=pool, collection_cache=collection_cache
    )

    middlewares = [
        Middleware(
            CORSMiddleware,
            allow_origins=cors_origins,
            allow_credentials=True,
            allow_methods=["GET", "POST", "PATCH", "DELETE", "OPTIONS"],
            allow_headers=["*"],
        )
    ]

    api = StacApi(
        settings=settings,
        client=client,
        middlewares=middlewares,
        title=title,
        description=description,
        api_version=version,
        search_post_request_model=FilteredSearchPostRequest,
        items_get_request_model=ItemCollectionUriWithToken,
    )

    # Add catalog management routes
    catalog_router = create_catalog_router(registry, pool, auth, queryables_cache)
    api.app.include_router(catalog_router)

    # /queryables endpoint - required for STAC Browser "Additional Filtering" CQL2 builder.
    # The landing page advertises rel=http://www.opengis.net/def/rel/ogc/1.0/queryables
    # pointing here. We populate enum lists from the live catalogs so STAC Browser
    # shows dropdown pickers rather than free-text fields.
    @api.app.get(
        "/queryables",
        response_model=None,
        include_in_schema=True,
        summary="Queryable properties for CQL2 filtering",
        tags=["STAC API - Filter Extension"],
    )
    def queryables(request: Request):
        # Use cache to avoid expensive DISTINCT queries on every request
        # Cache key is "global" since queryables are the same for all requests
        return queryables_cache.get_or_compute(
            "global",
            lambda: get_queryables(request, registry.get_paths(), pool),
        )

    # Per-collection queryables - enum values scoped to items in that collection
    @api.app.get(
        "/collections/{collection_id}/queryables",
        response_model=None,
        include_in_schema=True,
        summary="Queryable properties for a specific collection",
        tags=["STAC API - Filter Extension"],
    )
    def collection_queryables(collection_id: str, request: Request):
        # Cache per collection
        result = queryables_cache.get_or_compute(
            f"collection:{collection_id}",
            lambda: get_collection_queryables(
                request, collection_id, registry.get_paths(), pool
            ),
        )
        if result is None:
            from fastapi import HTTPException
            raise HTTPException(
                status_code=404,
                detail=f"Collection '{collection_id}' not found",
            )
        return result

    # POST /format - OGC CQL2 format-negotiation probe issued by STAC Browser.
    # Not required for filtering to work (filters travel as CQL2-JSON in /search),
    # but returning 200 silences the 404 log noise.
    @api.app.post(
        "/format",
        response_model=None,
        include_in_schema=False,
    )
    def cql2_format(body: dict | None = None):
        return body or {}

    # Health check endpoint for basic liveness probes
    @api.app.get(
        "/health",
        response_model=None,
        include_in_schema=True,
        summary="Basic health check",
        tags=["System"],
    )
    def health():
        return {
            "status": "ok",
            "catalogs_registered": len(registry),
            "pool_connections": len(pool),
        }

    # Readiness probe for Kubernetes
    @api.app.get(
        "/readiness",
        response_model=None,
        include_in_schema=True,
        summary="Kubernetes readiness probe",
        tags=["System"],
    )
    def readiness():
        paths = registry.get_paths()
        accessible = sum(1 for p in paths if Path(p).exists())
        return {
            "ready": accessible > 0 or len(paths) == 0,
            "catalogs_accessible": accessible,
            "catalogs_total": len(paths),
        }

    # Paleo time presets endpoints for climate scientist UX
    @api.app.get(
        "/paleo-presets",
        response_model=None,
        include_in_schema=True,
        summary="Get available paleo time period presets",
        tags=["Climate Science"],
    )
    def get_paleo_presets():
        """Return available paleo time period presets.

        Presets include commonly simulated periods like LGM, Mid-Holocene, Eemian, etc.
        These can be used for filtering collections by paleo time period.
        """
        from esm_catalog.api.paleo_presets import get_presets

        return {"presets": get_presets()}

    @api.app.post(
        "/paleo-presets",
        response_model=None,
        include_in_schema=True,
        summary="Add a user-defined paleo time preset",
        tags=["Climate Science"],
    )
    def add_paleo_preset(preset: dict):
        """Add a user-defined paleo time preset.

        Required fields in body:
        - id: Unique identifier (e.g., "my_period")
        - name: Human-readable name (e.g., "My Custom Period")
        - display: Display string (e.g., "15.0 ka")
        - years_bp: Years before present (1950 CE)
        - description: Optional description
        """
        from esm_catalog.api.paleo_presets import add_preset

        result = add_preset(
            preset_id=preset.get("id", ""),
            name=preset.get("name", ""),
            display=preset.get("display", ""),
            years_bp=preset.get("years_bp", 0),
            description=preset.get("description", ""),
        )
        return {"status": "created", "preset": result}

    @api.app.delete(
        "/paleo-presets/{preset_id}",
        response_model=None,
        include_in_schema=True,
        summary="Delete a user-added paleo time preset",
        tags=["Climate Science"],
    )
    def delete_paleo_preset(preset_id: str):
        """Delete a user-added paleo time preset.

        Only user-added presets can be deleted. Built-in presets are protected.
        """
        from esm_catalog.api.paleo_presets import delete_preset

        deleted = delete_preset(preset_id)
        if deleted:
            return {"status": "deleted", "id": preset_id}
        from fastapi import HTTPException

        raise HTTPException(
            status_code=404,
            detail=f"Preset '{preset_id}' not found or is a built-in preset",
        )

    # Register lifespan handler for cleanup on shutdown
    # We wrap any existing lifespan from StacApi to ensure proper chaining
    original_lifespan = api.app.router.lifespan_context

    @asynccontextmanager
    async def lifespan(app: FastAPI):
        # Startup: chain to original lifespan if present
        if original_lifespan is not None:
            async with original_lifespan(app):
                yield
        else:
            yield
        # Shutdown: close all pool connections
        pool.close_all()

    api.app.router.lifespan_context = lifespan

    return api


# ---------------------------------------------------------------------------
# Module-level ``app`` for direct uvicorn invocation:
#   uvicorn esm_catalog.api.app:app [--reload]
#
# Configure via environment variables:
#   ESM_CATALOG_DB       - colon-separated list of catalog.duckdb paths
#   ESM_CATALOG_REGISTRY - path to registry persistence file (optional)
# ---------------------------------------------------------------------------


def _app_from_env():
    db_env = os.environ.get("ESM_CATALOG_DB", "")
    registry_env = os.environ.get("ESM_CATALOG_REGISTRY", "")

    catalogs = [p for p in db_env.split(":") if p] if db_env else []
    if not catalogs:
        # Development fallback: look for catalog.duckdb in cwd
        default = Path("catalog.duckdb")
        if default.exists():
            catalogs = [str(default)]
        else:
            # No default found - start with empty catalog list
            # Server will serve empty results until catalogs are added via API
            catalogs = []

    registry_path = registry_env if registry_env else None

    api = create_app(
        catalogs=catalogs,
        registry_persist_path=registry_path,
    )
    return api.app


app = _app_from_env()
