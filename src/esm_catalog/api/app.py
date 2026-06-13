"""STAC API application factory.

Builds a ``stac-fastapi`` application backed by DuckDB catalog files.

Typical use::

    from esm_catalog.api.app import create_app
    api = create_app(catalogs=["/work/exp/catalog.duckdb"])
    # pass api.app to uvicorn

    # With dynamic catalog management
    api = create_app(
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
from typing import List, Union

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import RedirectResponse
from stac_fastapi.api.app import StacApi
from stac_fastapi.types.config import ApiSettings
from starlette.middleware import Middleware
from starlette.requests import Request

from esm_catalog.api.cache import CollectionCache
from esm_catalog.api.client import (
    DuckDBCatalogClient,
    FilteredSearchPostRequest,
    ItemCollectionUriWithToken,
)
from esm_catalog.api.pool import CatalogPool
from esm_catalog.api.registry import CatalogRegistry

_DEFAULT_TITLE = "ESM-Tools STAC Catalog"
_DEFAULT_DESCRIPTION = (
    "STAC API for ESM-Tools experiment output. "
    "Backed by DuckDB per-experiment catalogs."
)
_DEFAULT_VERSION = "1.0"


def create_app(
    catalogs: List[Union[str, Path]] | None = None,
    registry_persist_path: str | Path | None = None,
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

    registry = CatalogRegistry(
        initial_catalogs=[str(c) for c in catalogs],
        persist_path=registry_persist_path,
    )
    pool = CatalogPool()
    collection_cache = CollectionCache(ttl_seconds=300)

    settings = ApiSettings(
        stac_fastapi_title=title,
        stac_fastapi_description=description,
        stac_fastapi_version=version,
        stac_fastapi_landing_id="esm-tools-stac",
    )

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

    # stac-fastapi's middlewares parameter is not always reliable; add CORS
    # directly to the FastAPI app to guarantee the headers are present.
    api.app.add_middleware(
        CORSMiddleware,
        allow_origins=cors_origins,
        allow_credentials=True,
        allow_methods=["GET", "POST", "PATCH", "DELETE", "OPTIONS"],
        allow_headers=["*"],
    )

    # POST /format - OGC CQL2 format-negotiation probe issued by STAC Browser.
    @api.app.post("/format", response_model=None, include_in_schema=False)
    async def cql2_format(request: Request):
        return {}

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

    # Register lifespan handler for cleanup on shutdown
    original_lifespan = api.app.router.lifespan_context

    @asynccontextmanager
    async def lifespan(app: FastAPI):
        if original_lifespan is not None:
            async with original_lifespan(app):
                yield
        else:
            yield
        pool.close_all()

    api.app.router.lifespan_context = lifespan

    # Serve catalog management UI at /ui (optional — only if directory exists)
    try:
        from fastapi.staticfiles import StaticFiles

        _ui_dir = Path(__file__).parent / "ui"
        if _ui_dir.is_dir():
            api.app.mount(
                "/ui", StaticFiles(directory=str(_ui_dir), html=True), name="ui"
            )
    except ImportError:
        pass

    @api.app.api_route("/admin", methods=["GET", "HEAD"], include_in_schema=False)
    def admin_redirect():
        return RedirectResponse(url="/ui")

    return api


def _app_from_env():
    db_env = os.environ.get("ESM_CATALOG_DB", "")
    registry_env = os.environ.get("ESM_CATALOG_REGISTRY", "")

    catalogs = [p for p in db_env.split(":") if p] if db_env else []
    if not catalogs:
        default = Path("catalog.duckdb")
        if default.exists():
            catalogs = [str(default)]
        else:
            catalogs = []

    registry_path = registry_env if registry_env else None

    api = create_app(
        catalogs=catalogs,
        registry_persist_path=registry_path,
    )
    return api.app


_app_singleton = None


def __getattr__(name: str):
    if name == "app":
        global _app_singleton
        if _app_singleton is None:
            _app_singleton = _app_from_env()
        return _app_singleton
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")
