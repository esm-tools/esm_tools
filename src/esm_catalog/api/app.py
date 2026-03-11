"""STAC API application factory.

Builds a ``stac-fastapi`` application backed by DuckDB catalog files.

Typical use::

    # From the CLI (esm-catalog serve ...)
    from esm_catalog.api.app import create_app
    app = create_app(catalogs=["/work/exp/catalog.duckdb"])

    # Direct uvicorn run:
    #   uvicorn esm_catalog.api.app:app
    # (uses ESM_CATALOG_DB env var or the default path)
"""

from __future__ import annotations

import os
from pathlib import Path
from typing import List, Union

from fastapi.middleware.cors import CORSMiddleware
from starlette.middleware import Middleware
from stac_fastapi.api.app import StacApi
from stac_fastapi.types.config import ApiSettings

from esm_catalog.api.client import DuckDBCatalogClient


_DEFAULT_TITLE = "ESM-Tools STAC Catalog"
_DEFAULT_DESCRIPTION = (
    "STAC API for ESM-Tools experiment output. "
    "Backed by DuckDB per-experiment catalogs."
)
_DEFAULT_VERSION = "1.0"


def create_app(
    catalogs: List[Union[str, Path]],
    title: str = _DEFAULT_TITLE,
    description: str = _DEFAULT_DESCRIPTION,
    version: str = _DEFAULT_VERSION,
    cors_origins: List[str] | None = None,
) -> StacApi:
    """Create and return a configured :class:`~stac_fastapi.api.app.StacApi` instance.

    Args:
        catalogs:     Paths to ``catalog.duckdb`` files to serve.
        title:        Landing-page title.
        description:  Landing-page description.
        version:      API version string.
        cors_origins: List of allowed CORS origins.  Defaults to ``["*"]``
                      (allow all) which is needed for STAC Browser access.

    Returns:
        A :class:`StacApi` instance ready to be passed to uvicorn.
    """
    if cors_origins is None:
        cors_origins = ["*"]

    settings = ApiSettings(
        stac_fastapi_title=title,
        stac_fastapi_description=description,
        stac_fastapi_version=version,
        stac_fastapi_landing_id="esm-tools-stac",
    )

    client = DuckDBCatalogClient(catalogs=[str(c) for c in catalogs])

    middlewares = [
        Middleware(
            CORSMiddleware,
            allow_origins=cors_origins,
            allow_credentials=True,
            allow_methods=["GET", "POST", "OPTIONS"],
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
    )

    # /queryables endpoint — required for STAC Browser "Additional Filtering" CQL2 builder.
    # The landing page already advertises rel=queryables pointing here; without this endpoint
    # the browser falls back to hiding the filter UI.
    @api.app.get(
        "/queryables",
        response_model=None,
        include_in_schema=True,
        summary="Queryable properties for CQL2 filtering",
        tags=["STAC API - Filter Extension"],
    )
    def queryables():
        return {
            "$schema": "https://json-schema.org/draft/07/schema",
            "$id": "/queryables",
            "type": "object",
            "title": "Queryable properties for ESM-Tools STAC Catalog",
            "description": (
                "Properties that can be used as filter predicates in CQL2 expressions."
            ),
            "properties": {
                "datetime": {
                    "title": "Datetime",
                    "type": "string",
                    "format": "date-time",
                },
                "experiment": {
                    "title": "Experiment ID",
                    "type": "string",
                },
                "component": {
                    "title": "Model Component",
                    "type": "string",
                },
                "variable": {
                    "title": "Variable",
                    "type": "string",
                },
                "collection": {
                    "title": "Collection",
                    "type": "string",
                },
            },
        }

    # POST /format — OGC CQL2 format-negotiation probe issued by STAC Browser.
    # Not required for filtering to work (filters travel as CQL2-JSON in /search),
    # but returning 200 silences the 404 log noise.
    @api.app.post(
        "/format",
        response_model=None,
        include_in_schema=False,
    )
    def cql2_format(body: dict | None = None):
        return body or {}

    return api


# ---------------------------------------------------------------------------
# Module-level ``app`` for direct uvicorn invocation:
#   uvicorn esm_catalog.api.app:app [--reload]
#
# Configure via environment variables:
#   ESM_CATALOG_DB   — colon-separated list of catalog.duckdb paths
# ---------------------------------------------------------------------------

def _app_from_env():
    db_env = os.environ.get("ESM_CATALOG_DB", "")
    catalogs = [p for p in db_env.split(":") if p] if db_env else []
    if not catalogs:
        # Development fallback: look for catalog.duckdb in cwd
        default = Path("catalog.duckdb")
        if default.exists():
            catalogs = [str(default)]
        else:
            catalogs = [str(default)]  # Will warn but still start
    api = create_app(catalogs)
    return api.app


app = _app_from_env()
