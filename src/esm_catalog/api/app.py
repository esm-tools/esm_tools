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

import json
import os
from pathlib import Path
from typing import List, Union

from fastapi.middleware.cors import CORSMiddleware
from starlette.middleware import Middleware
from starlette.requests import Request
from stac_fastapi.api.app import StacApi
from stac_fastapi.types.config import ApiSettings

from esm_catalog.api.client import DuckDBCatalogClient, FilteredSearchPostRequest


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
        search_post_request_model=FilteredSearchPostRequest,
    )

    # /queryables endpoint — required for STAC Browser "Additional Filtering" CQL2 builder.
    # The landing page advertises rel=http://www.opengis.net/def/rel/ogc/1.0/queryables
    # pointing here.  We populate enum lists from the live catalogs so STAC Browser
    # shows dropdown pickers rather than free-text fields.
    @api.app.get(
        "/queryables",
        response_model=None,
        include_in_schema=True,
        summary="Queryable properties for CQL2 filtering",
        tags=["STAC API - Filter Extension"],
    )
    def queryables(request: Request):
        base_url = str(request.base_url).rstrip("/")

        # Collect unique values via DISTINCT queries on the items table.
        # Summaries on collections are often empty; the items table is the
        # authoritative source for what values actually exist.
        from esm_catalog.storage.duckdb import CatalogDB

        def _distinct(db, json_path: str) -> list[str]:
            """Return sorted distinct non-null string values for a JSON property."""
            rows = db.db.execute(
                f"SELECT DISTINCT json_extract_string(data, '{json_path}') AS v "
                f"FROM items WHERE v IS NOT NULL ORDER BY v"
            ).fetchall()
            return [r[0] for r in rows if r[0]]

        experiments: set[str] = set()
        components: set[str] = set()
        variables: set[str] = set()
        all_variables: set[str] = set()
        collections: set[str] = set()
        for path in catalogs:
            p = Path(path)
            if not p.exists():
                continue
            db = CatalogDB(p)
            try:
                collections.update(_distinct(db, "$.collection"))
                experiments.update(_distinct(db, "$.properties.experiment"))
                components.update(_distinct(db, "$.properties.component"))
                variables.update(_distinct(db, "$.properties.variable"))

                # Collect all variable names from multi-variable files.
                # $.properties.variables is a JSON array; flatten via Python.
                rows = db.db.execute(
                    "SELECT json_extract_string(data, '$.properties.variables') "
                    "FROM items "
                    "WHERE json_array_length(data, '$.properties.variables') > 0"
                ).fetchall()
                for (arr_str,) in rows:
                    if arr_str:
                        try:
                            names = json.loads(arr_str)
                            if isinstance(names, list):
                                all_variables.update(n for n in names if n)
                        except Exception:
                            pass
            finally:
                db.close()

        def _str_prop(title: str, enum: list[str]) -> dict:
            p: dict = {"title": title, "type": "string"}
            if enum:
                p["enum"] = sorted(enum)
            return p

        return {
            "$schema": "https://json-schema.org/draft/2019-09/schema",
            "$id": f"{base_url}/queryables",
            "type": "object",
            "title": "Queryable properties for ESM-Tools STAC Catalog",
            "properties": {
                "datetime":   {"title": "Datetime",       "type": "string", "format": "date-time"},
                "collection": _str_prop("Collection",     sorted(collections)),
                "experiment": _str_prop("Experiment ID",  sorted(experiments)),
                "component":  _str_prop("Model Component", sorted(components)),
                "variable":   _str_prop("Variable",       sorted(variables)),
                "variables":  _str_prop("Any Variable (multi-var files)", sorted(all_variables)),
            },
        }

    # /collections/{collection_id}/queryables — per-collection queryables for the
    # CQL2 "Additional Filters" section that appears when browsing a collection's
    # items.  Returns the same schema as /queryables but with enum values scoped
    # to items that belong to the requested collection.
    @api.app.get(
        "/collections/{collection_id}/queryables",
        response_model=None,
        include_in_schema=True,
        summary="Queryable properties for a specific collection",
        tags=["STAC API - Filter Extension"],
    )
    def collection_queryables(collection_id: str, request: Request):
        from esm_catalog.storage.duckdb import CatalogDB
        from fastapi.responses import JSONResponse

        base_url = str(request.base_url).rstrip("/")

        def _distinct(db, json_path: str, cid: str) -> list[str]:
            rows = db.db.execute(
                f"SELECT DISTINCT json_extract_string(data, '{json_path}') AS v "
                f"FROM items "
                f"WHERE json_extract_string(data, '$.collection') = ? AND v IS NOT NULL "
                f"ORDER BY v",
                [cid],
            ).fetchall()
            return [r[0] for r in rows if r[0]]

        # Verify collection exists and collect distinct values across all DBs.
        found = False
        experiments: set[str] = set()
        components: set[str] = set()
        variables: set[str] = set()
        all_variables: set[str] = set()
        for path in catalogs:
            p = Path(path)
            if not p.exists():
                continue
            db = CatalogDB(p)
            try:
                # Check if this collection exists in this DB
                row = db.db.execute(
                    "SELECT COUNT(*) FROM items "
                    "WHERE json_extract_string(data, '$.collection') = ?",
                    [collection_id],
                ).fetchone()
                if row and row[0] > 0:
                    found = True
                experiments.update(_distinct(db, "$.properties.experiment", collection_id))
                components.update(_distinct(db, "$.properties.component", collection_id))
                variables.update(_distinct(db, "$.properties.variable", collection_id))

                rows = db.db.execute(
                    "SELECT json_extract_string(data, '$.properties.variables') "
                    "FROM items "
                    "WHERE json_extract_string(data, '$.collection') = ? "
                    "  AND json_array_length(data, '$.properties.variables') > 0",
                    [collection_id],
                ).fetchall()
                for (arr_str,) in rows:
                    if arr_str:
                        try:
                            names = json.loads(arr_str)
                            if isinstance(names, list):
                                all_variables.update(n for n in names if n)
                        except Exception:
                            pass
            finally:
                db.close()

        if not found:
            return JSONResponse(status_code=404, content={"detail": f"Collection '{collection_id}' not found"})

        def _str_prop(title: str, enum: list[str]) -> dict:
            p: dict = {"title": title, "type": "string"}
            if enum:
                p["enum"] = sorted(enum)
            return p

        return {
            "$schema": "https://json-schema.org/draft/2019-09/schema",
            "$id": f"{base_url}/collections/{collection_id}/queryables",
            "type": "object",
            "title": f"Queryable properties for collection {collection_id!r}",
            "properties": {
                "datetime":   {"title": "Datetime",       "type": "string", "format": "date-time"},
                "experiment": _str_prop("Experiment ID",  sorted(experiments)),
                "component":  _str_prop("Model Component", sorted(components)),
                "variable":   _str_prop("Variable",       sorted(variables)),
                "variables":  _str_prop("Any Variable (multi-var files)", sorted(all_variables)),
            },
        }

    # GET /stac-extensions/hpc/v0.1.0/schema.json
    # Serves the HPC storage extension schema locally so STAC Browser can
    # validate items against it.  The canonical URL
    # (https://esm-tools.github.io/stac-hpc-extension/v0.1.0/schema.json)
    # is not yet published; items' stac_extensions are rewritten at serve time
    # to point here instead.
    @api.app.get(
        "/stac-extensions/hpc/v0.1.0/schema.json",
        response_model=None,
        include_in_schema=False,
    )
    def hpc_schema(request: Request):
        base_url = str(request.base_url).rstrip("/")
        schema_id = f"{base_url}/stac-extensions/hpc/v0.1.0/schema.json"
        return {
            "$schema": "http://json-schema.org/draft-07/schema#",
            "$id": schema_id,
            "title": "HPC Storage Extension",
            "description": "STAC extension for HPC facility and storage-tier metadata.",
            "oneOf": [{"$ref": "#/definitions/stac_item"}],
            "definitions": {
                "stac_item": {
                    "allOf": [
                        {"$ref": "#/definitions/hpc_props"},
                        {"$ref": "#/definitions/hpc_assets"},
                    ]
                },
                "hpc_props": {
                    "type": "object",
                    "properties": {
                        "properties": {
                            "type": "object",
                            "properties": {
                                "hpc:facility":     {"type": "string",  "title": "HPC facility name"},
                                "hpc:system":       {"type": "string",  "title": "HPC system name"},
                                "hpc:storage_tier": {"type": "string",  "title": "Storage tier",
                                                     "enum": ["hot", "warm", "cold"]},
                                "hpc:last_access":  {"type": "string",  "title": "Last access time",
                                                     "format": "date-time"},
                            },
                        }
                    },
                },
                "hpc_assets": {
                    "type": "object",
                    "properties": {
                        "assets": {
                            "type": "object",
                            "additionalProperties": {
                                "type": "object",
                                "properties": {
                                    "hpc:storage_type":       {"type": "string"},
                                    "hpc:state":              {"type": "string"},
                                    "hpc:recall_time_estimate": {"type": "string"},
                                },
                            },
                        }
                    },
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
    async def cql2_format(request: Request):
        # STAC Browser sends the CQL2 expression as plain text, not JSON.
        # Accept anything and echo back an empty 200 to silence log noise.
        return {}

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
