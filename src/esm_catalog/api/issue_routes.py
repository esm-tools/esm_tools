"""REST endpoints for experiment issue tracking.

Provides CRUD operations on issues attached to STAC Collections.
Issues are stored in the same DuckDB catalog as the collection data.

Usage::

    from esm_catalog.api.issue_routes import create_issue_router
    router = create_issue_router(registry, pool)
    app.include_router(router)
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from fastapi import APIRouter, HTTPException
from loguru import logger
from pydantic import BaseModel

if TYPE_CHECKING:
    from esm_catalog.api.pool import CatalogPool
    from esm_catalog.api.registry import CatalogRegistry


# ---------------------------------------------------------------------------
# Request / Response models
# ---------------------------------------------------------------------------

class IssueCreate(BaseModel):
    title: str
    body: str | None = None
    labels: str | None = None
    created_by: str | None = None


class IssueUpdate(BaseModel):
    title: str | None = None
    body: str | None = None
    status: str | None = None
    labels: str | None = None


# ---------------------------------------------------------------------------
# Router factory
# ---------------------------------------------------------------------------

def create_issue_router(
    registry: "CatalogRegistry",
    pool: "CatalogPool",
) -> APIRouter:
    """Create a FastAPI router for issue endpoints."""

    router = APIRouter(tags=["issues"])

    def _find_catalog_for_collection(collection_id: str):
        """Find the CatalogDB that contains the given collection.

        Returns the catalog path (for opening a read-write connection),
        or raises 404 if the collection is not found.
        """
        for path in registry.get_paths():
            db = pool.get(path)
            if db is None:
                continue
            col = db.get_collection(collection_id)
            if col is not None:
                return path
        raise HTTPException(404, f"Collection '{collection_id}' not found")

    def _open_writable(path: str):
        """Open a temporary read-write connection for issue mutations."""
        from esm_catalog.storage.duckdb import CatalogDB
        return CatalogDB(path, read_only=False)

    # -----------------------------------------------------------------------
    # Endpoints
    # -----------------------------------------------------------------------

    @router.get("/collections/{collection_id}/issues")
    def list_issues(collection_id: str, status: str = "open"):
        path = _find_catalog_for_collection(collection_id)
        db = pool.get(path)
        issues = db.get_issues(collection_id, status=status)
        # Serialize datetime objects to ISO strings
        for issue in issues:
            for key in ("created_at", "updated_at"):
                if issue.get(key) is not None:
                    issue[key] = str(issue[key])
        return {"issues": issues, "total": len(issues)}

    @router.post("/collections/{collection_id}/issues", status_code=201)
    def create_issue(collection_id: str, body: IssueCreate):
        path = _find_catalog_for_collection(collection_id)
        db = _open_writable(path)
        try:
            issue = db.create_issue(
                collection_id=collection_id,
                title=body.title,
                body=body.body,
                labels=body.labels,
                created_by=body.created_by,
            )
            for key in ("created_at", "updated_at"):
                if issue.get(key) is not None:
                    issue[key] = str(issue[key])
            logger.info("Created issue {} for collection {}", issue["id"], collection_id)
            return issue
        finally:
            db.close()

    @router.get("/collections/{collection_id}/issues/{issue_id}")
    def get_issue(collection_id: str, issue_id: str):
        path = _find_catalog_for_collection(collection_id)
        db = pool.get(path)
        issue = db.get_issue(issue_id)
        if issue is None:
            raise HTTPException(404, f"Issue '{issue_id}' not found")
        for key in ("created_at", "updated_at"):
            if issue.get(key) is not None:
                issue[key] = str(issue[key])
        return issue

    @router.patch("/collections/{collection_id}/issues/{issue_id}")
    def update_issue(collection_id: str, issue_id: str, body: IssueUpdate):
        path = _find_catalog_for_collection(collection_id)
        db = _open_writable(path)
        try:
            updates = body.model_dump(exclude_none=True)
            issue = db.update_issue(issue_id, **updates)
            if issue is None:
                raise HTTPException(404, f"Issue '{issue_id}' not found")
            for key in ("created_at", "updated_at"):
                if issue.get(key) is not None:
                    issue[key] = str(issue[key])
            logger.info("Updated issue {}", issue_id)
            return issue
        finally:
            db.close()

    @router.delete("/collections/{collection_id}/issues/{issue_id}")
    def close_issue(collection_id: str, issue_id: str):
        path = _find_catalog_for_collection(collection_id)
        db = _open_writable(path)
        try:
            issue = db.close_issue(issue_id)
            if issue is None:
                raise HTTPException(404, f"Issue '{issue_id}' not found")
            for key in ("created_at", "updated_at"):
                if issue.get(key) is not None:
                    issue[key] = str(issue[key])
            logger.info("Closed issue {}", issue_id)
            return issue
        finally:
            db.close()

    return router
