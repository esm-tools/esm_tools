"""Experiment-level STAC hierarchy endpoints.

GET /experiments         → list all experiments with collection counts
GET /experiments/{id}    → STAC Catalog object for one experiment
"""
from fastapi import APIRouter, HTTPException, Query, Request

from esm_catalog.api.client import DuckDBCatalogClient, _inject_experiment_catalog_links
from esm_catalog.api.responses import (
    ExperimentCatalog,
    ExperimentLink,
    ExperimentSummary,
    ExperimentsListResponse,
)


def create_experiment_router(client: DuckDBCatalogClient) -> APIRouter:
    router = APIRouter(prefix="/experiments", tags=["Experiment Hierarchy"])

    @router.get("", response_model=ExperimentsListResponse)
    def list_experiments(
        request: Request,
        limit: int = Query(100, ge=1, le=1000),
        offset: int = Query(0, ge=0),
    ) -> ExperimentsListResponse:
        base_url = str(request.base_url).rstrip("/")
        all_ids = client._get_all_experiment_ids()
        total = len(all_ids)
        page_ids = all_ids[offset:offset + limit]

        summaries = []
        for exp_id in page_ids:
            cols = client._get_collections_for_experiment(exp_id)
            summaries.append(ExperimentSummary(
                id=exp_id, title=exp_id,
                collection_count=len(cols),
                href=f"{base_url}/experiments/{exp_id}",
            ))

        links = [
            ExperimentLink(rel="self",  href=f"{base_url}/experiments?limit={limit}&offset={offset}"),
            ExperimentLink(rel="root",  href=f"{base_url}/"),
            ExperimentLink(rel="first", href=f"{base_url}/experiments?limit={limit}&offset=0"),
        ]
        if offset > 0:
            links.append(ExperimentLink(rel="prev",
                href=f"{base_url}/experiments?limit={limit}&offset={max(0, offset-limit)}"))
        if offset + limit < total:
            links.append(ExperimentLink(rel="next",
                href=f"{base_url}/experiments?limit={limit}&offset={offset+limit}"))

        return ExperimentsListResponse(
            experiments=summaries, numberMatched=total,
            numberReturned=len(summaries), links=links,
        )

    @router.get("/{experiment_id}", response_model=ExperimentCatalog,
                responses={404: {"description": "Experiment not found"}})
    def get_experiment(experiment_id: str, request: Request) -> ExperimentCatalog:
        base_url = str(request.base_url).rstrip("/")
        cols = client._get_collections_for_experiment(experiment_id)
        if not cols:
            raise HTTPException(404, detail=f"Experiment '{experiment_id}' not found")
        catalog_dict = _inject_experiment_catalog_links(experiment_id, base_url, cols)
        return ExperimentCatalog(**catalog_dict)

    return router


def create_collection_experiment_router(client: DuckDBCatalogClient) -> APIRouter:
    """Router for GET /collections/{id}/experiment.

    Convenience shortcut: given a component collection (e.g. ``basic-001-echam``),
    return the parent experiment catalog — the same JSON as
    ``GET /experiments/basic-001`` — without the caller needing to know the
    experiment ID in advance.
    """
    router = APIRouter(tags=["Experiment Hierarchy"])

    @router.get(
        "/collections/{collection_id}/experiment",
        response_model=ExperimentCatalog,
        responses={404: {"description": "Collection or experiment not found"}},
        summary="Get the experiment catalog for a collection",
    )
    def get_collection_experiment(
        collection_id: str, request: Request
    ) -> ExperimentCatalog:
        base_url = str(request.base_url).rstrip("/")

        # Find the collection across all catalogs to get its experiment field.
        dbs = client._open_catalogs()
        try:
            found_col: dict | None = None
            for db in dbs:
                col = db.get_collection(collection_id)
                if col is not None:
                    found_col = col
                    break
        finally:
            client._close_catalogs(dbs)

        if found_col is None:
            raise HTTPException(
                status_code=404,
                detail=f"Collection '{collection_id}' not found",
            )

        experiment_id: str | None = found_col.get("experiment") or None
        if not experiment_id:
            raise HTTPException(
                status_code=404,
                detail=f"Collection '{collection_id}' has no experiment field",
            )

        cols = client._get_collections_for_experiment(experiment_id)
        catalog_dict = _inject_experiment_catalog_links(experiment_id, base_url, cols)
        return ExperimentCatalog(**catalog_dict)

    return router
