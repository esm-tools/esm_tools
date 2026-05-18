"""Experiment-level STAC hierarchy endpoints.

GET /experiments              → list all experiments with component lists
GET /experiments?filter=...   → CQL2-filtered experiment search
GET /experiments/{id}         → STAC Catalog object for one experiment

With Option A (one collection per experiment) experiments and collections share
the same ID. Filtering is delegated to search_collections() so that all existing
CQL2 operators work — including component facet (component='fesom'),
variable facet (variable='tas'), and namelist parameters (nml:...).
"""
from typing import Optional

from fastapi import APIRouter, HTTPException, Query, Request

from esm_catalog.api.client import DuckDBCatalogClient, _inject_experiment_catalog_links
from esm_catalog.api.cql2 import parse_filter
from esm_catalog.api.responses import (
    ExperimentCatalog,
    ExperimentLink,
    ExperimentSummary,
    ExperimentsListResponse,
)


def _collection_to_summary(col: dict, base_url: str) -> ExperimentSummary:
    """Build an ExperimentSummary from a collection dict (Option A: col id == experiment id)."""
    exp_id = col.get("experiment") or col["id"]
    return ExperimentSummary(
        id=exp_id,
        title=col.get("title", exp_id),
        components=sorted(col.get("components", [])),
        href=f"{base_url}/experiments/{exp_id}",
    )


def create_experiment_router(client: DuckDBCatalogClient) -> APIRouter:
    router = APIRouter(prefix="/experiments", tags=["Experiment Hierarchy"])

    @router.get("", response_model=ExperimentsListResponse)
    def list_experiments(
        request: Request,
        limit: int = Query(100, ge=1, le=1000),
        offset: int = Query(0, ge=0),
        filter: Optional[str] = Query(
            None,
            description=(
                "CQL2 filter expression for experiment search. "
                "Examples: component='fesom', variable='tas', "
                "nml:radctl.co2vmr > 284.0"
            ),
        ),
        filter_lang: Optional[str] = Query(
            None, alias="filter-lang",
            description="Filter language: cql2-text (default) or cql2-json",
        ),
    ) -> ExperimentsListResponse:
        """List experiments, optionally filtered by component, variable, or namelist params.

        With the collapsed-collection layout (Option A) each experiment maps to
        exactly one STAC Collection.  Filtering is therefore identical to
        collection search: any property visible in collection_item_props or the
        collection's own fields can be used as a CQL2 predicate.

        Common facets:
        - ``component='fesom'``   — experiments that ran the FESOM ocean model
        - ``variable='tas'``      — experiments that produced surface temperature
        - ``nml:radctl.co2vmr > 284.0`` — experiments with elevated CO₂
        """
        base_url = str(request.base_url).rstrip("/")

        filter_props: dict = {}
        if filter:
            filter_props = parse_filter(filter, filter_lang)

        # Delegate to search_collections across all catalogs
        dbs = client._open_catalogs()
        try:
            matched_cols: list[dict] = []
            seen: set[str] = set()
            for db in dbs:
                cols, _ = db.search_collections(
                    filter_props=filter_props if filter_props else None
                )
                for col in cols:
                    exp_id = col.get("experiment") or col["id"]
                    if exp_id not in seen:
                        matched_cols.append(col)
                        seen.add(exp_id)
        finally:
            client._close_catalogs(dbs)

        total = len(matched_cols)
        page_cols = matched_cols[offset:offset + limit]
        summaries = [_collection_to_summary(col, base_url) for col in page_cols]

        qs = f"limit={limit}"
        if filter:
            qs += f"&filter={filter}"
            if filter_lang:
                qs += f"&filter-lang={filter_lang}"

        links = [
            ExperimentLink(rel="self",  href=f"{base_url}/experiments?{qs}&offset={offset}"),
            ExperimentLink(rel="root",  href=f"{base_url}/"),
            ExperimentLink(rel="first", href=f"{base_url}/experiments?{qs}&offset=0"),
        ]
        if offset > 0:
            links.append(ExperimentLink(
                rel="prev",
                href=f"{base_url}/experiments?{qs}&offset={max(0, offset - limit)}",
            ))
        if offset + limit < total:
            links.append(ExperimentLink(
                rel="next",
                href=f"{base_url}/experiments?{qs}&offset={offset + limit}",
            ))

        return ExperimentsListResponse(
            experiments=summaries,
            numberMatched=total,
            numberReturned=len(summaries),
            links=links,
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
