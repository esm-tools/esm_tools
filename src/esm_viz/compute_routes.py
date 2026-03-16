"""
REST API routes for Dask compute cluster management.

Endpoints::

    POST   /compute/clusters          - Create or register a cluster
    GET    /compute/clusters          - List active clusters
    GET    /compute/clusters/{id}     - Get cluster status
    PATCH  /compute/clusters/{id}     - Scale workers
    DELETE /compute/clusters/{id}     - Disconnect / shutdown

Example usage with curl::

    # Create a local cluster
    curl -X POST http://localhost:8000/compute/clusters \\
        -H "Content-Type: application/json" \\
        -d '{"type": "local", "workers": 4}'

    # Connect to an existing scheduler
    curl -X POST http://localhost:8000/compute/clusters \\
        -H "Content-Type: application/json" \\
        -d '{"scheduler_address": "tcp://10.0.0.5:8786"}'

    # List clusters
    curl http://localhost:8000/compute/clusters

    # Scale a cluster
    curl -X PATCH http://localhost:8000/compute/clusters/abc123 \\
        -H "Content-Type: application/json" \\
        -d '{"workers": 8}'

    # Delete a cluster
    curl -X DELETE http://localhost:8000/compute/clusters/abc123
"""

from __future__ import annotations

from fastapi import APIRouter, HTTPException, Response
from loguru import logger
from pydantic import BaseModel

from esm_viz.compute import (
    ClusterCreateRequest,
    ClusterResponse,
    ClusterScaleRequest,
    ComputeManager,
    get_compute_manager,
)


class ClusterListResponse(BaseModel):
    """Response for GET /compute/clusters."""

    clusters: list[ClusterResponse]
    total: int


def create_compute_router(
    manager: ComputeManager | None = None,
) -> APIRouter:
    """Create the compute management router.

    Parameters
    ----------
    manager : ComputeManager, optional
        Injected manager instance. If not provided, the module-level
        singleton from ``get_compute_manager()`` is used.

    Returns
    -------
    APIRouter
        FastAPI router with compute cluster CRUD endpoints.
    """
    router = APIRouter(prefix="/compute", tags=["Compute Management"])

    def _mgr() -> ComputeManager:
        return manager if manager is not None else get_compute_manager()

    @router.post(
        "/clusters",
        response_model=ClusterResponse,
        status_code=201,
    )
    async def create_cluster(body: ClusterCreateRequest) -> ClusterResponse:
        """Create a new Dask cluster or connect to an existing scheduler.

        Request body fields:
            - type: "local", "gateway", "slurm", or omit for existing
            - workers: Number of workers (default 2)
            - memory: Memory per worker, e.g. "4GiB"
            - scheduler_address: tcp://... for existing schedulers
            - gateway_url: Dask Gateway URL
            - queue: SLURM partition
            - cores: Cores per worker (SLURM)
            - name: Human-readable cluster name
        """
        try:
            return _mgr().create_cluster(body)
        except RuntimeError as exc:
            raise HTTPException(status_code=400, detail=str(exc))
        except ValueError as exc:
            raise HTTPException(status_code=422, detail=str(exc))
        except Exception as exc:
            logger.exception("Cluster creation failed: {}", exc)
            raise HTTPException(
                status_code=500, detail=f"Cluster creation failed: {exc}"
            )

    @router.get("/clusters", response_model=ClusterListResponse)
    async def list_clusters() -> ClusterListResponse:
        """List all active compute clusters."""
        clusters = _mgr().list_clusters()
        return ClusterListResponse(clusters=clusters, total=len(clusters))

    @router.get("/clusters/{cluster_id}", response_model=ClusterResponse)
    async def get_cluster(cluster_id: str) -> ClusterResponse:
        """Get status for a specific cluster."""
        try:
            return _mgr().get_cluster(cluster_id)
        except KeyError:
            raise HTTPException(
                status_code=404,
                detail=f"Cluster '{cluster_id}' not found",
            )

    @router.patch("/clusters/{cluster_id}", response_model=ClusterResponse)
    async def scale_cluster(
        cluster_id: str, body: ClusterScaleRequest
    ) -> ClusterResponse:
        """Scale a cluster's worker count or enable adaptive scaling.

        Request body (at least one required):
            - workers: Target number of workers
            - adapt_min / adapt_max: Enable adaptive scaling
        """
        try:
            return _mgr().scale_cluster(cluster_id, body)
        except KeyError:
            raise HTTPException(
                status_code=404,
                detail=f"Cluster '{cluster_id}' not found",
            )
        except ValueError as exc:
            raise HTTPException(status_code=400, detail=str(exc))

    @router.delete("/clusters/{cluster_id}", status_code=204)
    async def delete_cluster(cluster_id: str) -> Response:
        """Disconnect from (and optionally shut down) a cluster.

        If the cluster was created by this manager it will be shut down.
        If it was an external scheduler, only the client is disconnected.
        """
        try:
            _mgr().delete_cluster(cluster_id)
        except KeyError:
            raise HTTPException(
                status_code=404,
                detail=f"Cluster '{cluster_id}' not found",
            )
        return Response(status_code=204)

    return router
