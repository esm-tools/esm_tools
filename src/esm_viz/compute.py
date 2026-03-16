"""
Dask distributed compute cluster management.

Manages the lifecycle of Dask clusters (local, SLURM, Gateway, or
pre-existing schedulers) so that heavy operations -- time averaging,
anomaly computation across many files -- can be offloaded from the
viz server to a proper distributed backend.

When a ``distributed.Client`` is registered as the default client,
all subsequent ``da.compute()`` calls automatically route to that
cluster. The viz code does not need to pass the client explicitly.
"""

from __future__ import annotations

import uuid
from dataclasses import dataclass, field
from datetime import datetime, timezone
from enum import Enum
from typing import Any

from loguru import logger
from pydantic import BaseModel, Field

# ---------------------------------------------------------------------------
# Optional dependency imports
# ---------------------------------------------------------------------------

try:
    from distributed import Client, LocalCluster
    HAS_DISTRIBUTED = True
except ImportError:
    HAS_DISTRIBUTED = False

try:
    import dask_gateway  # noqa: F401
    HAS_GATEWAY = True
except ImportError:
    HAS_GATEWAY = False

try:
    import dask_jobqueue  # noqa: F401
    HAS_JOBQUEUE = True
except ImportError:
    HAS_JOBQUEUE = False


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------


class ClusterType(str, Enum):
    """Supported cluster backends."""

    local = "local"
    gateway = "gateway"
    slurm = "slurm"
    existing = "existing"


class ClusterCreateRequest(BaseModel):
    """Request body for creating or registering a cluster."""

    type: ClusterType | None = Field(
        None,
        description=(
            "Cluster type. Omit when connecting to an existing scheduler "
            "via scheduler_address."
        ),
    )
    workers: int = Field(2, ge=1, description="Initial number of workers.")
    memory: str | None = Field(
        None, description="Memory per worker, e.g. '4GiB'."
    )
    scheduler_address: str | None = Field(
        None,
        description="Address of an existing scheduler (tcp://host:port).",
    )
    gateway_url: str | None = Field(
        None, description="Dask Gateway URL (for type=gateway)."
    )
    queue: str | None = Field(
        None, description="SLURM queue/partition (for type=slurm)."
    )
    cores: int = Field(
        2, ge=1, description="CPU cores per worker (for SLURM)."
    )
    name: str | None = Field(
        None, description="Human-readable name for this cluster."
    )


class ClusterResponse(BaseModel):
    """Serialisable cluster status returned by the API."""

    id: str
    name: str
    type: str
    status: str
    n_workers: int
    scheduler_address: str | None = None
    dashboard_url: str | None = None
    created_at: str


class ClusterScaleRequest(BaseModel):
    """Request body for scaling a cluster."""

    workers: int | None = Field(
        None, ge=1, description="Target number of workers (fixed scale)."
    )
    adapt_min: int | None = Field(
        None, ge=0, description="Minimum workers for adaptive scaling."
    )
    adapt_max: int | None = Field(
        None, ge=1, description="Maximum workers for adaptive scaling."
    )


# ---------------------------------------------------------------------------
# Internal bookkeeping
# ---------------------------------------------------------------------------


@dataclass
class ClusterInfo:
    """Runtime state for a single managed cluster."""

    id: str
    name: str
    type: str
    cluster: Any  # LocalCluster | SLURMCluster | GatewayCluster | None
    client: Any  # distributed.Client
    created_at: datetime = field(default_factory=lambda: datetime.now(timezone.utc))
    owns_cluster: bool = True  # False when connecting to an existing scheduler


# ---------------------------------------------------------------------------
# Threshold helper
# ---------------------------------------------------------------------------


def should_use_cluster(n_files: int, total_size_bytes: int) -> bool:
    """Decide whether to route computation to a cluster.

    Heuristic: use a cluster when there are more than 5 files or when
    the total data volume exceeds 1 GB.

    Parameters
    ----------
    n_files : int
        Number of data files involved.
    total_size_bytes : int
        Combined size of the data files in bytes.

    Returns
    -------
    bool
        True if a cluster should be used.
    """
    return n_files > 5 or total_size_bytes > 1_000_000_000


# ---------------------------------------------------------------------------
# ComputeManager
# ---------------------------------------------------------------------------


class ComputeManager:
    """Lifecycle manager for Dask distributed clusters.

    Maintains a registry of active clusters and their associated
    ``distributed.Client`` instances. At most one client is set as the
    Dask *default* at any time (the most recently created one).

    All public methods are synchronous -- the cluster operations
    themselves are blocking I/O and there is no benefit to making
    them async given that they are called infrequently by human users.
    """

    def __init__(self) -> None:
        self._active_clusters: dict[str, ClusterInfo] = {}

    # -- helpers ----------------------------------------------------------

    def _require_distributed(self) -> None:
        if not HAS_DISTRIBUTED:
            raise RuntimeError(
                "The 'distributed' package is required for cluster management. "
                "Install it with: pip install distributed"
            )

    def _build_response(self, info: ClusterInfo) -> ClusterResponse:
        n_workers = 0
        scheduler_address = None
        dashboard_url = None
        status = "unknown"

        try:
            if info.client is not None:
                scheduler_info = info.client.scheduler_info()
                n_workers = len(scheduler_info.get("workers", {}))
                scheduler_address = info.client.scheduler.address
            status = "running"
        except Exception:
            status = "disconnected"

        if info.cluster is not None:
            try:
                dashboard_url = getattr(info.cluster, "dashboard_link", None)
                if scheduler_address is None:
                    scheduler_address = getattr(
                        info.cluster, "scheduler_address", None
                    )
            except Exception:
                pass

        return ClusterResponse(
            id=info.id,
            name=info.name,
            type=info.type,
            status=status,
            n_workers=n_workers,
            scheduler_address=scheduler_address,
            dashboard_url=dashboard_url,
            created_at=info.created_at.isoformat(),
        )

    # -- public API -------------------------------------------------------

    def create_cluster(self, request: ClusterCreateRequest) -> ClusterResponse:
        """Create a new cluster or connect to an existing scheduler.

        Parameters
        ----------
        request : ClusterCreateRequest
            Cluster specification.

        Returns
        -------
        ClusterResponse
            Status of the newly created cluster.

        Raises
        ------
        RuntimeError
            If required packages are not installed.
        ValueError
            If the request is invalid.
        """
        self._require_distributed()

        cluster_id = uuid.uuid4().hex[:12]
        cluster_obj: Any = None
        owns_cluster = True

        # Determine effective type
        if request.scheduler_address:
            effective_type = ClusterType.existing
        elif request.type is not None:
            effective_type = request.type
        else:
            effective_type = ClusterType.local

        name = request.name or f"{effective_type.value}-{cluster_id[:6]}"

        if effective_type == ClusterType.existing:
            logger.info(
                "Connecting to existing scheduler at {}",
                request.scheduler_address,
            )
            client = Client(request.scheduler_address)
            owns_cluster = False

        elif effective_type == ClusterType.local:
            logger.info(
                "Creating LocalCluster with {} workers", request.workers
            )
            kwargs: dict[str, Any] = {"n_workers": request.workers}
            if request.memory:
                kwargs["memory_limit"] = request.memory
            cluster_obj = LocalCluster(**kwargs)
            client = Client(cluster_obj)

        elif effective_type == ClusterType.gateway:
            if not HAS_GATEWAY:
                raise RuntimeError(
                    "dask_gateway is not installed. "
                    "Install it with: pip install dask-gateway"
                )
            gw_url = request.gateway_url or ""
            logger.info("Connecting via Dask Gateway at {}", gw_url)
            gateway = dask_gateway.Gateway(gw_url)  # type: ignore[name-defined]
            cluster_obj = gateway.new_cluster()
            cluster_obj.scale(request.workers)
            client = cluster_obj.get_client()

        elif effective_type == ClusterType.slurm:
            if not HAS_JOBQUEUE:
                raise RuntimeError(
                    "dask_jobqueue is not installed. "
                    "Install it with: pip install dask-jobqueue"
                )
            logger.info(
                "Creating SLURMCluster with {} workers, queue={}",
                request.workers,
                request.queue,
            )
            slurm_kwargs: dict[str, Any] = {
                "cores": request.cores,
            }
            if request.memory:
                slurm_kwargs["memory"] = request.memory
            if request.queue:
                slurm_kwargs["queue"] = request.queue
            if request.name:
                slurm_kwargs["name"] = request.name

            cluster_obj = dask_jobqueue.SLURMCluster(**slurm_kwargs)  # type: ignore[name-defined]
            cluster_obj.scale(jobs=request.workers)
            client = Client(cluster_obj)

        else:
            raise ValueError(f"Unsupported cluster type: {effective_type}")

        info = ClusterInfo(
            id=cluster_id,
            name=name,
            type=effective_type.value,
            cluster=cluster_obj,
            client=client,
            owns_cluster=owns_cluster,
        )
        self._active_clusters[cluster_id] = info

        logger.info("Registered cluster {} (type={})", cluster_id, effective_type.value)
        return self._build_response(info)

    def list_clusters(self) -> list[ClusterResponse]:
        """List all active clusters."""
        return [self._build_response(info) for info in self._active_clusters.values()]

    def get_cluster(self, cluster_id: str) -> ClusterResponse:
        """Get status for a specific cluster.

        Raises
        ------
        KeyError
            If the cluster does not exist.
        """
        info = self._active_clusters.get(cluster_id)
        if info is None:
            raise KeyError(f"Cluster '{cluster_id}' not found")
        return self._build_response(info)

    def scale_cluster(
        self, cluster_id: str, request: ClusterScaleRequest
    ) -> ClusterResponse:
        """Scale a cluster to a new worker count or enable adaptive scaling.

        Parameters
        ----------
        cluster_id : str
            Cluster ID.
        request : ClusterScaleRequest
            Scaling parameters.

        Raises
        ------
        KeyError
            If the cluster does not exist.
        ValueError
            If the cluster cannot be scaled (e.g. external scheduler).
        """
        info = self._active_clusters.get(cluster_id)
        if info is None:
            raise KeyError(f"Cluster '{cluster_id}' not found")

        cluster = info.cluster
        if cluster is None:
            raise ValueError(
                "Cannot scale an externally-managed scheduler; "
                "scale it through its own management interface."
            )

        if request.adapt_min is not None and request.adapt_max is not None:
            logger.info(
                "Enabling adaptive scaling [{}, {}] on cluster {}",
                request.adapt_min,
                request.adapt_max,
                cluster_id,
            )
            cluster.adapt(minimum=request.adapt_min, maximum=request.adapt_max)
        elif request.workers is not None:
            logger.info(
                "Scaling cluster {} to {} workers", cluster_id, request.workers
            )
            cluster.scale(request.workers)
        else:
            raise ValueError(
                "Provide either 'workers' for fixed scaling or "
                "'adapt_min'/'adapt_max' for adaptive scaling."
            )

        return self._build_response(info)

    def delete_cluster(self, cluster_id: str) -> None:
        """Disconnect from (and optionally shut down) a cluster.

        Parameters
        ----------
        cluster_id : str
            Cluster ID.

        Raises
        ------
        KeyError
            If the cluster does not exist.
        """
        info = self._active_clusters.pop(cluster_id, None)
        if info is None:
            raise KeyError(f"Cluster '{cluster_id}' not found")

        # Close the client
        try:
            if info.client is not None:
                info.client.close()
                logger.info("Closed client for cluster {}", cluster_id)
        except Exception as exc:
            logger.warning("Error closing client for cluster {}: {}", cluster_id, exc)

        # Shut down the cluster only if we own it
        if info.owns_cluster and info.cluster is not None:
            try:
                info.cluster.close()
                logger.info("Shut down cluster {}", cluster_id)
            except Exception as exc:
                logger.warning(
                    "Error shutting down cluster {}: {}", cluster_id, exc
                )

    def get_client(self, cluster_id: str | None = None) -> Any:
        """Return the best available ``distributed.Client``, or ``None``.

        Parameters
        ----------
        cluster_id : str, optional
            If provided, return the client for that specific cluster.
            Otherwise return the most recently created client.

        Returns
        -------
        distributed.Client or None
            The client instance, or None if no clusters are registered.
        """
        if cluster_id is not None:
            info = self._active_clusters.get(cluster_id)
            return info.client if info is not None else None

        if not self._active_clusters:
            return None

        # Return the most recently created cluster's client
        latest = max(
            self._active_clusters.values(), key=lambda i: i.created_at
        )
        return latest.client


# ---------------------------------------------------------------------------
# Module-level singleton
# ---------------------------------------------------------------------------

_compute_manager: ComputeManager | None = None


def get_compute_manager() -> ComputeManager:
    """Return the module-level ``ComputeManager`` singleton.

    Creates it on first call.
    """
    global _compute_manager
    if _compute_manager is None:
        _compute_manager = ComputeManager()
    return _compute_manager
