"""Dynamic catalog registry with optional persistence.

Manages the list of catalogs served by the API. Catalogs can be added,
updated, and removed at runtime. If a persistence path is provided,
the registry is saved to disk after each modification.

Example::

    # In-memory only
    registry = CatalogRegistry(initial_catalogs=["/path/to/catalog.duckdb"])

    # With persistence
    registry = CatalogRegistry(
        initial_catalogs=["/path/to/catalog.duckdb"],
        persist_path="/var/lib/esm-catalog/registry.json",
    )

    # Add a catalog dynamically
    info = registry.add_catalog(CatalogCreateRequest(
        path="/path/to/another.duckdb",
        name="Experiment 2",
    ))

    # Update metadata
    registry.update_catalog(info.id, CatalogUpdateRequest(
        name="Experiment 2 (Updated)",
    ))

    # Get current paths for querying
    paths = registry.get_paths()
"""

from __future__ import annotations

import json
import threading
import time
import uuid
from enum import Enum
from pathlib import Path
from typing import Optional

from loguru import logger
from pydantic import BaseModel, Field

# Default TTL for status cache (seconds)
_STATUS_CACHE_TTL = 60


class CatalogStatus(str, Enum):
    """Status of a registered catalog."""

    ACTIVE = "active"  # File exists and is accessible
    MISSING = "missing"  # File does not exist (yet)
    ERROR = "error"  # File exists but cannot be opened


class CatalogInfo(BaseModel):
    """Registered catalog metadata."""

    id: str = Field(..., description="Unique identifier for this registration")
    path: str = Field(..., description="Absolute path to the DuckDB catalog file")
    status: CatalogStatus = Field(default=CatalogStatus.MISSING)
    name: Optional[str] = Field(default=None, description="Human-readable name")
    description: Optional[str] = Field(default=None, description="Catalog description")

    model_config = {"use_enum_values": True}


class CatalogCreateRequest(BaseModel):
    """Request body for POST /catalogs."""

    path: str = Field(..., description="Path to the DuckDB catalog file")
    name: Optional[str] = Field(default=None, description="Human-readable name")
    description: Optional[str] = Field(default=None, description="Catalog description")


class CatalogUpdateRequest(BaseModel):
    """Request body for PATCH /catalogs/{id}."""

    path: Optional[str] = Field(default=None, description="New path (triggers reconnection)")
    name: Optional[str] = Field(default=None, description="New human-readable name")
    description: Optional[str] = Field(default=None, description="New description")


class CatalogRegistry:
    """Thread-safe registry of catalog paths with optional file persistence.

    Manages the list of catalogs served by the API. Catalogs can be added
    and removed at runtime. If a persistence path is provided, the registry
    is saved to disk after each modification.

    Attributes:
        _lock: Threading lock for thread-safe access.
        _catalogs: Mapping from catalog ID to CatalogInfo.
        _persist_path: Optional path to JSON file for persistence.
        _status_cache: Cache of (status, timestamp) for lazy status refresh.
        _status_ttl: TTL in seconds for status cache entries.
    """

    def __init__(
        self,
        initial_catalogs: list[str | Path] | None = None,
        persist_path: str | Path | None = None,
        status_cache_ttl: int = _STATUS_CACHE_TTL,
    ) -> None:
        """Initialize the registry.

        Args:
            initial_catalogs: Paths passed at server startup (from CLI/env).
            persist_path: Optional JSON file for saving/loading registry state.
            status_cache_ttl: TTL in seconds for status cache (default 60).
        """
        self._lock = threading.Lock()
        self._catalogs: dict[str, CatalogInfo] = {}
        self._persist_path = Path(persist_path) if persist_path else None
        self._status_cache: dict[str, tuple[CatalogStatus, float]] = {}
        self._status_ttl = status_cache_ttl

        # Load persisted state if it exists
        if self._persist_path and self._persist_path.exists():
            self._load()

        # Add initial catalogs (from CLI) - these are added if not already present
        for path in initial_catalogs or []:
            self._add_initial(str(path))

    def _generate_id(self) -> str:
        """Generate a short unique ID for a catalog."""
        return str(uuid.uuid4())[:8]

    def _add_initial(self, path: str) -> None:
        """Add an initial catalog (from CLI) without triggering persistence."""
        path_resolved = str(Path(path).resolve())
        existing = self._find_by_path(path_resolved)
        if existing:
            return  # Already registered

        catalog_id = self._generate_id()
        status = CatalogStatus.ACTIVE if Path(path_resolved).exists() else CatalogStatus.MISSING
        self._catalogs[catalog_id] = CatalogInfo(
            id=catalog_id,
            path=path_resolved,
            status=status,
            name=Path(path_resolved).stem,
        )
        logger.debug("Registered initial catalog: {} -> {}", catalog_id, path_resolved)

    def _find_by_path(self, path: str) -> CatalogInfo | None:
        """Find a catalog by its resolved path."""
        for info in self._catalogs.values():
            if info.path == path:
                return info
        return None

    def _refresh_status(self, info: CatalogInfo, force: bool = False) -> None:
        """Update the status of a catalog based on file existence.

        Uses a TTL-based cache to avoid expensive Path.exists() calls
        on every request. The cache is bypassed if force=True.

        Args:
            info: Catalog info to update.
            force: If True, bypass cache and check filesystem directly.
        """
        now = time.monotonic()
        cached = self._status_cache.get(info.id)

        # Use cached status if valid and not forced
        if not force and cached is not None:
            cached_status, cached_time = cached
            if now - cached_time < self._status_ttl:
                info.status = cached_status
                return

        # Check filesystem and update cache
        if Path(info.path).exists():
            info.status = CatalogStatus.ACTIVE
        else:
            info.status = CatalogStatus.MISSING

        self._status_cache[info.id] = (info.status, now)

    def list_catalogs(self) -> list[CatalogInfo]:
        """Return all registered catalogs with updated status.

        Status is refreshed on each call to reflect current file existence.
        """
        with self._lock:
            result = []
            for info in self._catalogs.values():
                self._refresh_status(info)
                result.append(info.model_copy())
            return result

    def get_catalog(self, catalog_id: str) -> CatalogInfo | None:
        """Get a single catalog by ID with refreshed status."""
        with self._lock:
            info = self._catalogs.get(catalog_id)
            if info:
                self._refresh_status(info)
                return info.model_copy()
            return None

    def add_catalog(self, request: CatalogCreateRequest) -> CatalogInfo:
        """Register a new catalog.

        Args:
            request: Catalog creation request with path and optional metadata.

        Returns:
            The created CatalogInfo.

        Raises:
            ValueError: If the path is already registered.
        """
        path_resolved = str(Path(request.path).resolve())

        with self._lock:
            existing = self._find_by_path(path_resolved)
            if existing:
                raise ValueError(f"Catalog already registered with ID: {existing.id}")

            catalog_id = self._generate_id()
            status = CatalogStatus.ACTIVE if Path(path_resolved).exists() else CatalogStatus.MISSING

            info = CatalogInfo(
                id=catalog_id,
                path=path_resolved,
                status=status,
                name=request.name or Path(path_resolved).stem,
                description=request.description,
            )
            self._catalogs[catalog_id] = info
            self._save()

            logger.info("Registered catalog: {} -> {}", catalog_id, path_resolved)
            return info.model_copy()

    def update_catalog(self, catalog_id: str, request: CatalogUpdateRequest) -> CatalogInfo | None:
        """Update a catalog's metadata.

        Args:
            catalog_id: ID of the catalog to update.
            request: Update request with optional fields to change.

        Returns:
            Updated CatalogInfo, or None if catalog not found.

        Raises:
            ValueError: If the new path is already registered to another catalog.
        """
        with self._lock:
            info = self._catalogs.get(catalog_id)
            if info is None:
                return None

            # Update path if provided
            if request.path is not None:
                new_path = str(Path(request.path).resolve())
                if new_path != info.path:
                    # Check for conflicts
                    existing = self._find_by_path(new_path)
                    if existing and existing.id != catalog_id:
                        raise ValueError(f"Path already registered to catalog: {existing.id}")
                    info.path = new_path
                    # Invalidate status cache since path changed
                    self._status_cache.pop(catalog_id, None)

            # Update other fields
            if request.name is not None:
                info.name = request.name
            if request.description is not None:
                info.description = request.description

            # Refresh status (force=True since we just invalidated or need fresh status)
            self._refresh_status(info, force=True)
            self._save()

            logger.info("Updated catalog: {}", catalog_id)
            return info.model_copy()

    def remove_catalog(self, catalog_id: str) -> bool:
        """Unregister a catalog.

        Args:
            catalog_id: ID of the catalog to remove.

        Returns:
            True if the catalog was removed, False if not found.
        """
        with self._lock:
            if catalog_id not in self._catalogs:
                return False

            info = self._catalogs.pop(catalog_id)
            # Clean up status cache entry
            self._status_cache.pop(catalog_id, None)
            self._save()

            logger.info("Unregistered catalog: {} ({})", catalog_id, info.path)
            return True

    def get_paths(self) -> list[str]:
        """Return list of all catalog paths for query operations."""
        with self._lock:
            return [info.path for info in self._catalogs.values()]

    def get_path(self, catalog_id: str) -> str | None:
        """Return the path for a specific catalog ID."""
        with self._lock:
            info = self._catalogs.get(catalog_id)
            return info.path if info else None

    def invalidate_status(self, catalog_id: str | None = None) -> None:
        """Invalidate status cache for a catalog or all catalogs.

        Use this after a catalog file has been externally modified to
        force a fresh status check on the next access.

        Args:
            catalog_id: Specific catalog to invalidate, or None for all.
        """
        with self._lock:
            if catalog_id is None:
                self._status_cache.clear()
                logger.debug("Invalidated all status cache entries")
            elif catalog_id in self._status_cache:
                del self._status_cache[catalog_id]
                logger.debug("Invalidated status cache for catalog: {}", catalog_id)

    def _load(self) -> None:
        """Load registry from persistence file."""
        if not self._persist_path or not self._persist_path.exists():
            return
        try:
            data = json.loads(self._persist_path.read_text())
            for item in data.get("catalogs", []):
                info = CatalogInfo(**item)
                self._catalogs[info.id] = info
            logger.debug("Loaded {} catalogs from {}", len(self._catalogs), self._persist_path)
        except Exception as e:
            logger.warning("Failed to load catalog registry from {}: {}", self._persist_path, e)

    def _save(self) -> None:
        """Save registry to persistence file."""
        if not self._persist_path:
            return
        try:
            self._persist_path.parent.mkdir(parents=True, exist_ok=True)
            data = {"catalogs": [info.model_dump() for info in self._catalogs.values()]}
            self._persist_path.write_text(json.dumps(data, indent=2))
            logger.debug("Saved {} catalogs to {}", len(self._catalogs), self._persist_path)
        except Exception as e:
            logger.warning("Failed to save catalog registry to {}: {}", self._persist_path, e)

    def __len__(self) -> int:
        """Return the number of registered catalogs."""
        with self._lock:
            return len(self._catalogs)
