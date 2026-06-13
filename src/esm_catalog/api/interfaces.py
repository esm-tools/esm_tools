"""Protocol interfaces for the ESM-Catalog API.

This module defines abstract Protocol interfaces that enable better abstraction,
dependency injection, and testing for the ESM-Catalog API components. Using
protocols allows for loose coupling between components and facilitates mocking
in unit tests.
"""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING, Any, Iterator, Protocol


class StorageBackend(Protocol):
    """Protocol defining the interface for catalog storage backends.

    Storage backends are responsible for persisting and retrieving catalog
    data (collections and items). Implementations might use SQLite, PostgreSQL,
    or other storage mechanisms.

    All search methods return a tuple of (results, total_count) to support
    pagination. The total_count represents the total number of matching
    records before limit/offset are applied.
    """

    def search_items(
        self, filter_props: dict | None, limit: int, offset: int
    ) -> tuple[list[dict], int]:
        """Search for items matching the given filter properties.

        Args:
            filter_props: Dictionary of property filters to apply, or None
                for no filtering.
            limit: Maximum number of items to return.
            offset: Number of items to skip for pagination.

        Returns:
            A tuple of (items, total_count) where items is a list of item
            dictionaries and total_count is the total number of matching
            items before pagination.
        """
        ...

    def search_collections(
        self, filter_props: dict | None, limit: int, offset: int
    ) -> tuple[list[dict], int]:
        """Search for collections matching the given filter properties.

        Args:
            filter_props: Dictionary of property filters to apply, or None
                for no filtering.
            limit: Maximum number of collections to return.
            offset: Number of collections to skip for pagination.

        Returns:
            A tuple of (collections, total_count) where collections is a list
            of collection dictionaries and total_count is the total number of
            matching collections before pagination.
        """
        ...

    def get_collection(self, collection_id: str) -> dict | None:
        """Retrieve a single collection by its identifier.

        Args:
            collection_id: The unique identifier of the collection to retrieve.

        Returns:
            The collection dictionary if found, or None if no collection
            exists with the given identifier.
        """
        ...

    def iter_collections(self) -> Iterator[dict]:
        """Iterate over all collections in the storage backend.

        Yields:
            Collection dictionaries, one at a time. This method is memory-
            efficient for large catalogs as it does not load all collections
            into memory at once.
        """
        ...


class ConnectionPool(Protocol):
    """Protocol defining the interface for managing storage backend connections.

    The connection pool manages the lifecycle of storage backend connections,
    providing caching, retrieval, and cleanup functionality. This abstraction
    allows for efficient resource management and connection reuse.
    """

    def get(self, path: str | Path) -> StorageBackend | None:
        """Get a storage backend connection for the given path.

        If a connection already exists in the pool, it is returned. Otherwise,
        a new connection may be created depending on the implementation.

        Args:
            path: The path to the catalog storage (e.g., database file path).

        Returns:
            A StorageBackend instance if the connection exists or can be
            created, or None if the path is invalid or connection fails.
        """
        ...

    def close(self, path: str | Path) -> bool:
        """Close and remove a specific connection from the pool.

        Args:
            path: The path identifying the connection to close.

        Returns:
            True if a connection was found and closed, False if no connection
            existed for the given path.
        """
        ...

    def refresh(self, path: str | Path) -> StorageBackend | None:
        """Refresh a connection by closing and reopening it.

        This is useful when the underlying storage has been modified externally
        and the connection state needs to be reset.

        Args:
            path: The path to the catalog storage to refresh.

        Returns:
            A fresh StorageBackend instance, or None if the refresh fails.
        """
        ...

    def close_all(self) -> None:
        """Close all connections in the pool.

        This method should be called during application shutdown to ensure
        all resources are properly released.
        """
        ...


class CatalogRegistryProtocol(Protocol):
    """Protocol defining the interface for catalog registry operations.

    The catalog registry maintains a list of known catalogs and provides
    CRUD operations for managing them. It serves as the central point for
    discovering and accessing available catalogs in the system.
    """

    def get_paths(self) -> list[str]:
        """Get the file paths of all registered catalogs.

        Returns:
            A list of string paths pointing to catalog storage locations.
        """
        ...

    def list_catalogs(self) -> list[Any]:
        """List all registered catalogs with their metadata.

        Returns:
            A list of catalog objects or dictionaries containing catalog
            information. The exact type depends on the implementation.
        """
        ...

    def add_catalog(self, request: Any) -> Any:
        """Add a new catalog to the registry.

        Args:
            request: A request object containing the catalog information
                to add. The exact type depends on the implementation.

        Returns:
            The created catalog object or a confirmation response.
            The exact type depends on the implementation.
        """
        ...

    def remove_catalog(self, catalog_id: str) -> bool:
        """Remove a catalog from the registry.

        Args:
            catalog_id: The unique identifier of the catalog to remove.

        Returns:
            True if the catalog was found and removed, False if no catalog
            existed with the given identifier.
        """
        ...
