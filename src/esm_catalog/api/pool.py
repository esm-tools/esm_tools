"""Thread-safe DuckDB connection pool for federated catalogs.

Maintains cached connections to catalog files, avoiding the overhead of
opening and closing connections on every HTTP request. Connections are
opened lazily on first access and reused across requests.

DuckDB supports concurrent readers, so multiple threads can safely query
the same connection. Write operations (if added) would need serialization,
but the current API is read-only for catalog queries.

Example::

    pool = CatalogPool()
    db = pool.get("/path/to/catalog.duckdb")
    if db:
        items, total = db.search_items({"collection": "exp-001"})

    # Later: close a specific connection (e.g., when catalog is unregistered)
    pool.close("/path/to/catalog.duckdb")

    # Or refresh after external modification
    pool.refresh("/path/to/catalog.duckdb")

    # On shutdown
    pool.close_all()
"""

from __future__ import annotations

import threading
from pathlib import Path
from typing import TYPE_CHECKING

from loguru import logger

if TYPE_CHECKING:
    from esm_catalog.storage.duckdb import CatalogDB


class CatalogPool:
    """Thread-safe pool of CatalogDB connections.

    Maintains at most one connection per catalog path. Connections are
    opened lazily on first access and reused across requests.

    Attributes:
        _lock: Threading lock protecting the connection dictionaries.
        _connections: Mapping from resolved path string to CatalogDB instance.
    """

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._connections: dict[str, CatalogDB] = {}

    def _resolve_path(self, path: str | Path) -> str:
        """Resolve and normalize a catalog path."""
        return str(Path(path).resolve())

    def get(self, path: str | Path) -> CatalogDB | None:
        """Get or open a connection to the catalog at *path*.

        Returns None if the catalog file does not exist. The connection
        is cached and reused on subsequent calls.

        Args:
            path: Path to the catalog.duckdb file.

        Returns:
            CatalogDB instance, or None if file doesn't exist.
        """
        from esm_catalog.storage.duckdb import CatalogDB

        path_str = self._resolve_path(path)

        with self._lock:
            # Return cached connection if available
            if path_str in self._connections:
                return self._connections[path_str]

            # Check if file exists before opening
            if not Path(path_str).exists():
                logger.debug("Catalog not found: {}", path_str)
                return None

            # Open new connection
            try:
                db = CatalogDB(path_str)
                self._connections[path_str] = db
                logger.debug("Opened catalog connection: {}", path_str)
                return db
            except Exception as e:
                logger.error("Failed to open catalog {}: {}", path_str, e)
                return None

    def close(self, path: str | Path) -> bool:
        """Close and remove a specific catalog connection.

        Args:
            path: Path to the catalog.duckdb file.

        Returns:
            True if a connection was closed, False if no connection existed.
        """
        path_str = self._resolve_path(path)

        with self._lock:
            if path_str not in self._connections:
                return False

            try:
                self._connections[path_str].close()
            except Exception as e:
                logger.warning("Error closing catalog {}: {}", path_str, e)

            del self._connections[path_str]
            logger.debug("Closed catalog connection: {}", path_str)
            return True

    def refresh(self, path: str | Path) -> CatalogDB | None:
        """Close and re-open a catalog connection.

        Use this when the underlying DuckDB file has been modified externally
        and you need to pick up the changes.

        Args:
            path: Path to the catalog.duckdb file.

        Returns:
            Fresh CatalogDB instance, or None if file doesn't exist.
        """
        self.close(path)
        return self.get(path)

    def close_all(self) -> None:
        """Close all connections. Call this on server shutdown."""
        with self._lock:
            for path_str, db in list(self._connections.items()):
                try:
                    db.close()
                    logger.debug("Closed catalog connection: {}", path_str)
                except Exception as e:
                    logger.warning("Error closing catalog {}: {}", path_str, e)

            self._connections.clear()
            logger.debug("Closed all catalog connections")

    def is_open(self, path: str | Path) -> bool:
        """Check if a connection is currently open for the given path."""
        path_str = self._resolve_path(path)
        with self._lock:
            return path_str in self._connections

    def open_paths(self) -> list[str]:
        """Return list of paths with currently open connections."""
        with self._lock:
            return list(self._connections.keys())

    def __len__(self) -> int:
        """Return the number of open connections."""
        with self._lock:
            return len(self._connections)
