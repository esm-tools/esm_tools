"""Thread-local DuckDB connection pool for federated catalogs.

Maintains one DuckDB connection *per thread* per catalog path.  DuckDB
connections are not thread-safe: the same connection object must not be
used concurrently from multiple threads (https://duckdb.org/docs/stable/
guides/python/multiple_threads).  Thread-local storage is the standard
solution — each worker thread gets its own connection and never shares it.

Example::

    pool = CatalogPool()
    db = pool.get("/path/to/catalog.duckdb")
    if db:
        items, total = db.search_items({"collection": "exp-001"})

    # Unregister a catalog (new requests will stop seeing it)
    pool.close("/path/to/catalog.duckdb")

    # Refresh after external modification
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
    """Per-thread DuckDB connection pool.

    Each thread that calls :meth:`get` receives its own :class:`CatalogDB`
    instance, preventing concurrent-access crashes caused by sharing a single
    DuckDB connection across threads.

    Attributes:
        _lock: Protects the shared ``_registered_paths`` set.
        _registered_paths: Paths that have been registered with the pool.
        _local: Thread-local storage holding per-thread connection dicts.
    """

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._registered_paths: set[str] = set()
        self._local = threading.local()

    def _resolve_path(self, path: str | Path) -> str:
        """Resolve and normalize a catalog path."""
        return str(Path(path).resolve())

    def _thread_connections(self) -> dict[str, "CatalogDB"]:
        """Return (or initialise) the connection dict for the current thread."""
        if not hasattr(self._local, "connections"):
            self._local.connections = {}
        return self._local.connections

    def get(self, path: str | Path) -> "CatalogDB | None":
        """Return a thread-local connection to the catalog at *path*.

        A new :class:`CatalogDB` is opened the first time this thread
        accesses a given path; subsequent calls from the same thread reuse
        the cached connection.

        Args:
            path: Path to the catalog.duckdb file.

        Returns:
            CatalogDB instance for this thread, or None if the file does not
            exist.
        """
        from esm_catalog.storage.duckdb import CatalogDB

        path_str = self._resolve_path(path)

        # Guard: file must exist
        if not Path(path_str).exists():
            logger.debug("Catalog not found: {}", path_str)
            return None

        # Register the path so open_paths() / __len__ stay accurate
        with self._lock:
            self._registered_paths.add(path_str)

        conns = self._thread_connections()
        if path_str not in conns:
            try:
                db = CatalogDB(path_str)
                conns[path_str] = db
                logger.debug(
                    "Opened catalog connection (thread {}): {}",
                    threading.current_thread().name,
                    path_str,
                )
            except Exception as e:
                logger.error("Failed to open catalog {}: {}", path_str, e)
                return None

        return conns[path_str]

    def close(self, path: str | Path) -> bool:
        """Unregister a catalog path and close this thread's connection to it.

        Other threads may still hold their own connections; those will remain
        open until those threads exit or call :meth:`close_all`.

        Args:
            path: Path to the catalog.duckdb file.

        Returns:
            True (always — the path is removed from the registry regardless).
        """
        path_str = self._resolve_path(path)

        with self._lock:
            self._registered_paths.discard(path_str)

        conns = self._thread_connections()
        if path_str in conns:
            try:
                conns[path_str].close()
            except Exception as e:
                logger.warning("Error closing catalog {}: {}", path_str, e)
            del conns[path_str]
            logger.debug("Closed catalog connection (thread {}): {}",
                         threading.current_thread().name, path_str)

        return True

    def refresh(self, path: str | Path) -> "CatalogDB | None":
        """Close and re-open *this thread's* connection to *path*.

        Use after the underlying DuckDB file has been modified externally.

        Args:
            path: Path to the catalog.duckdb file.

        Returns:
            Fresh CatalogDB instance, or None if the file doesn't exist.
        """
        path_str = self._resolve_path(path)

        # Close this thread's connection (if any) so get() re-opens it
        conns = self._thread_connections()
        if path_str in conns:
            try:
                conns[path_str].close()
            except Exception as e:
                logger.warning("Error closing catalog for refresh {}: {}", path_str, e)
            del conns[path_str]

        return self.get(path)

    def close_all(self) -> None:
        """Unregister all paths and close this thread's connections.

        Intended for the shutdown hook, which runs in the main thread after
        uvicorn has stopped accepting requests (worker threads have exited).
        """
        with self._lock:
            self._registered_paths.clear()

        conns = self._thread_connections()
        for path_str, db in list(conns.items()):
            try:
                db.close()
                logger.debug("Closed catalog connection: {}", path_str)
            except Exception as e:
                logger.warning("Error closing catalog {}: {}", path_str, e)
        conns.clear()
        logger.debug("Closed all catalog connections (thread {})",
                     threading.current_thread().name)

    def is_open(self, path: str | Path) -> bool:
        """Check if this thread currently has an open connection to *path*."""
        path_str = self._resolve_path(path)
        return path_str in self._thread_connections()

    def open_paths(self) -> list[str]:
        """Return the list of registered catalog paths."""
        with self._lock:
            return list(self._registered_paths)

    def __len__(self) -> int:
        """Return the number of registered catalog paths."""
        with self._lock:
            return len(self._registered_paths)
