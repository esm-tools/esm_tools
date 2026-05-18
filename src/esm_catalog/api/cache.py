"""TTL-based caching for ESM-Catalog API.

Provides thread-safe caching with configurable time-to-live (TTL) for
expensive operations like queryables computation and collection listing.

Example usage::

    cache = QueryablesCache(ttl_seconds=300)

    def compute_queryables():
        # expensive database queries...
        return {"properties": {...}}

    result = cache.get_or_compute("global", compute_queryables)
"""

from __future__ import annotations

import threading
import time
from typing import Callable, Generic, TypeVar

from loguru import logger

T = TypeVar("T")


class _CacheEntry(Generic[T]):
    """Internal cache entry with value and expiration timestamp."""

    __slots__ = ("value", "expires_at")

    def __init__(self, value: T, expires_at: float) -> None:
        self.value = value
        self.expires_at = expires_at

    def is_expired(self) -> bool:
        """Check if this entry has expired."""
        return time.monotonic() > self.expires_at


class QueryablesCache:
    """Thread-safe TTL cache for queryables endpoint responses.

    Caches the expensive DISTINCT queries used to populate enum values
    in the /queryables schema. Entries expire after the configured TTL.

    Attributes:
        ttl_seconds: Time-to-live for cache entries in seconds.

    Example::

        cache = QueryablesCache(ttl_seconds=300)

        # First call computes the value
        result = cache.get_or_compute("key", lambda: expensive_computation())

        # Subsequent calls within TTL return cached value
        result = cache.get_or_compute("key", lambda: expensive_computation())

        # Invalidate when data changes
        cache.invalidate("key")
    """

    def __init__(self, ttl_seconds: int = 300) -> None:
        """Initialize the queryables cache.

        Args:
            ttl_seconds: Time-to-live for cache entries in seconds.
                         Defaults to 300 (5 minutes).
        """
        self._ttl_seconds = ttl_seconds
        self._cache: dict[str, _CacheEntry[dict]] = {}
        self._lock = threading.Lock()
        logger.debug(
            "Initialized QueryablesCache with TTL={} seconds", ttl_seconds
        )

    @property
    def ttl_seconds(self) -> int:
        """Return the configured TTL in seconds."""
        return self._ttl_seconds

    def get_or_compute(
        self, key: str, compute_fn: Callable[[], dict]
    ) -> dict:
        """Get cached value or compute and cache if missing/expired.

        Thread-safe: only one thread will execute compute_fn for a given
        key at a time. Other threads will wait for the result.

        Args:
            key: Cache key (e.g., "global" or a collection ID).
            compute_fn: Zero-argument callable that returns the value
                        to cache. Only called if cache miss or expired.

        Returns:
            The cached or freshly computed dict value.

        Raises:
            Exception: Re-raises any exception from compute_fn.
        """
        with self._lock:
            entry = self._cache.get(key)
            if entry is not None and not entry.is_expired():
                logger.trace("Cache hit for queryables key '{}'", key)
                return entry.value

            # Cache miss or expired - compute new value
            logger.debug(
                "Cache {} for queryables key '{}', computing...",
                "expired" if entry else "miss",
                key,
            )

        # Compute outside lock to avoid blocking other keys
        # (but we hold lock during check-and-set to prevent races)
        value = compute_fn()

        with self._lock:
            expires_at = time.monotonic() + self._ttl_seconds
            self._cache[key] = _CacheEntry(value, expires_at)
            logger.debug(
                "Cached queryables key '{}', expires in {} seconds",
                key,
                self._ttl_seconds,
            )

        return value

    def invalidate(self, key: str | None = None) -> None:
        """Invalidate cache entries.

        Args:
            key: Specific key to invalidate, or None to clear all entries.
        """
        with self._lock:
            if key is None:
                count = len(self._cache)
                self._cache.clear()
                logger.debug("Invalidated all {} queryables cache entries", count)
            elif key in self._cache:
                del self._cache[key]
                logger.debug("Invalidated queryables cache key '{}'", key)
            else:
                logger.trace(
                    "Attempted to invalidate non-existent key '{}'", key
                )


class CollectionCache:
    """Thread-safe TTL cache for collection list responses.

    Caches the list of available collections to avoid repeated
    filesystem or database queries.

    Attributes:
        ttl_seconds: Time-to-live for cache entries in seconds.

    Example::

        cache = CollectionCache(ttl_seconds=60)

        # First call computes the value
        collections = cache.get_or_compute(
            "all", lambda: list_collections_from_db()
        )

        # Invalidate when collections change
        cache.invalidate()
    """

    def __init__(self, ttl_seconds: int = 300) -> None:
        """Initialize the collection cache.

        Args:
            ttl_seconds: Time-to-live for cache entries in seconds.
                         Defaults to 300 (5 minutes).
        """
        self._ttl_seconds = ttl_seconds
        self._cache: dict[str, _CacheEntry[dict]] = {}
        self._lock = threading.Lock()
        logger.debug(
            "Initialized CollectionCache with TTL={} seconds", ttl_seconds
        )

    @property
    def ttl_seconds(self) -> int:
        """Return the configured TTL in seconds."""
        return self._ttl_seconds

    def get_or_compute(
        self, key: str, compute_fn: Callable[[], dict]
    ) -> dict:
        """Get cached value or compute and cache if missing/expired.

        Thread-safe: only one thread will execute compute_fn for a given
        key at a time. Other threads will wait for the result.

        Args:
            key: Cache key (e.g., "all" or a specific collection ID).
            compute_fn: Zero-argument callable that returns the value
                        to cache. Only called if cache miss or expired.

        Returns:
            The cached or freshly computed dict value.

        Raises:
            Exception: Re-raises any exception from compute_fn.
        """
        with self._lock:
            entry = self._cache.get(key)
            if entry is not None and not entry.is_expired():
                logger.trace("Cache hit for collection key '{}'", key)
                return entry.value

            # Cache miss or expired - compute new value
            logger.debug(
                "Cache {} for collection key '{}', computing...",
                "expired" if entry else "miss",
                key,
            )

        # Compute outside lock to avoid blocking other keys
        value = compute_fn()

        with self._lock:
            expires_at = time.monotonic() + self._ttl_seconds
            self._cache[key] = _CacheEntry(value, expires_at)
            logger.debug(
                "Cached collection key '{}', expires in {} seconds",
                key,
                self._ttl_seconds,
            )

        return value

    def invalidate(self, key: str | None = None) -> None:
        """Invalidate cache entries.

        Args:
            key: Specific key to invalidate, or None to clear all entries.
        """
        with self._lock:
            if key is None:
                count = len(self._cache)
                self._cache.clear()
                logger.debug("Invalidated all {} collection cache entries", count)
            elif key in self._cache:
                del self._cache[key]
                logger.debug("Invalidated collection cache key '{}'", key)
            else:
                logger.trace(
                    "Attempted to invalidate non-existent key '{}'", key
                )
