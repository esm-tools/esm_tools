"""Middleware components for the ESM-Catalog STAC API.

This module provides middleware for request logging, correlation ID tracking,
and other cross-cutting concerns in the STAC API.
"""

from __future__ import annotations

import time
import uuid
from typing import Callable

from loguru import logger
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.requests import Request
from starlette.responses import Response


class CorrelationIdMiddleware(BaseHTTPMiddleware):
    """Middleware that adds a correlation ID to each request.

    The correlation ID is a short UUID (first 8 characters) that can be used
    to trace requests through the system. It is stored in request.state.correlation_id
    and can be accessed by downstream handlers.

    Example:
        ```python
        from starlette.applications import Starlette
        from esm_catalog.api.middleware import CorrelationIdMiddleware

        app = Starlette()
        app.add_middleware(CorrelationIdMiddleware)

        @app.route("/")
        async def homepage(request):
            correlation_id = request.state.correlation_id
            return PlainTextResponse(f"Request ID: {correlation_id}")
        ```
    """

    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """Add correlation ID to request state and process the request.

        Args:
            request: The incoming HTTP request.
            call_next: The next middleware or route handler in the chain.

        Returns:
            The HTTP response from downstream handlers.
        """
        correlation_id = str(uuid.uuid4())[:8]
        request.state.correlation_id = correlation_id
        response = await call_next(request)
        return response


class RequestLoggingMiddleware(BaseHTTPMiddleware):
    """Middleware that logs incoming requests and outgoing responses.

    This middleware logs:
    - Incoming requests at INFO level with method, path, and client IP
    - Outgoing responses at INFO level with status code and request duration
    - Each log entry includes a correlation ID for request tracing

    The correlation ID is also stored in request.state.correlation_id for
    access by downstream handlers.

    Example:
        ```python
        from starlette.applications import Starlette
        from esm_catalog.api.middleware import RequestLoggingMiddleware

        app = Starlette()
        app.add_middleware(RequestLoggingMiddleware)
        ```

    Log output example:
        ```
        INFO | [abc12345] --> GET /collections from 127.0.0.1
        INFO | [abc12345] <-- 200 OK in 45.23ms
        ```
    """

    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """Log the request, process it, and log the response with timing.

        Args:
            request: The incoming HTTP request.
            call_next: The next middleware or route handler in the chain.

        Returns:
            The HTTP response from downstream handlers.
        """
        # Generate correlation ID
        correlation_id = str(uuid.uuid4())[:8]
        request.state.correlation_id = correlation_id

        # Extract client IP (handle proxied requests)
        client_ip = self._get_client_ip(request)

        # Log incoming request
        logger.info(
            "[{correlation_id}] --> {method} {path} from {client_ip}",
            correlation_id=correlation_id,
            method=request.method,
            path=request.url.path,
            client_ip=client_ip,
        )

        # Process request and measure timing
        start_time = time.perf_counter()
        response = await call_next(request)
        duration_ms = (time.perf_counter() - start_time) * 1000

        # Log response
        logger.info(
            "[{correlation_id}] <-- {status_code} in {duration:.2f}ms",
            correlation_id=correlation_id,
            status_code=response.status_code,
            duration=duration_ms,
        )

        return response

    def _get_client_ip(self, request: Request) -> str:
        """Extract the client IP address from the request.

        Checks for X-Forwarded-For header first (for proxied requests),
        then falls back to the direct client address.

        Args:
            request: The incoming HTTP request.

        Returns:
            The client IP address as a string, or "unknown" if not available.
        """
        # Check for forwarded header (common with reverse proxies)
        forwarded_for = request.headers.get("x-forwarded-for")
        if forwarded_for:
            # X-Forwarded-For can contain multiple IPs; the first is the client
            return forwarded_for.split(",")[0].strip()

        # Fall back to direct client address
        if request.client:
            return request.client.host

        return "unknown"
