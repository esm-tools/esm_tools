"""Pluggable authentication module for the ESM-Catalog API.

Provides a JupyterHub-style authentication pattern where the authenticator
class can be swapped out for different backends (API key, OAuth, SSO, etc.)
without changing the API structure.

The default NoAuthenticator allows all access. For production deployments,
provide a custom authenticator implementation.

Example::

    from esm_catalog.api.auth import Authenticator, User

    class MyInstitutionAuthenticator(Authenticator):
        async def authenticate(self, request: Request) -> User | None:
            token = request.headers.get("Authorization")
            if not token:
                return None
            user_info = await my_sso_client.validate(token)
            if user_info:
                return User(
                    name=user_info["username"],
                    admin=user_info.get("is_admin", False),
                    groups=user_info.get("groups", []),
                )
            return None

        def get_permissions(self, user: User, resource: str) -> set[str]:
            if user.admin:
                return {"read", "write", "delete"}
            if "catalog-managers" in user.groups:
                return {"read", "write"}
            return {"read"}

    # Use in app creation
    app = create_app(
        catalogs=[...],
        authenticator=MyInstitutionAuthenticator(),
    )
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Callable

from fastapi import Depends, HTTPException, Request

if TYPE_CHECKING:
    pass


@dataclass
class User:
    """Authenticated user information.

    Attributes:
        name: Username or identifier.
        admin: Whether the user has admin privileges.
        groups: Optional list of group memberships for RBAC.
        metadata: Optional additional user metadata.
    """

    name: str
    admin: bool = False
    groups: list[str] = field(default_factory=list)
    metadata: dict = field(default_factory=dict)


class Authenticator(ABC):
    """Base class for authentication backends.

    Subclass this to implement custom authentication logic for your
    institution's SSO, OAuth, API keys, or other authentication mechanism.

    The authenticator is called on each request to protected endpoints.
    Return a User object if authentication succeeds, or None if it fails.
    """

    @abstractmethod
    async def authenticate(self, request: "Request") -> User | None:
        """Authenticate a request and return the user if successful.

        Args:
            request: The incoming HTTP request.

        Returns:
            User object if authenticated, None otherwise.
        """
        pass

    def get_permissions(self, user: User, resource: str) -> set[str]:
        """Return permissions for user on resource.

        Override this method to implement Role-Based Access Control (RBAC).
        The default implementation grants read-only access to all users
        and full access to admins.

        Args:
            user: Authenticated user.
            resource: Resource identifier (e.g., "catalogs", "items").

        Returns:
            Set of permission strings (e.g., {"read", "write", "delete"}).
        """
        if user.admin:
            return {"read", "write", "delete"}
        return {"read"}

    def requires_auth(self, resource: str, action: str) -> bool:
        """Check if a resource/action combination requires authentication.

        Override to make certain endpoints public. The default requires
        authentication for all write operations.

        Args:
            resource: Resource identifier.
            action: Action being performed (e.g., "read", "write", "delete").

        Returns:
            True if authentication is required.
        """
        return action in {"write", "delete"}


class NoAuthenticator(Authenticator):
    """Default authenticator that allows all access without authentication.

    All requests are treated as coming from an anonymous admin user.
    Use this for development or single-user deployments.
    """

    async def authenticate(self, request: "Request") -> User | None:
        """Always returns an anonymous admin user."""
        return User(name="anonymous", admin=True)

    def requires_auth(self, resource: str, action: str) -> bool:
        """Never requires authentication."""
        return False


class APIKeyAuthenticator(Authenticator):
    """Simple API key authentication.

    Validates requests against a configured API key passed in the
    X-API-Key header or as a query parameter.

    Example::

        auth = APIKeyAuthenticator(
            api_keys={"admin-key-123": User(name="admin", admin=True)},
        )
    """

    def __init__(
        self,
        api_keys: dict[str, User] | None = None,
        header_name: str = "X-API-Key",
        query_param: str = "api_key",
    ) -> None:
        """Initialize the authenticator.

        Args:
            api_keys: Mapping from API key strings to User objects.
            header_name: HTTP header name to check for API key.
            query_param: Query parameter name to check for API key.
        """
        self.api_keys = api_keys or {}
        self.header_name = header_name
        self.query_param = query_param

    async def authenticate(self, request: "Request") -> User | None:
        """Validate API key from header or query parameter."""
        # Check header first
        key = request.headers.get(self.header_name)

        # Fall back to query parameter
        if not key:
            key = request.query_params.get(self.query_param)

        if not key:
            return None

        return self.api_keys.get(key)


# Future authenticator stubs for reference
# These would be implemented when integrating with specific auth providers


class JupyterHubAuthenticator(Authenticator):
    """JupyterHub OAuth integration for STAC API.

    When running as a JupyterHub service, this authenticator validates
    tokens against the JupyterHub API and extracts user/group information.

    Setup:
        1. Register as a JupyterHub service in jupyterhub_config.py
        2. The service receives JUPYTERHUB_API_TOKEN and JUPYTERHUB_API_URL
        3. User requests include Authorization header with their token

    Example jupyterhub_config.py::

        c.JupyterHub.services = [
            {
                "name": "stac-catalog",
                "url": "http://127.0.0.1:8000",
                "command": ["esm-catalog", "serve", "--port", "8000"],
                "environment": {
                    "JUPYTERHUB_API_URL": "http://127.0.0.1:8081/hub/api",
                },
            }
        ]

    Write permissions are granted to:
        - Hub admins
        - Users in the "catalog-managers" group (configurable)
    """

    def __init__(
        self,
        hub_api_url: str | None = None,
        api_token: str | None = None,
        write_groups: list[str] | None = None,
    ) -> None:
        """Initialize with JupyterHub API settings.

        Args:
            hub_api_url: URL of the JupyterHub API (e.g., http://hub:8081/hub/api).
                        If not provided, uses JUPYTERHUB_API_URL environment variable.
            api_token: Service API token for authenticating with Hub API.
                      If not provided, uses JUPYTERHUB_API_TOKEN environment variable.
            write_groups: List of group names that grant write access.
                         Default: ["catalog-managers"]
        """
        import os

        self.hub_api_url = (
            hub_api_url or os.environ.get("JUPYTERHUB_API_URL", "")
        ).rstrip("/")
        self.api_token = api_token or os.environ.get("JUPYTERHUB_API_TOKEN", "")
        self.write_groups = write_groups or ["catalog-managers"]

        if not self.hub_api_url:
            raise ValueError(
                "JupyterHub API URL required. Set JUPYTERHUB_API_URL or pass hub_api_url."
            )

    async def authenticate(self, request: Request) -> User | None:
        """Validate token against JupyterHub API.

        Extracts the token from the Authorization header and validates it
        against the Hub API to get user information.
        """
        import httpx

        # Extract token from Authorization header
        auth_header = request.headers.get("Authorization", "")
        if auth_header.startswith("Bearer "):
            token = auth_header[7:]
        elif auth_header.startswith("token "):
            token = auth_header[6:]
        else:
            # Also check for JupyterHub's cookie-based auth
            token = request.cookies.get("jupyterhub-services")
            if not token:
                return None

        # Validate token against Hub API
        try:
            async with httpx.AsyncClient() as client:
                resp = await client.get(
                    f"{self.hub_api_url}/user",
                    headers={"Authorization": f"token {token}"},
                    timeout=10,
                )

                if resp.status_code != 200:
                    return None

                user_info = resp.json()

                return User(
                    name=user_info.get("name", "unknown"),
                    admin=user_info.get("admin", False),
                    groups=user_info.get("groups", []),
                    metadata={
                        "kind": user_info.get("kind", "user"),
                        "server": user_info.get("server"),
                    },
                )

        except httpx.RequestError:
            return None

    def get_permissions(self, user: User, resource: str) -> set[str]:
        """Return permissions based on Hub admin status or group membership.

        Write/delete access is granted to:
            - Hub admins
            - Users in any of the configured write_groups
        """
        # Admins get full access
        if user.admin:
            return {"read", "write", "delete"}

        # Check group membership for write access
        user_groups = set(user.groups)
        write_groups = set(self.write_groups)
        if user_groups & write_groups:
            return {"read", "write", "delete"}

        # Default: read-only
        return {"read"}


# ---------------------------------------------------------------------------
# FastAPI Dependency Injection Helpers
# ---------------------------------------------------------------------------


def create_auth_dependency(
    authenticator: Authenticator,
    resource: str,
    action: str,
) -> Callable:
    """Create a FastAPI dependency for authentication and authorization.

    Use this to create Depends() callables for route protection.

    Example::

        auth = APIKeyAuthenticator(api_keys={...})

        # Create dependencies for different permission levels
        require_read = create_auth_dependency(auth, "catalogs", "read")
        require_write = create_auth_dependency(auth, "catalogs", "write")

        @router.get("/catalogs")
        async def list_catalogs(user: User = Depends(require_read)):
            ...

        @router.post("/catalogs")
        async def add_catalog(body: CatalogCreate, user: User = Depends(require_write)):
            ...

    Args:
        authenticator: The authenticator instance to use.
        resource: Resource identifier (e.g., "catalogs", "items").
        action: Required action (e.g., "read", "write", "delete").

    Returns:
        An async callable suitable for use with FastAPI Depends().
    """

    async def auth_dependency(request: Request) -> User:
        """FastAPI dependency that authenticates and authorizes the request."""
        # Check if this action requires authentication
        if not authenticator.requires_auth(resource, action):
            # Return anonymous user for public endpoints
            return User(name="anonymous", admin=False)

        # Authenticate the request
        user = await authenticator.authenticate(request)
        if user is None:
            raise HTTPException(
                status_code=401,
                detail="Authentication required",
                headers={"WWW-Authenticate": "Bearer"},
            )

        # Check permissions
        permissions = authenticator.get_permissions(user, resource)
        if action not in permissions:
            raise HTTPException(
                status_code=403,
                detail=f"Permission denied: '{action}' on '{resource}'",
            )

        return user

    return auth_dependency


def create_optional_auth_dependency(authenticator: Authenticator) -> Callable:
    """Create a FastAPI dependency that optionally authenticates.

    Unlike create_auth_dependency, this does not raise errors if
    authentication fails. Instead, it returns None for unauthenticated
    requests. Useful for endpoints where authentication is optional
    but provides additional features.

    Example::

        optional_auth = create_optional_auth_dependency(auth)

        @router.get("/items")
        async def list_items(user: User | None = Depends(optional_auth)):
            if user and user.admin:
                return all_items()  # Admins see everything
            return public_items()  # Others see only public

    Args:
        authenticator: The authenticator instance to use.

    Returns:
        An async callable suitable for use with FastAPI Depends().
    """

    async def optional_auth_dependency(request: Request) -> User | None:
        """FastAPI dependency that optionally authenticates the request."""
        return await authenticator.authenticate(request)

    return optional_auth_dependency


class AuthDependencies:
    """Container for pre-built auth dependencies.

    Provides convenient access to common permission checks as
    FastAPI Depends() callables.

    Example::

        auth_deps = AuthDependencies(my_authenticator)

        @router.get("/catalogs")
        async def list_catalogs(user: User = Depends(auth_deps.read_catalogs)):
            ...

        @router.post("/catalogs")
        async def add_catalog(user: User = Depends(auth_deps.write_catalogs)):
            ...
    """

    def __init__(self, authenticator: Authenticator) -> None:
        """Initialize with an authenticator.

        Args:
            authenticator: The authenticator to use for all dependencies.
        """
        self.authenticator = authenticator

        # Pre-build common dependencies
        self.read_catalogs = create_auth_dependency(authenticator, "catalogs", "read")
        self.write_catalogs = create_auth_dependency(authenticator, "catalogs", "write")
        self.delete_catalogs = create_auth_dependency(
            authenticator, "catalogs", "delete"
        )

        self.read_items = create_auth_dependency(authenticator, "items", "read")
        self.write_items = create_auth_dependency(authenticator, "items", "write")

        self.optional = create_optional_auth_dependency(authenticator)
