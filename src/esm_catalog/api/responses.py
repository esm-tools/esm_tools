"""Unified Pydantic response models for the ESM-Catalog API.

This module provides standardized response models for health checks,
readiness probes, queryables endpoints, and error responses.
"""

from __future__ import annotations

from pydantic import BaseModel, Field


class HealthResponse(BaseModel):
    """Health check response indicating API status and resource counts.

    Attributes:
        status: Current health status (e.g., "healthy", "degraded").
        catalogs_registered: Number of catalogs currently registered.
        pool_connections: Number of active database pool connections.
    """

    status: str
    catalogs_registered: int
    pool_connections: int


class ReadinessResponse(BaseModel):
    """Readiness probe response indicating catalog accessibility.

    Attributes:
        ready: Whether the API is ready to serve requests.
        catalogs_accessible: Number of catalogs that are currently accessible.
        catalogs_total: Total number of registered catalogs.
    """

    ready: bool
    catalogs_accessible: int
    catalogs_total: int


class QueryablesProperty(BaseModel):
    """Schema definition for a single queryable property.

    Attributes:
        title: Human-readable title for the property.
        type: JSON Schema type (e.g., "string", "integer", "array").
        format: Optional format specifier (e.g., "date-time", "uri").
        enum: Optional list of allowed values for the property.
    """

    title: str
    type: str
    format: str | None = None
    enum: list[str] | None = None


class QueryablesResponse(BaseModel):
    """OGC API - Features queryables response following JSON Schema format.

    This model represents the queryables endpoint response which describes
    the properties available for filtering in CQL2 queries.

    Attributes:
        schema_: JSON Schema dialect URI (aliased as "$schema").
        id: Unique identifier URI for this schema (aliased as "$id").
        type: JSON Schema type, typically "object".
        title: Human-readable title for the queryables schema.
        properties: Dictionary mapping property names to their schemas.
        additionalProperties: Whether additional properties are allowed.
    """

    schema_: str = Field(alias="$schema")
    id: str = Field(alias="$id")
    type: str
    title: str
    properties: dict[str, QueryablesProperty]
    additionalProperties: bool = True

    model_config = {
        "populate_by_name": True,
    }


class ErrorResponse(BaseModel):
    """Standardized error response for API errors.

    Attributes:
        detail: Human-readable error message describing what went wrong.
        code: Optional machine-readable error code for programmatic handling.
        correlation_id: Optional unique identifier for tracing the request.
    """

    detail: str
    code: str | None = None
    correlation_id: str | None = None


class ExperimentLink(BaseModel):
    rel: str
    href: str
    type: str = "application/json"
    title: str | None = None


class ExperimentCatalog(BaseModel):
    """STAC Catalog object for a single experiment (type='Catalog')."""
    type: str = "Catalog"
    id: str
    stac_version: str = "1.0.0"
    description: str
    title: str | None = None
    links: list[ExperimentLink] = Field(default_factory=list)


class ExperimentSummary(BaseModel):
    id: str
    title: str
    collection_count: int
    href: str


class ExperimentsListResponse(BaseModel):
    experiments: list[ExperimentSummary]
    numberMatched: int
    numberReturned: int
    links: list[ExperimentLink] = Field(default_factory=list)
