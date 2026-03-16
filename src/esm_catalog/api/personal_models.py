"""Pydantic request/response models for the personal-collections API.

These models define the HTTP-layer data shapes.  The storage layer has its
own Pydantic models (``PersonalCollection``, ``CollectionShare``, etc.) which
are mapped to/from these at the route level.
"""

from __future__ import annotations

from datetime import datetime

from pydantic import BaseModel, Field

from esm_catalog.storage.personal import Role


# ---------------------------------------------------------------------------
# Collection models
# ---------------------------------------------------------------------------


class CollectionCreateRequest(BaseModel):
    """POST body for creating a personal collection."""

    name: str = Field(..., min_length=1, max_length=256)
    description: str = ""
    parent_id: str | None = None


class CollectionUpdateRequest(BaseModel):
    """PATCH body for updating a personal collection (all fields optional)."""

    name: str | None = Field(default=None, min_length=1, max_length=256)
    description: str | None = None
    parent_id: str | None = Field(default=None, description="Set to null to move to root")


class CollectionResponse(BaseModel):
    """Representation of a personal collection returned by the API."""

    id: str
    owner: str
    name: str
    description: str = ""
    labels: list[str] = Field(default_factory=list)
    parent_id: str | None = None
    created_at: datetime
    updated_at: datetime


class CollectionDetailResponse(CollectionResponse):
    """Extended representation that includes item IDs."""

    item_ids: list[str] = Field(default_factory=list)


class CollectionListResponse(BaseModel):
    """Response for GET /users/{username}/collections."""

    collections: list[CollectionResponse]
    total: int


# ---------------------------------------------------------------------------
# Item management models
# ---------------------------------------------------------------------------


class AddItemsRequest(BaseModel):
    """POST body for adding items to a collection."""

    item_ids: list[str] = Field(..., min_length=1)


class AddItemsResponse(BaseModel):
    """Response after adding items."""

    added: int
    collection_id: str


# ---------------------------------------------------------------------------
# Label models
# ---------------------------------------------------------------------------


class LabelCreateRequest(BaseModel):
    """POST body for creating a label."""

    name: str = Field(..., min_length=1, max_length=64)
    color: str = Field(
        default="#808080",
        pattern=r"^#[0-9a-fA-F]{6}$",
        description="Hex colour code, e.g. #ff5733",
    )


class LabelResponse(BaseModel):
    """Representation of a user label."""

    id: str
    owner: str
    name: str
    color: str


class LabelListResponse(BaseModel):
    """Response for GET /users/{username}/labels."""

    labels: list[LabelResponse]
    total: int


# ---------------------------------------------------------------------------
# Share models
# ---------------------------------------------------------------------------


class ShareCreateRequest(BaseModel):
    """POST body for sharing a collection."""

    username: str = Field(..., min_length=1)
    role: Role = Role.viewer


class ShareResponse(BaseModel):
    """Representation of a single share grant."""

    id: str
    collection_id: str
    username: str
    role: Role


class ShareListResponse(BaseModel):
    """Response for GET .../shares."""

    shares: list[ShareResponse]
    total: int


# ---------------------------------------------------------------------------
# Tree models
# ---------------------------------------------------------------------------


class TreeNodeResponse(BaseModel):
    """A single node in the user's sidebar tree."""

    id: str
    owner: str
    name: str
    type: str
    collection_id: str | None = None
    parent_id: str | None = None
    position: int = 0
    children: list[TreeNodeResponse] = Field(default_factory=list)


class TreeResponse(BaseModel):
    """Response for GET /users/{username}/tree."""

    roots: list[TreeNodeResponse]


class TreeNodeUpdateRequest(BaseModel):
    """PATCH body for reordering a tree node."""

    node_id: str
    parent_id: str | None = None
    position: int = 0


# Resolve forward references for recursive TreeNodeResponse
TreeNodeResponse.model_rebuild()
