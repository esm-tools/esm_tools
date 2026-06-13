"""Personal collections REST API routes.

Provides endpoints for managing personal collections of STAC items,
including labels, sharing, and hierarchical tree organisation.

Endpoints::

    POST   /users/{username}/collections                              - Create collection
    GET    /users/{username}/collections                              - List collections
    GET    /users/{username}/collections/{collection_id}              - Get collection detail
    PATCH  /users/{username}/collections/{collection_id}              - Update collection
    DELETE /users/{username}/collections/{collection_id}              - Delete collection

    POST   /users/{username}/collections/{collection_id}/items        - Add items
    DELETE /users/{username}/collections/{collection_id}/items/{item_id} - Remove item

    GET    /users/{username}/labels                                   - List labels
    POST   /users/{username}/labels                                   - Create label
    DELETE /users/{username}/labels/{label_id}                        - Delete label

    POST   /users/{username}/collections/{collection_id}/shares       - Share collection
    GET    /users/{username}/collections/{collection_id}/shares       - List shares
    DELETE /users/{username}/collections/{collection_id}/shares/{share_id} - Revoke share

    GET    /users/{username}/tree                                     - Get tree
    PATCH  /users/{username}/tree                                     - Reorder tree node
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from fastapi import APIRouter, HTTPException, Request, Response
from loguru import logger

from esm_catalog.api.personal_models import (
    AddItemsRequest,
    AddItemsResponse,
    CollectionCreateRequest,
    CollectionDetailResponse,
    CollectionListResponse,
    CollectionResponse,
    CollectionUpdateRequest,
    LabelCreateRequest,
    LabelListResponse,
    LabelResponse,
    ShareCreateRequest,
    ShareListResponse,
    ShareResponse,
    TreeNodeResponse,
    TreeNodeUpdateRequest,
    TreeResponse,
)
from esm_catalog.storage.personal import PersonalCollectionStore, Role, TreeNode

if TYPE_CHECKING:
    from esm_catalog.api.auth import Authenticator, User


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _collection_to_response(col) -> CollectionResponse:
    """Map a storage ``PersonalCollection`` to an API response model."""
    return CollectionResponse(
        id=col.id,
        owner=col.owner,
        name=col.name,
        description=col.description_md,
        labels=col.labels,
        parent_id=col.parent_id,
        created_at=col.created_at,
        updated_at=col.updated_at,
    )


def _share_to_response(share) -> ShareResponse:
    """Map a storage ``CollectionShare`` to an API response model."""
    return ShareResponse(
        id=share.id,
        collection_id=share.collection_id,
        username=share.username,
        role=share.role,
    )


def _label_to_response(label) -> LabelResponse:
    """Map a storage ``Label`` to an API response model."""
    return LabelResponse(
        id=label.id,
        owner=label.owner,
        name=label.name,
        color=label.color,
    )


def _build_tree_hierarchy(flat_nodes: list[TreeNode]) -> list[TreeNodeResponse]:
    """Assemble a flat list of ``TreeNode`` objects into a nested hierarchy."""
    node_map: dict[str, TreeNodeResponse] = {}
    for n in flat_nodes:
        node_map[n.id] = TreeNodeResponse(
            id=n.id,
            owner=n.owner,
            name=n.name,
            type=n.type,
            collection_id=n.collection_id,
            parent_id=n.parent_id,
            position=n.position,
        )

    roots: list[TreeNodeResponse] = []
    for n in flat_nodes:
        resp = node_map[n.id]
        if n.parent_id and n.parent_id in node_map:
            node_map[n.parent_id].children.append(resp)
        else:
            roots.append(resp)

    return roots


# ---------------------------------------------------------------------------
# Router factory
# ---------------------------------------------------------------------------


def create_personal_router(
    store: PersonalCollectionStore,
    authenticator: "Authenticator | None" = None,
) -> APIRouter:
    """Create the personal-collections router with injected dependencies.

    Args:
        store: Storage backend for personal collections.
        authenticator: Optional authenticator for access control.

    Returns:
        FastAPI router with personal-collection endpoints.
    """
    router = APIRouter(prefix="/users", tags=["Personal Collections"])

    # ------------------------------------------------------------------
    # Auth helpers
    # ------------------------------------------------------------------

    async def _authenticate(request: Request) -> "User | None":
        """Return the authenticated user, or ``None`` when auth is disabled."""
        if authenticator is None:
            return None
        return await authenticator.authenticate(request)

    async def _require_owner_or_admin(
        request: Request, username: str
    ) -> "User":
        """Ensure the caller owns the ``{username}`` resource or is an admin.

        Returns the authenticated ``User`` on success.
        Raises ``HTTPException(401)`` or ``HTTPException(403)`` on failure.
        """
        if authenticator is None:
            from esm_catalog.api.auth import User as UserCls

            return UserCls(name=username, admin=True)

        if not authenticator.requires_auth("personal_collections", "write"):
            from esm_catalog.api.auth import User as UserCls

            return UserCls(name=username, admin=False)

        user = await authenticator.authenticate(request)
        if user is None:
            raise HTTPException(status_code=401, detail="Authentication required")
        if user.name != username and not user.admin:
            raise HTTPException(status_code=403, detail="Permission denied")
        return user

    async def _require_read_access(
        request: Request, username: str, collection_id: str
    ) -> "User | None":
        """Allow the owner, an admin, or anyone with a share to read.

        Returns the authenticated user (``None`` when auth is disabled).
        Raises ``HTTPException(401)`` or ``HTTPException(403)`` otherwise.
        """
        if authenticator is None:
            return None

        # When no real authentication is required (e.g. NoAuthenticator),
        # return None so the caller uses the URL username for ownership checks.
        if not authenticator.requires_auth("personal_collections", "read"):
            return None

        user = await authenticator.authenticate(request)
        if user is None:
            raise HTTPException(status_code=401, detail="Authentication required")
        if user.name == username or user.admin:
            return user

        has_access = store.check_access(
            collection_id=collection_id,
            username=user.name,
            required_role=Role.viewer,
        )
        if not has_access:
            raise HTTPException(status_code=403, detail="Permission denied")
        return user

    # ------------------------------------------------------------------
    # Collection CRUD
    # ------------------------------------------------------------------

    @router.post(
        "/{username}/collections",
        response_model=CollectionResponse,
        status_code=201,
    )
    async def create_collection(
        username: str,
        body: CollectionCreateRequest,
        request: Request,
    ) -> CollectionResponse:
        """Create a new personal collection for the user."""
        await _require_owner_or_admin(request, username)
        col = store.create_collection(
            owner=username,
            name=body.name,
            description=body.description,
            parent_id=body.parent_id,
        )
        return _collection_to_response(col)

    @router.get(
        "/{username}/collections",
        response_model=CollectionListResponse,
    )
    async def list_collections(
        username: str,
        request: Request,
    ) -> CollectionListResponse:
        """List collections visible to the caller.

        When the caller is the owner all owned collections are returned.
        When the caller is a different user, only shared collections are
        included (handled at the storage layer).
        """
        caller = await _authenticate(request)
        # Determine whose perspective to list from.
        accessor = caller.name if caller else username
        collections = store.list_collections(accessor)
        # If the caller is not the owner, filter to only collections
        # owned by {username} (the store returns all accessible).
        if accessor != username:
            collections = [c for c in collections if c.owner == username]
        results = [_collection_to_response(c) for c in collections]
        return CollectionListResponse(collections=results, total=len(results))

    @router.get(
        "/{username}/collections/{collection_id}",
        response_model=CollectionDetailResponse,
    )
    async def get_collection(
        username: str,
        collection_id: str,
        request: Request,
    ) -> CollectionDetailResponse:
        """Get full details of a collection including item IDs."""
        caller = await _require_read_access(request, username, collection_id)
        accessor = caller.name if caller else username
        try:
            col = store.get_collection(collection_id, accessor)
        except KeyError:
            raise HTTPException(
                status_code=404,
                detail=f"Collection '{collection_id}' not found",
            )
        except PermissionError:
            raise HTTPException(status_code=403, detail="Permission denied")

        item_ids = store.get_items(collection_id, accessor)
        resp = CollectionDetailResponse(
            id=col.id,
            owner=col.owner,
            name=col.name,
            description=col.description_md,
            labels=col.labels,
            parent_id=col.parent_id,
            created_at=col.created_at,
            updated_at=col.updated_at,
            item_ids=item_ids,
        )
        return resp

    @router.patch(
        "/{username}/collections/{collection_id}",
        response_model=CollectionResponse,
    )
    async def update_collection(
        username: str,
        collection_id: str,
        body: CollectionUpdateRequest,
        request: Request,
    ) -> CollectionResponse:
        """Update a collection's name, description, or parent."""
        user = await _require_owner_or_admin(request, username)
        updates: dict = {}
        if body.name is not None:
            updates["name"] = body.name
        if body.description is not None:
            updates["description_md"] = body.description
        if body.parent_id is not None:
            updates["parent_id"] = body.parent_id

        try:
            col = store.update_collection(
                collection_id=collection_id,
                username=user.name,
                updates=updates,
            )
        except KeyError:
            raise HTTPException(
                status_code=404,
                detail=f"Collection '{collection_id}' not found",
            )
        except PermissionError:
            raise HTTPException(status_code=403, detail="Permission denied")

        return _collection_to_response(col)

    @router.delete(
        "/{username}/collections/{collection_id}",
        status_code=204,
    )
    async def delete_collection(
        username: str,
        collection_id: str,
        request: Request,
    ) -> Response:
        """Delete a personal collection (does not remove underlying items)."""
        user = await _require_owner_or_admin(request, username)
        try:
            store.delete_collection(collection_id, user.name)
        except KeyError:
            raise HTTPException(
                status_code=404,
                detail=f"Collection '{collection_id}' not found",
            )
        except PermissionError:
            raise HTTPException(status_code=403, detail="Permission denied")
        return Response(status_code=204)

    # ------------------------------------------------------------------
    # Item management
    # ------------------------------------------------------------------

    @router.post(
        "/{username}/collections/{collection_id}/items",
        response_model=AddItemsResponse,
        status_code=201,
    )
    async def add_items(
        username: str,
        collection_id: str,
        body: AddItemsRequest,
        request: Request,
    ) -> AddItemsResponse:
        """Add catalog items to a personal collection by their IDs."""
        user = await _require_owner_or_admin(request, username)
        try:
            store.add_items(
                collection_id=collection_id,
                username=user.name,
                item_ids=body.item_ids,
            )
        except KeyError:
            raise HTTPException(
                status_code=404,
                detail=f"Collection '{collection_id}' not found",
            )
        except PermissionError:
            raise HTTPException(status_code=403, detail="Permission denied")
        return AddItemsResponse(added=len(body.item_ids), collection_id=collection_id)

    @router.delete(
        "/{username}/collections/{collection_id}/items/{item_id}",
        status_code=204,
    )
    async def remove_item(
        username: str,
        collection_id: str,
        item_id: str,
        request: Request,
    ) -> Response:
        """Remove a single item from a personal collection."""
        user = await _require_owner_or_admin(request, username)
        try:
            store.remove_item(
                collection_id=collection_id,
                username=user.name,
                item_id=item_id,
            )
        except KeyError:
            raise HTTPException(
                status_code=404,
                detail=f"Collection '{collection_id}' not found",
            )
        except PermissionError:
            raise HTTPException(status_code=403, detail="Permission denied")
        return Response(status_code=204)

    # ------------------------------------------------------------------
    # Labels
    # ------------------------------------------------------------------

    @router.get(
        "/{username}/labels",
        response_model=LabelListResponse,
    )
    async def list_labels(
        username: str,
        request: Request,
    ) -> LabelListResponse:
        """List all labels belonging to a user."""
        labels = store.list_labels(owner=username)
        results = [_label_to_response(lb) for lb in labels]
        return LabelListResponse(labels=results, total=len(results))

    @router.post(
        "/{username}/labels",
        response_model=LabelResponse,
        status_code=201,
    )
    async def create_label(
        username: str,
        body: LabelCreateRequest,
        request: Request,
    ) -> LabelResponse:
        """Create a new label for organising collections."""
        await _require_owner_or_admin(request, username)
        label = store.create_label(
            owner=username, name=body.name, color=body.color
        )
        return _label_to_response(label)

    @router.delete(
        "/{username}/labels/{label_id}",
        status_code=204,
    )
    async def delete_label(
        username: str,
        label_id: str,
        request: Request,
    ) -> Response:
        """Delete a user label.  Associated collection-label links are removed."""
        await _require_owner_or_admin(request, username)
        deleted = store.delete_label(owner=username, label_id=label_id)
        if not deleted:
            raise HTTPException(
                status_code=404,
                detail=f"Label '{label_id}' not found",
            )
        return Response(status_code=204)

    # ------------------------------------------------------------------
    # Shares
    # ------------------------------------------------------------------

    @router.post(
        "/{username}/collections/{collection_id}/shares",
        response_model=ShareResponse,
        status_code=201,
    )
    async def share_collection(
        username: str,
        collection_id: str,
        body: ShareCreateRequest,
        request: Request,
    ) -> ShareResponse:
        """Grant another user access to a personal collection."""
        user = await _require_owner_or_admin(request, username)
        try:
            share = store.share_collection(
                collection_id=collection_id,
                owner=user.name,
                target_user=body.username,
                role=body.role,
            )
        except KeyError:
            raise HTTPException(
                status_code=404,
                detail=f"Collection '{collection_id}' not found",
            )
        except PermissionError:
            raise HTTPException(status_code=403, detail="Permission denied")
        except ValueError as exc:
            raise HTTPException(status_code=400, detail=str(exc))
        return _share_to_response(share)

    @router.get(
        "/{username}/collections/{collection_id}/shares",
        response_model=ShareListResponse,
    )
    async def list_shares(
        username: str,
        collection_id: str,
        request: Request,
    ) -> ShareListResponse:
        """List all share grants for a collection."""
        user = await _require_owner_or_admin(request, username)
        try:
            shares = store.list_shares(
                collection_id=collection_id,
                username=user.name,
            )
        except PermissionError:
            raise HTTPException(status_code=403, detail="Permission denied")
        results = [_share_to_response(s) for s in shares]
        return ShareListResponse(shares=results, total=len(results))

    @router.delete(
        "/{username}/collections/{collection_id}/shares/{share_id}",
        status_code=204,
    )
    async def revoke_share(
        username: str,
        collection_id: str,
        share_id: str,
        request: Request,
    ) -> Response:
        """Revoke a share grant."""
        user = await _require_owner_or_admin(request, username)
        try:
            store.revoke_share(
                collection_id=collection_id,
                owner=user.name,
                share_id=share_id,
            )
        except KeyError:
            raise HTTPException(
                status_code=404,
                detail=f"Share '{share_id}' not found",
            )
        except PermissionError:
            raise HTTPException(status_code=403, detail="Permission denied")
        return Response(status_code=204)

    # ------------------------------------------------------------------
    # Tree
    # ------------------------------------------------------------------

    @router.get(
        "/{username}/tree",
        response_model=TreeResponse,
    )
    async def get_tree(
        username: str,
        request: Request,
    ) -> TreeResponse:
        """Return the hierarchical tree structure for a user's collections."""
        flat_nodes = store.get_tree(owner=username)
        roots = _build_tree_hierarchy(flat_nodes)
        return TreeResponse(roots=roots)

    @router.patch(
        "/{username}/tree",
        response_model=TreeResponse,
    )
    async def update_tree(
        username: str,
        body: TreeNodeUpdateRequest,
        request: Request,
    ) -> TreeResponse:
        """Create, rename, delete, or move nodes in the user's collection tree."""
        await _require_owner_or_admin(request, username)
        try:
            if body.action == "create_folder":
                if not body.name:
                    raise HTTPException(status_code=422, detail="'name' is required for create_folder")
                store.create_folder(
                    owner=username,
                    name=body.name,
                    parent_id=body.parent_id,
                )
            elif body.action == "rename":
                if not body.node_id or not body.name:
                    raise HTTPException(status_code=422, detail="'node_id' and 'name' are required for rename")
                store.rename_tree_node(owner=username, node_id=body.node_id, name=body.name)
            elif body.action == "delete_folder":
                if not body.node_id:
                    raise HTTPException(status_code=422, detail="'node_id' is required for delete_folder")
                store.delete_tree_node(owner=username, node_id=body.node_id)
            else:  # "move"
                if not body.node_id:
                    raise HTTPException(status_code=422, detail="'node_id' is required for move")
                parent_id = body.target_folder_id if body.target_folder_id is not None else body.parent_id
                store.update_tree_node(
                    owner=username,
                    node_id=body.node_id,
                    parent_id=parent_id,
                    position=body.position,
                )
        except KeyError as exc:
            raise HTTPException(status_code=404, detail=str(exc))
        # Return the updated tree.
        flat_nodes = store.get_tree(owner=username)
        roots = _build_tree_hierarchy(flat_nodes)
        return TreeResponse(roots=roots)

    return router
