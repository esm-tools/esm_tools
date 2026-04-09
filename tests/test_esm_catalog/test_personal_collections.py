"""Contract tests for the Personal Collections API.

These tests define the expected behavior of the personal collections
endpoints before the implementation is written. They serve as both
specification and regression suite.

Endpoints under test:

    POST   /users/{username}/collections
    GET    /users/{username}/collections
    GET    /users/{username}/collections/{collection_id}
    PATCH  /users/{username}/collections/{collection_id}
    DELETE /users/{username}/collections/{collection_id}
    POST   /users/{username}/collections/{collection_id}/items
    DELETE /users/{username}/collections/{collection_id}/items/{item_id}
    GET    /users/{username}/labels
    POST   /users/{username}/labels
    DELETE /users/{username}/labels/{label_id}
    POST   /users/{username}/collections/{collection_id}/shares
    GET    /users/{username}/collections/{collection_id}/shares
    DELETE /users/{username}/collections/{collection_id}/shares/{share_id}
    GET    /users/{username}/tree
    PATCH  /users/{username}/tree
"""

from __future__ import annotations

import pytest
from fastapi.testclient import TestClient

from esm_catalog.api.auth import Authenticator, User


# ---------------------------------------------------------------------------
# Mock authenticator
# ---------------------------------------------------------------------------


class MockAuthenticator(Authenticator):
    """Test authenticator that returns a pre-configured user.

    Pass ``user=None`` to simulate an unauthenticated request.
    """

    def __init__(self, user: User | None = None) -> None:
        self._user = user

    async def authenticate(self, request) -> User | None:
        return self._user

    def get_permissions(self, user: User, resource: str) -> set[str]:
        if user.admin:
            return {"read", "write", "delete"}
        if "maintainer" in user.groups:
            return {"read", "write", "delete"}
        if "developer" in user.groups:
            return {"read", "write"}
        return {"read"}

    def requires_auth(self, resource: str, action: str) -> bool:
        return True


# ---------------------------------------------------------------------------
# User fixtures
# ---------------------------------------------------------------------------


@pytest.fixture()
def alice() -> User:
    """Owner user -- full privileges."""
    return User(name="alice", admin=False, groups=["owner"])


@pytest.fixture()
def bob() -> User:
    """Maintainer user -- can CRUD items but not delete collection."""
    return User(name="bob", admin=False, groups=["maintainer"])


@pytest.fixture()
def carol() -> User:
    """Viewer user -- read-only access."""
    return User(name="carol", admin=False, groups=[])


@pytest.fixture()
def dave() -> User:
    """Developer user -- can view and edit metadata but not add/remove items."""
    return User(name="dave", admin=False, groups=["developer"])


@pytest.fixture()
def admin_user() -> User:
    """Admin user -- unrestricted access."""
    return User(name="admin", admin=True)


# ---------------------------------------------------------------------------
# App / client factory
# ---------------------------------------------------------------------------


def _make_client(user: User | None, tmp_path) -> TestClient:
    """Build a TestClient wired to a MockAuthenticator for *user*.

    A fresh DuckDB catalog is created in *tmp_path* so that each test
    gets an isolated database.
    """
    from esm_catalog.api.app import create_app
    from esm_catalog.storage.duckdb import CatalogDB

    db_path = tmp_path / "catalog.duckdb"
    with CatalogDB(db_path) as db:
        # Seed a minimal STAC collection so the catalog is not empty.
        db.insert_collection(
            {
                "type": "Collection",
                "id": "exp-fesom",
                "stac_version": "1.0.0",
                "description": "Seeded collection",
                "links": [],
                "title": "exp-fesom",
                "extent": {
                    "spatial": {"bbox": [[-180, -90, 180, 90]]},
                    "temporal": {"interval": [["2000-01-01T00:00:00Z", None]]},
                },
                "license": "proprietary",
            }
        )
        # Seed a STAC item so we have something to add to personal collections.
        db.insert_item(
            {
                "type": "Feature",
                "stac_version": "1.0.0",
                "stac_extensions": [],
                "id": "temp-fesom-2000",
                "collection": "exp-fesom",
                "geometry": {
                    "type": "Polygon",
                    "coordinates": [
                        [[-180, -90], [180, -90], [180, 90], [-180, 90], [-180, -90]]
                    ],
                },
                "bbox": [-180.0, -90.0, 180.0, 90.0],
                "properties": {
                    "datetime": "2000-01-01T00:00:00Z",
                    "experiment": "basic",
                    "component": "fesom",
                    "variable": "temp",
                },
                "assets": {
                    "data": {
                        "href": "/work/exp/output.nc",
                        "type": "application/x-netcdf",
                    }
                },
                "links": [],
            }
        )

    auth = MockAuthenticator(user=user)
    api = create_app(catalogs=[db_path], authenticator=auth)
    return TestClient(api.app)


@pytest.fixture()
def alice_client(alice, tmp_path) -> TestClient:
    return _make_client(alice, tmp_path)


@pytest.fixture()
def bob_client(bob, tmp_path) -> TestClient:
    return _make_client(bob, tmp_path)


@pytest.fixture()
def carol_client(carol, tmp_path) -> TestClient:
    return _make_client(carol, tmp_path)


@pytest.fixture()
def dave_client(dave, tmp_path) -> TestClient:
    return _make_client(dave, tmp_path)


@pytest.fixture()
def anon_client(tmp_path) -> TestClient:
    return _make_client(None, tmp_path)


@pytest.fixture()
def admin_client(admin_user, tmp_path) -> TestClient:
    return _make_client(admin_user, tmp_path)


# ---------------------------------------------------------------------------
# Helper: create a collection and return the response body
# ---------------------------------------------------------------------------


def _create_collection(
    client: TestClient,
    username: str = "alice",
    name: str = "My Favorites",
    description: str = "hand-picked items",
    label_ids: list[str] | None = None,
    parent_id: str | None = None,
) -> tuple[dict, int]:
    """POST a new personal collection and return (json_body, status_code)."""
    payload: dict = {"name": name, "description": description}
    if label_ids is not None:
        payload["label_ids"] = label_ids
    if parent_id is not None:
        payload["parent_id"] = parent_id
    r = client.post(f"/users/{username}/collections", json=payload)
    return r.json(), r.status_code


def _create_label(
    client: TestClient,
    username: str = "alice",
    name: str = "important",
    color: str = "#ff5733",
) -> tuple[dict, int]:
    r = client.post(
        f"/users/{username}/labels", json={"name": name, "color": color}
    )
    return r.json(), r.status_code


# ===================================================================
# 1. CRUD Basics
# ===================================================================


class TestCollectionCRUD:
    """Create, list, get, update, and delete personal collections."""

    def test_create_collection_returns_201(self, alice_client):
        r = alice_client.post(
            "/users/alice/collections",
            json={"name": "My Favorites", "description": "curated items"},
        )
        assert r.status_code == 201

    def test_create_collection_response_has_id(self, alice_client):
        body, status = _create_collection(alice_client)
        assert status == 201
        assert "id" in body
        assert body["name"] == "My Favorites"

    def test_create_collection_response_has_timestamps(self, alice_client):
        body, status = _create_collection(alice_client)
        assert status == 201
        assert "created_at" in body
        assert "updated_at" in body

    def test_create_collection_owner_is_username(self, alice_client):
        body, status = _create_collection(alice_client)
        assert status == 201
        assert body["owner"] == "alice"

    def test_list_collections_returns_200(self, alice_client):
        r = alice_client.get("/users/alice/collections")
        assert r.status_code == 200

    def test_list_collections_empty_initially(self, alice_client):
        r = alice_client.get("/users/alice/collections")
        data = r.json()
        assert data["collections"] == []
        assert data["total"] == 0

    def test_list_collections_after_create(self, alice_client):
        _create_collection(alice_client)
        r = alice_client.get("/users/alice/collections")
        data = r.json()
        assert data["total"] == 1
        assert data["collections"][0]["name"] == "My Favorites"

    def test_get_collection_returns_200(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        r = alice_client.get(f"/users/alice/collections/{cid}")
        assert r.status_code == 200

    def test_get_collection_detail_has_item_ids(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        detail = alice_client.get(f"/users/alice/collections/{cid}").json()
        assert "item_ids" in detail

    def test_get_nonexistent_collection_returns_404(self, alice_client):
        r = alice_client.get("/users/alice/collections/does-not-exist")
        assert r.status_code == 404

    def test_update_collection_returns_200(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        r = alice_client.patch(
            f"/users/alice/collections/{cid}",
            json={"name": "Renamed"},
        )
        assert r.status_code == 200
        assert r.json()["name"] == "Renamed"

    def test_update_collection_partial(self, alice_client):
        """PATCH with only description should leave name unchanged."""
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        r = alice_client.patch(
            f"/users/alice/collections/{cid}",
            json={"description": "new desc"},
        )
        assert r.status_code == 200
        assert r.json()["name"] == "My Favorites"
        assert r.json()["description"] == "new desc"

    def test_update_nonexistent_collection_returns_404(self, alice_client):
        r = alice_client.patch(
            "/users/alice/collections/nonexistent",
            json={"name": "nope"},
        )
        assert r.status_code == 404

    def test_delete_collection_returns_204(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        r = alice_client.delete(f"/users/alice/collections/{cid}")
        assert r.status_code == 204

    def test_delete_collection_is_gone(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.delete(f"/users/alice/collections/{cid}")
        r = alice_client.get(f"/users/alice/collections/{cid}")
        assert r.status_code == 404

    def test_delete_nonexistent_collection_returns_404(self, alice_client):
        r = alice_client.delete("/users/alice/collections/nonexistent")
        assert r.status_code == 404

    def test_create_collection_name_required(self, alice_client):
        r = alice_client.post(
            "/users/alice/collections", json={"description": "no name"}
        )
        assert r.status_code == 422

    def test_create_collection_empty_name_rejected(self, alice_client):
        r = alice_client.post(
            "/users/alice/collections", json={"name": "", "description": "x"}
        )
        assert r.status_code == 422


# ===================================================================
# 2. Items Management
# ===================================================================


class TestItemsManagement:
    """Add items to a personal collection, list items, remove an item."""

    def test_add_items_returns_200(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        r = alice_client.post(
            f"/users/alice/collections/{cid}/items",
            json={"item_ids": ["temp-fesom-2000"]},
        )
        assert r.status_code == 200

    def test_add_items_response_shape(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        r = alice_client.post(
            f"/users/alice/collections/{cid}/items",
            json={"item_ids": ["temp-fesom-2000"]},
        )
        data = r.json()
        assert data["added"] == 1
        assert data["collection_id"] == cid

    def test_item_appears_in_collection_detail(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.post(
            f"/users/alice/collections/{cid}/items",
            json={"item_ids": ["temp-fesom-2000"]},
        )
        detail = alice_client.get(f"/users/alice/collections/{cid}").json()
        assert "temp-fesom-2000" in detail["item_ids"]

    def test_item_count_updates(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.post(
            f"/users/alice/collections/{cid}/items",
            json={"item_ids": ["temp-fesom-2000"]},
        )
        detail = alice_client.get(f"/users/alice/collections/{cid}").json()
        assert detail["item_count"] == 1

    def test_remove_item_returns_204(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.post(
            f"/users/alice/collections/{cid}/items",
            json={"item_ids": ["temp-fesom-2000"]},
        )
        r = alice_client.delete(
            f"/users/alice/collections/{cid}/items/temp-fesom-2000"
        )
        assert r.status_code == 204

    def test_remove_item_decreases_count(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.post(
            f"/users/alice/collections/{cid}/items",
            json={"item_ids": ["temp-fesom-2000"]},
        )
        alice_client.delete(
            f"/users/alice/collections/{cid}/items/temp-fesom-2000"
        )
        detail = alice_client.get(f"/users/alice/collections/{cid}").json()
        assert detail["item_count"] == 0
        assert "temp-fesom-2000" not in detail["item_ids"]

    def test_remove_nonexistent_item_returns_404(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        r = alice_client.delete(
            f"/users/alice/collections/{cid}/items/no-such-item"
        )
        assert r.status_code == 404

    def test_add_items_to_nonexistent_collection_returns_404(self, alice_client):
        r = alice_client.post(
            "/users/alice/collections/nonexistent/items",
            json={"item_ids": ["temp-fesom-2000"]},
        )
        assert r.status_code == 404

    def test_add_items_empty_list_rejected(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        r = alice_client.post(
            f"/users/alice/collections/{cid}/items",
            json={"item_ids": []},
        )
        assert r.status_code == 422

    def test_add_duplicate_item_is_idempotent(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.post(
            f"/users/alice/collections/{cid}/items",
            json={"item_ids": ["temp-fesom-2000"]},
        )
        alice_client.post(
            f"/users/alice/collections/{cid}/items",
            json={"item_ids": ["temp-fesom-2000"]},
        )
        detail = alice_client.get(f"/users/alice/collections/{cid}").json()
        assert detail["item_count"] == 1


# ===================================================================
# 3. RBAC Enforcement
# ===================================================================


class TestRBACEnforcement:
    """Role-Based Access Control checks."""

    def test_owner_can_create_collection(self, alice_client):
        _, status = _create_collection(alice_client)
        assert status == 201

    def test_owner_can_delete_collection(self, alice_client):
        body, _ = _create_collection(alice_client)
        r = alice_client.delete(f"/users/alice/collections/{body['id']}")
        assert r.status_code == 204

    def test_owner_can_add_items(self, alice_client):
        body, _ = _create_collection(alice_client)
        r = alice_client.post(
            f"/users/alice/collections/{body['id']}/items",
            json={"item_ids": ["temp-fesom-2000"]},
        )
        assert r.status_code == 200

    def test_anon_cannot_create_collection(self, anon_client):
        r = anon_client.post(
            "/users/alice/collections",
            json={"name": "Hack", "description": "nope"},
        )
        assert r.status_code == 401

    def test_anon_cannot_list_collections(self, anon_client):
        r = anon_client.get("/users/alice/collections")
        assert r.status_code == 401

    def test_anon_cannot_delete_collection(self, anon_client):
        r = anon_client.delete("/users/alice/collections/some-id")
        assert r.status_code == 401

    def test_anon_cannot_add_items(self, anon_client):
        r = anon_client.post(
            "/users/alice/collections/some-id/items",
            json={"item_ids": ["temp-fesom-2000"]},
        )
        assert r.status_code == 401

    def test_wrong_user_cannot_access_other_user_collections(self, bob_client):
        r = bob_client.get("/users/alice/collections")
        assert r.status_code == 403

    def test_wrong_user_cannot_delete_other_user_collection(self, bob_client):
        r = bob_client.delete("/users/alice/collections/some-id")
        assert r.status_code == 403

    def test_viewer_can_get_shared_collection(self, alice_client, carol_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.post(
            f"/users/alice/collections/{cid}/shares",
            json={"username": "carol", "role": "viewer"},
        )
        r = carol_client.get(f"/users/alice/collections/{cid}")
        assert r.status_code == 200

    def test_viewer_cannot_update_shared_collection(self, alice_client, carol_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.post(
            f"/users/alice/collections/{cid}/shares",
            json={"username": "carol", "role": "viewer"},
        )
        r = carol_client.patch(
            f"/users/alice/collections/{cid}",
            json={"name": "Nope"},
        )
        assert r.status_code == 403

    def test_viewer_cannot_add_items_to_shared_collection(self, alice_client, carol_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.post(
            f"/users/alice/collections/{cid}/shares",
            json={"username": "carol", "role": "viewer"},
        )
        r = carol_client.post(
            f"/users/alice/collections/{cid}/items",
            json={"item_ids": ["temp-fesom-2000"]},
        )
        assert r.status_code == 403

    def test_viewer_cannot_delete_shared_collection(self, alice_client, carol_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.post(
            f"/users/alice/collections/{cid}/shares",
            json={"username": "carol", "role": "viewer"},
        )
        r = carol_client.delete(f"/users/alice/collections/{cid}")
        assert r.status_code == 403

    def test_admin_can_access_any_user_collections(self, admin_client):
        _create_collection(admin_client, username="alice")
        r = admin_client.get("/users/alice/collections")
        assert r.status_code == 200


# ===================================================================
# 4. Labels
# ===================================================================


class TestLabels:
    """Create, list, and delete user labels."""

    def test_create_label_returns_201(self, alice_client):
        r = alice_client.post(
            "/users/alice/labels",
            json={"name": "important", "color": "#ff5733"},
        )
        assert r.status_code == 201

    def test_create_label_response_has_id(self, alice_client):
        body, status = _create_label(alice_client)
        assert status == 201
        assert "id" in body
        assert body["name"] == "important"
        assert body["color"] == "#ff5733"
        assert body["owner"] == "alice"

    def test_list_labels_returns_200(self, alice_client):
        r = alice_client.get("/users/alice/labels")
        assert r.status_code == 200

    def test_list_labels_empty_initially(self, alice_client):
        data = alice_client.get("/users/alice/labels").json()
        assert data["labels"] == []
        assert data["total"] == 0

    def test_list_labels_after_create(self, alice_client):
        _create_label(alice_client)
        data = alice_client.get("/users/alice/labels").json()
        assert data["total"] == 1
        assert data["labels"][0]["name"] == "important"

    def test_delete_label_returns_204(self, alice_client):
        body, _ = _create_label(alice_client)
        lid = body["id"]
        r = alice_client.delete(f"/users/alice/labels/{lid}")
        assert r.status_code == 204

    def test_delete_label_is_gone(self, alice_client):
        body, _ = _create_label(alice_client)
        lid = body["id"]
        alice_client.delete(f"/users/alice/labels/{lid}")
        data = alice_client.get("/users/alice/labels").json()
        assert data["total"] == 0

    def test_delete_nonexistent_label_returns_404(self, alice_client):
        r = alice_client.delete("/users/alice/labels/no-such-label")
        assert r.status_code == 404

    def test_create_label_invalid_color_rejected(self, alice_client):
        r = alice_client.post(
            "/users/alice/labels",
            json={"name": "bad", "color": "not-a-hex"},
        )
        assert r.status_code == 422

    def test_create_label_empty_name_rejected(self, alice_client):
        r = alice_client.post(
            "/users/alice/labels",
            json={"name": "", "color": "#aabbcc"},
        )
        assert r.status_code == 422

    def test_assign_label_to_collection(self, alice_client):
        label_body, _ = _create_label(alice_client)
        lid = label_body["id"]
        col_body, status = _create_collection(alice_client, label_ids=[lid])
        assert status == 201
        assert lid in col_body.get("label_ids", [])

    def test_update_collection_labels(self, alice_client):
        label_body, _ = _create_label(alice_client)
        lid = label_body["id"]
        col_body, _ = _create_collection(alice_client)
        cid = col_body["id"]
        r = alice_client.patch(
            f"/users/alice/collections/{cid}",
            json={"label_ids": [lid]},
        )
        assert r.status_code == 200
        assert lid in r.json()["label_ids"]

    def test_remove_label_from_collection(self, alice_client):
        label_body, _ = _create_label(alice_client)
        lid = label_body["id"]
        col_body, _ = _create_collection(alice_client, label_ids=[lid])
        cid = col_body["id"]
        r = alice_client.patch(
            f"/users/alice/collections/{cid}",
            json={"label_ids": []},
        )
        assert r.status_code == 200
        assert r.json()["label_ids"] == []


# ===================================================================
# 5. Sharing
# ===================================================================


class TestSharing:
    """Share collection with another user, list shares, revoke share."""

    def test_share_collection_returns_201(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        r = alice_client.post(
            f"/users/alice/collections/{cid}/shares",
            json={"username": "bob", "role": "viewer"},
        )
        assert r.status_code == 201

    def test_share_response_has_id_and_role(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        r = alice_client.post(
            f"/users/alice/collections/{cid}/shares",
            json={"username": "bob", "role": "viewer"},
        )
        share = r.json()
        assert "id" in share
        assert share["shared_with"] == "bob"
        assert share["role"] == "viewer"
        assert share["collection_id"] == cid

    def test_list_shares_returns_200(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        r = alice_client.get(f"/users/alice/collections/{cid}/shares")
        assert r.status_code == 200

    def test_list_shares_empty_initially(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        data = alice_client.get(f"/users/alice/collections/{cid}/shares").json()
        assert data["shares"] == []
        assert data["total"] == 0

    def test_list_shares_after_sharing(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.post(
            f"/users/alice/collections/{cid}/shares",
            json={"username": "bob", "role": "viewer"},
        )
        data = alice_client.get(f"/users/alice/collections/{cid}/shares").json()
        assert data["total"] == 1
        assert data["shares"][0]["shared_with"] == "bob"

    def test_revoke_share_returns_204(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        share_r = alice_client.post(
            f"/users/alice/collections/{cid}/shares",
            json={"username": "bob", "role": "viewer"},
        )
        sid = share_r.json()["id"]
        r = alice_client.delete(f"/users/alice/collections/{cid}/shares/{sid}")
        assert r.status_code == 204

    def test_revoke_share_removes_access(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        share_r = alice_client.post(
            f"/users/alice/collections/{cid}/shares",
            json={"username": "bob", "role": "viewer"},
        )
        sid = share_r.json()["id"]
        alice_client.delete(f"/users/alice/collections/{cid}/shares/{sid}")
        data = alice_client.get(f"/users/alice/collections/{cid}/shares").json()
        assert data["total"] == 0

    def test_revoke_nonexistent_share_returns_404(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        r = alice_client.delete(
            f"/users/alice/collections/{cid}/shares/no-such-share"
        )
        assert r.status_code == 404

    def test_share_nonexistent_collection_returns_404(self, alice_client):
        r = alice_client.post(
            "/users/alice/collections/nonexistent/shares",
            json={"username": "bob", "role": "viewer"},
        )
        assert r.status_code == 404

    def test_share_with_self_rejected(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        r = alice_client.post(
            f"/users/alice/collections/{cid}/shares",
            json={"username": "alice", "role": "viewer"},
        )
        assert r.status_code in (400, 422)

    def test_duplicate_share_rejected_or_idempotent(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.post(
            f"/users/alice/collections/{cid}/shares",
            json={"username": "bob", "role": "viewer"},
        )
        r = alice_client.post(
            f"/users/alice/collections/{cid}/shares",
            json={"username": "bob", "role": "editor"},
        )
        assert r.status_code in (200, 201, 409)

    def test_shared_user_can_access_collection(self, alice_client, bob_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.post(
            f"/users/alice/collections/{cid}/shares",
            json={"username": "bob", "role": "viewer"},
        )
        r = bob_client.get(f"/users/alice/collections/{cid}")
        assert r.status_code == 200


# ===================================================================
# 6. Tree Structure
# ===================================================================


class TestTreeStructure:
    """Get tree, move node, verify hierarchy."""

    def test_get_tree_returns_200(self, alice_client):
        r = alice_client.get("/users/alice/tree")
        assert r.status_code == 200

    def test_get_tree_empty_initially(self, alice_client):
        data = alice_client.get("/users/alice/tree").json()
        assert "roots" in data
        assert data["roots"] == []

    def test_tree_reflects_created_collection(self, alice_client):
        _create_collection(alice_client)
        data = alice_client.get("/users/alice/tree").json()
        assert len(data["roots"]) == 1
        node = data["roots"][0]
        assert node["name"] == "My Favorites"
        assert node["type"] == "collection"

    def test_tree_reflects_nested_collection(self, alice_client):
        parent_body, _ = _create_collection(alice_client, name="Parent")
        pid = parent_body["id"]
        _create_collection(alice_client, name="Child", parent_id=pid)
        data = alice_client.get("/users/alice/tree").json()
        root = data["roots"][0]
        assert root["name"] == "Parent"
        assert len(root["children"]) == 1
        assert root["children"][0]["name"] == "Child"

    def test_move_node_returns_200(self, alice_client):
        c1, _ = _create_collection(alice_client, name="A")
        c2, _ = _create_collection(alice_client, name="B")
        r = alice_client.patch(
            "/users/alice/tree",
            json={"node_id": c2["id"], "parent_id": c1["id"], "position": 0},
        )
        assert r.status_code == 200

    def test_move_node_updates_hierarchy(self, alice_client):
        c1, _ = _create_collection(alice_client, name="A")
        c2, _ = _create_collection(alice_client, name="B")
        alice_client.patch(
            "/users/alice/tree",
            json={"node_id": c2["id"], "parent_id": c1["id"], "position": 0},
        )
        data = alice_client.get("/users/alice/tree").json()
        assert len(data["roots"]) == 1
        assert data["roots"][0]["name"] == "A"
        assert data["roots"][0]["children"][0]["name"] == "B"

    def test_move_node_to_root(self, alice_client):
        c1, _ = _create_collection(alice_client, name="Parent")
        c2, _ = _create_collection(alice_client, name="Child", parent_id=c1["id"])
        alice_client.patch(
            "/users/alice/tree",
            json={"node_id": c2["id"], "parent_id": None, "position": 0},
        )
        data = alice_client.get("/users/alice/tree").json()
        assert len(data["roots"]) == 2

    def test_move_nonexistent_node_returns_404(self, alice_client):
        r = alice_client.patch(
            "/users/alice/tree",
            json={"node_id": "nonexistent", "parent_id": None, "position": 0},
        )
        assert r.status_code == 404

    def test_anon_cannot_get_tree(self, anon_client):
        r = anon_client.get("/users/alice/tree")
        assert r.status_code == 401


# ===================================================================
# 7. Edge Cases
# ===================================================================


class TestEdgeCases:
    """Delete collection with items, delete shared collection, duplicate name."""

    def test_delete_collection_with_items(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.post(
            f"/users/alice/collections/{cid}/items",
            json={"item_ids": ["temp-fesom-2000"]},
        )
        r = alice_client.delete(f"/users/alice/collections/{cid}")
        assert r.status_code == 204

    def test_delete_collection_does_not_delete_stac_items(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.post(
            f"/users/alice/collections/{cid}/items",
            json={"item_ids": ["temp-fesom-2000"]},
        )
        alice_client.delete(f"/users/alice/collections/{cid}")
        r = alice_client.get("/collections/exp-fesom/items/temp-fesom-2000")
        assert r.status_code == 200

    def test_delete_shared_collection_revokes_shares(self, alice_client):
        body, _ = _create_collection(alice_client)
        cid = body["id"]
        alice_client.post(
            f"/users/alice/collections/{cid}/shares",
            json={"username": "bob", "role": "viewer"},
        )
        alice_client.delete(f"/users/alice/collections/{cid}")
        r = alice_client.get(f"/users/alice/collections/{cid}/shares")
        assert r.status_code == 404

    def test_duplicate_collection_name_allowed(self, alice_client):
        body1, s1 = _create_collection(alice_client, name="Duplicates")
        body2, s2 = _create_collection(alice_client, name="Duplicates")
        assert s1 == 201
        assert s2 == 201
        assert body1["id"] != body2["id"]

    def test_collection_name_max_length(self, alice_client):
        r = alice_client.post(
            "/users/alice/collections",
            json={"name": "x" * 257, "description": "too long"},
        )
        assert r.status_code == 422

    def test_label_name_max_length(self, alice_client):
        r = alice_client.post(
            "/users/alice/labels",
            json={"name": "x" * 65, "color": "#aabbcc"},
        )
        assert r.status_code == 422

    def test_deleting_label_used_by_collection(self, alice_client):
        label_body, _ = _create_label(alice_client)
        lid = label_body["id"]
        col_body, _ = _create_collection(alice_client, label_ids=[lid])
        cid = col_body["id"]
        r = alice_client.delete(f"/users/alice/labels/{lid}")
        assert r.status_code == 204
        detail = alice_client.get(f"/users/alice/collections/{cid}").json()
        assert lid not in detail.get("label_ids", [])

    def test_multiple_collections_independent(self, alice_client):
        c1, _ = _create_collection(alice_client, name="C1")
        c2, _ = _create_collection(alice_client, name="C2")
        alice_client.post(
            f"/users/alice/collections/{c1['id']}/items",
            json={"item_ids": ["temp-fesom-2000"]},
        )
        detail2 = alice_client.get(f"/users/alice/collections/{c2['id']}").json()
        assert detail2["item_count"] == 0
        assert detail2["item_ids"] == []
