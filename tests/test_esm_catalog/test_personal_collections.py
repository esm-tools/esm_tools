"""Tests for personal collections — storage layer + API routes (PR-C2)."""

from __future__ import annotations

from pathlib import Path

import pytest
from fastapi.testclient import TestClient

from esm_catalog.api.app import create_app
from esm_catalog.storage.personal import PersonalCollectionStore, Role


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture()
def store(tmp_path: Path) -> PersonalCollectionStore:
    return PersonalCollectionStore(tmp_path / "personal.duckdb")


@pytest.fixture()
def client(tmp_path: Path) -> TestClient:
    # Point ESM_PERSONAL_DB at a temp file so tests are isolated
    import os
    os.environ["ESM_PERSONAL_DB"] = str(tmp_path / "personal.duckdb")
    yield TestClient(create_app(catalogs=[]).app)
    os.environ.pop("ESM_PERSONAL_DB", None)


# ---------------------------------------------------------------------------
# Storage layer — collections
# ---------------------------------------------------------------------------


def test_create_collection(store: PersonalCollectionStore):
    col = store.create_collection(owner="alice", name="My Research")
    assert col.id
    assert col.owner == "alice"
    assert col.name == "My Research"


def test_list_collections_empty(store: PersonalCollectionStore):
    cols = store.list_collections(username="alice")
    assert cols == []


def test_list_collections(store: PersonalCollectionStore):
    store.create_collection(owner="alice", name="Col A")
    store.create_collection(owner="alice", name="Col B")
    cols = store.list_collections(username="alice")
    assert len(cols) == 2


def test_get_collection(store: PersonalCollectionStore):
    col = store.create_collection(owner="alice", name="Col A")
    fetched = store.get_collection(col.id, username="alice")
    assert fetched is not None
    assert fetched.id == col.id


def test_update_collection(store: PersonalCollectionStore):
    col = store.create_collection(owner="alice", name="Old Name")
    updated = store.update_collection(col.id, username="alice", updates={"name": "New Name"})
    assert updated.name == "New Name"


def test_delete_collection(store: PersonalCollectionStore):
    col = store.create_collection(owner="alice", name="Temp")
    store.delete_collection(col.id, username="alice")
    with pytest.raises(KeyError):
        store.get_collection(col.id, username="alice")


# ---------------------------------------------------------------------------
# Storage layer — items
# ---------------------------------------------------------------------------


def test_add_and_get_items(store: PersonalCollectionStore):
    col = store.create_collection(owner="alice", name="Col A")
    store.add_items(col.id, username="alice", item_ids=["item-1", "item-2"])
    items = store.get_items(col.id, username="alice")
    assert set(items) == {"item-1", "item-2"}


def test_remove_item(store: PersonalCollectionStore):
    col = store.create_collection(owner="alice", name="Col A")
    store.add_items(col.id, username="alice", item_ids=["item-1", "item-2"])
    store.remove_item(col.id, username="alice", item_id="item-1")
    items = store.get_items(col.id, username="alice")
    assert items == ["item-2"]


# ---------------------------------------------------------------------------
# Storage layer — labels
# ---------------------------------------------------------------------------


def test_create_and_list_labels(store: PersonalCollectionStore):
    label = store.create_label(owner="alice", name="important", color="#ff0000")
    assert label.id
    labels = store.list_labels(owner="alice")
    assert len(labels) == 1
    assert labels[0].name == "important"


def test_delete_label(store: PersonalCollectionStore):
    label = store.create_label(owner="alice", name="temp")
    deleted = store.delete_label(owner="alice", label_id=label.id)
    assert deleted
    assert store.list_labels(owner="alice") == []


# ---------------------------------------------------------------------------
# Storage layer — tree
# ---------------------------------------------------------------------------


def test_create_folder_in_tree(store: PersonalCollectionStore):
    store.create_folder(owner="alice", name="Experiments")
    tree = store.get_tree(owner="alice")
    assert len(tree) == 1
    assert tree[0].name == "Experiments"
    assert tree[0].type == "folder"


def test_rename_tree_node(store: PersonalCollectionStore):
    store.create_folder(owner="alice", name="Old Name")
    node = store.get_tree(owner="alice")[0]
    store.rename_tree_node(owner="alice", node_id=node.id, name="New Name")
    updated = store.get_tree(owner="alice")[0]
    assert updated.name == "New Name"


# ---------------------------------------------------------------------------
# API routes — /users/{username}/collections
# ---------------------------------------------------------------------------


def test_api_list_collections_empty(client: TestClient):
    r = client.get("/users/alice/collections")
    assert r.status_code == 200
    data = r.json()
    assert data["collections"] == []
    assert data["total"] == 0


def test_api_create_collection(client: TestClient):
    r = client.post(
        "/users/alice/collections",
        json={"name": "My Research", "description": "Test collection"},
    )
    assert r.status_code == 201
    data = r.json()
    assert data["id"]
    assert data["name"] == "My Research"
    assert data["owner"] == "alice"


def test_api_get_collection(client: TestClient):
    r = client.post("/users/alice/collections", json={"name": "Col A"})
    col_id = r.json()["id"]
    r2 = client.get(f"/users/alice/collections/{col_id}")
    assert r2.status_code == 200
    assert r2.json()["id"] == col_id


def test_api_get_collection_not_found(client: TestClient):
    r = client.get("/users/alice/collections/nonexistent")
    assert r.status_code == 404


def test_api_update_collection(client: TestClient):
    r = client.post("/users/alice/collections", json={"name": "Old"})
    col_id = r.json()["id"]
    r2 = client.patch(f"/users/alice/collections/{col_id}", json={"name": "New"})
    assert r2.status_code == 200
    assert r2.json()["name"] == "New"


def test_api_delete_collection(client: TestClient):
    r = client.post("/users/alice/collections", json={"name": "Temp"})
    col_id = r.json()["id"]
    r2 = client.delete(f"/users/alice/collections/{col_id}")
    assert r2.status_code == 204


def test_api_deleted_collection_not_found(client: TestClient):
    r = client.post("/users/alice/collections", json={"name": "Temp"})
    col_id = r.json()["id"]
    client.delete(f"/users/alice/collections/{col_id}")
    r3 = client.get(f"/users/alice/collections/{col_id}")
    assert r3.status_code == 404


# ---------------------------------------------------------------------------
# API routes — items
# ---------------------------------------------------------------------------


def test_api_add_and_get_items(client: TestClient):
    col_id = client.post("/users/alice/collections", json={"name": "Col"}).json()["id"]
    r = client.post(
        f"/users/alice/collections/{col_id}/items",
        json={"item_ids": ["item-1", "item-2"]},
    )
    assert r.status_code == 201
    data = r.json()
    assert data["added"] == 2


def test_api_remove_item(client: TestClient):
    col_id = client.post("/users/alice/collections", json={"name": "Col"}).json()["id"]
    client.post(
        f"/users/alice/collections/{col_id}/items",
        json={"item_ids": ["item-1"]},
    )
    r = client.delete(f"/users/alice/collections/{col_id}/items/item-1")
    assert r.status_code == 204


# ---------------------------------------------------------------------------
# API routes — labels
# ---------------------------------------------------------------------------


def test_api_create_and_list_labels(client: TestClient):
    r = client.post("/users/alice/labels", json={"name": "important", "color": "#ff0000"})
    assert r.status_code == 201
    labels = client.get("/users/alice/labels").json()["labels"]
    assert len(labels) == 1
    assert labels[0]["name"] == "important"


def test_api_delete_label(client: TestClient):
    label_id = client.post("/users/alice/labels", json={"name": "temp"}).json()["id"]
    r = client.delete(f"/users/alice/labels/{label_id}")
    assert r.status_code == 204


# ---------------------------------------------------------------------------
# API routes — tree
# ---------------------------------------------------------------------------


def test_api_create_folder(client: TestClient):
    r = client.patch(
        "/users/alice/tree",
        json={"action": "create_folder", "name": "Experiments"},
    )
    assert r.status_code == 200
    roots = r.json()["roots"]
    assert len(roots) == 1
    assert roots[0]["name"] == "Experiments"


def test_api_get_tree_empty(client: TestClient):
    r = client.get("/users/alice/tree")
    assert r.status_code == 200
    assert r.json()["roots"] == []
