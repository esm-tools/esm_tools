"""Personal collections storage layer.

Allows authenticated users to curate personal collections of STAC items
from the global catalog.  Collections can be organized in a tree of folders,
tagged with labels, and shared with other users under a role-based access
control model.

Role hierarchy (descending privilege):
    owner > maintainer > developer > viewer

Roles grant the following permissions:
    owner       -- full control (CRUD items, metadata, sharing, delete)
    maintainer  -- CRUD items and update collection metadata
    developer   -- update collection metadata only
    viewer      -- read-only access
"""

from __future__ import annotations

import json
import os
import uuid
from datetime import datetime, timezone
from enum import Enum
from pathlib import Path
from typing import Literal

import duckdb
from loguru import logger
from pydantic import BaseModel, Field


# ------------------------------------------------------------------
# Enums
# ------------------------------------------------------------------


class Role(str, Enum):
    """Access role within a personal collection."""

    owner = "owner"
    maintainer = "maintainer"
    developer = "developer"
    viewer = "viewer"


# Ordered from most to least privileged so that index comparisons work.
_ROLE_HIERARCHY: list[Role] = [Role.owner, Role.maintainer, Role.developer, Role.viewer]


def _role_index(role: Role) -> int:
    """Return the hierarchy index for *role* (lower = more privileged)."""
    return _ROLE_HIERARCHY.index(role)


# ------------------------------------------------------------------
# Pydantic models
# ------------------------------------------------------------------


def _new_id() -> str:
    return str(uuid.uuid4())


def _utcnow() -> datetime:
    return datetime.now(timezone.utc)


class PersonalCollection(BaseModel):
    """A user-curated collection referencing global catalog items."""

    id: str = Field(default_factory=_new_id)
    owner: str
    name: str
    description_md: str = ""
    labels: list[str] = Field(default_factory=list)
    parent_id: str | None = None
    created_at: datetime = Field(default_factory=_utcnow)
    updated_at: datetime = Field(default_factory=_utcnow)


class CollectionShare(BaseModel):
    """A share grant linking a collection to a user with a specific role."""

    id: str = Field(default_factory=_new_id)
    collection_id: str
    username: str
    role: Role


class Label(BaseModel):
    """A user-defined label (tag) with a colour."""

    id: str = Field(default_factory=_new_id)
    owner: str
    name: str
    color: str = "#808080"


class TreeNode(BaseModel):
    """A node in the user's sidebar tree (folder or collection reference)."""

    id: str = Field(default_factory=_new_id)
    owner: str
    name: str
    type: Literal["folder", "collection"]
    collection_id: str | None = None
    parent_id: str | None = None
    position: int = 0


# ------------------------------------------------------------------
# Storage
# ------------------------------------------------------------------


class PersonalCollectionStore:
    """DuckDB-backed store for personal collections and related objects.

    Mirrors the connection / cursor pattern established by
    :class:`~esm_catalog.storage.duckdb.CatalogDB`.

    Parameters
    ----------
    path
        Path to a DuckDB database file.  Created (with schema) on first
        open; subsequent opens reuse the existing tables.
    """

    def __init__(self, path: Path | str) -> None:
        self.path = Path(path)
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self.db = duckdb.connect(str(self.path))
        _duckdb_threads = int(os.environ.get("ESM_CATALOG_DUCKDB_THREADS", "4"))
        self.db.execute(f"SET threads = {_duckdb_threads}")
        self.db.execute("SET TimeZone='UTC'")
        self._init_schema()

    # ------------------------------------------------------------------
    # Schema
    # ------------------------------------------------------------------

    def _init_schema(self) -> None:
        self.db.execute("""
            CREATE TABLE IF NOT EXISTS personal_collections (
                id             TEXT PRIMARY KEY,
                owner          TEXT NOT NULL,
                name           TEXT NOT NULL,
                description_md TEXT DEFAULT '',
                parent_id      TEXT,
                created_at     TIMESTAMPTZ DEFAULT now(),
                updated_at     TIMESTAMPTZ DEFAULT now()
            )
        """)
        self.db.execute("""
            CREATE TABLE IF NOT EXISTS collection_items (
                collection_id TEXT NOT NULL,
                item_id       TEXT NOT NULL,
                added_at      TIMESTAMPTZ DEFAULT now(),
                PRIMARY KEY (collection_id, item_id)
            )
        """)
        self.db.execute("""
            CREATE TABLE IF NOT EXISTS collection_shares (
                id            TEXT PRIMARY KEY,
                collection_id TEXT NOT NULL,
                username      TEXT NOT NULL,
                role          TEXT NOT NULL
            )
        """)
        self.db.execute("""
            CREATE TABLE IF NOT EXISTS labels (
                id    TEXT PRIMARY KEY,
                owner TEXT NOT NULL,
                name  TEXT NOT NULL,
                color TEXT DEFAULT '#808080'
            )
        """)
        self.db.execute("""
            CREATE TABLE IF NOT EXISTS collection_labels (
                collection_id TEXT NOT NULL,
                label_id      TEXT NOT NULL,
                PRIMARY KEY (collection_id, label_id)
            )
        """)
        self.db.execute("""
            CREATE TABLE IF NOT EXISTS tree_nodes (
                id            TEXT PRIMARY KEY,
                owner         TEXT NOT NULL,
                name          TEXT NOT NULL,
                type          TEXT NOT NULL,
                collection_id TEXT,
                parent_id     TEXT,
                position      INTEGER DEFAULT 0
            )
        """)

        # Indexes (DuckDB does not support CREATE INDEX IF NOT EXISTS).
        for stmt in [
            "CREATE INDEX idx_pc_owner ON personal_collections(owner)",
            "CREATE INDEX idx_ci_collection ON collection_items(collection_id)",
            "CREATE INDEX idx_cs_collection ON collection_shares(collection_id)",
            "CREATE INDEX idx_cs_username ON collection_shares(username)",
            "CREATE INDEX idx_labels_owner ON labels(owner)",
            "CREATE INDEX idx_cl_collection ON collection_labels(collection_id)",
            "CREATE INDEX idx_tn_owner ON tree_nodes(owner)",
            "CREATE INDEX idx_tn_parent ON tree_nodes(parent_id)",
        ]:
            try:
                self.db.execute(stmt)
            except duckdb.CatalogException:
                pass  # Already exists

    # ------------------------------------------------------------------
    # RBAC helpers
    # ------------------------------------------------------------------

    def check_access(
        self,
        collection_id: str,
        username: str,
        required_role: Role = Role.viewer,
    ) -> bool:
        """Return ``True`` if *username* holds at least *required_role* on *collection_id*.

        The owner always has ``Role.owner``.  Other users are checked via
        ``collection_shares``.  A user's effective role must be at least as
        privileged as *required_role* (lower index in ``_ROLE_HIERARCHY``).
        """
        cursor = self.db.cursor()
        try:
            # Check ownership first (fast path).
            row = cursor.execute(
                "SELECT owner FROM personal_collections WHERE id = ?",
                [collection_id],
            ).fetchone()
            if row is None:
                return False
            if row[0] == username:
                return True

            # Check explicit share.
            share_row = cursor.execute(
                "SELECT role FROM collection_shares WHERE collection_id = ? AND username = ?",
                [collection_id, username],
            ).fetchone()
            if share_row is None:
                return False
            user_role = Role(share_row[0])
            return _role_index(user_role) <= _role_index(required_role)
        finally:
            cursor.close()

    def _require_access(
        self,
        collection_id: str,
        username: str,
        required_role: Role,
    ) -> None:
        """Raise ``PermissionError`` unless *username* has *required_role*."""
        if not self.check_access(collection_id, username, required_role):
            raise PermissionError(
                f"User {username!r} lacks {required_role.value!r} access on collection {collection_id!r}"
            )

    def _get_effective_role(self, collection_id: str, username: str) -> Role | None:
        """Return the effective role of *username* on *collection_id*, or ``None``."""
        cursor = self.db.cursor()
        try:
            row = cursor.execute(
                "SELECT owner FROM personal_collections WHERE id = ?",
                [collection_id],
            ).fetchone()
            if row is None:
                return None
            if row[0] == username:
                return Role.owner
            share_row = cursor.execute(
                "SELECT role FROM collection_shares WHERE collection_id = ? AND username = ?",
                [collection_id, username],
            ).fetchone()
            if share_row is None:
                return None
            return Role(share_row[0])
        finally:
            cursor.close()

    # ------------------------------------------------------------------
    # Collections CRUD
    # ------------------------------------------------------------------

    def create_collection(
        self,
        owner: str,
        name: str,
        description: str = "",
        parent_id: str | None = None,
    ) -> PersonalCollection:
        """Create a new personal collection owned by *owner*."""
        col = PersonalCollection(
            owner=owner,
            name=name,
            description_md=description,
            parent_id=parent_id,
        )
        self.db.execute(
            """
            INSERT INTO personal_collections (id, owner, name, description_md, parent_id, created_at, updated_at)
            VALUES (?, ?, ?, ?, ?, ?, ?)
            """,
            [col.id, col.owner, col.name, col.description_md, col.parent_id, col.created_at, col.updated_at],
        )
        logger.debug("Created personal collection {!r} for user {!r}", col.id, owner)
        return col

    def get_collection(self, collection_id: str, username: str) -> PersonalCollection:
        """Return a collection if *username* has at least viewer access.

        Raises
        ------
        PermissionError
            If the user lacks viewer access.
        KeyError
            If the collection does not exist.
        """
        cursor = self.db.cursor()
        try:
            row = cursor.execute(
                "SELECT id, owner, name, description_md, parent_id, created_at, updated_at "
                "FROM personal_collections WHERE id = ?",
                [collection_id],
            ).fetchone()
        finally:
            cursor.close()

        if row is None:
            raise KeyError(f"Collection {collection_id!r} not found")

        self._require_access(collection_id, username, Role.viewer)

        col = PersonalCollection(
            id=row[0],
            owner=row[1],
            name=row[2],
            description_md=row[3],
            parent_id=row[4],
            created_at=row[5],
            updated_at=row[6],
        )

        # Attach labels.
        cursor = self.db.cursor()
        try:
            label_rows = cursor.execute(
                "SELECT l.name FROM collection_labels cl "
                "JOIN labels l ON cl.label_id = l.id "
                "WHERE cl.collection_id = ?",
                [collection_id],
            ).fetchall()
        finally:
            cursor.close()
        col.labels = [r[0] for r in label_rows]
        return col

    def list_collections(self, username: str) -> list[PersonalCollection]:
        """Return all collections the user owns or has been shared with."""
        cursor = self.db.cursor()
        try:
            rows = cursor.execute(
                """
                SELECT DISTINCT pc.id, pc.owner, pc.name, pc.description_md,
                       pc.parent_id, pc.created_at, pc.updated_at
                FROM personal_collections pc
                LEFT JOIN collection_shares cs ON pc.id = cs.collection_id
                WHERE pc.owner = ? OR cs.username = ?
                ORDER BY pc.updated_at DESC
                """,
                [username, username],
            ).fetchall()
        finally:
            cursor.close()

        results: list[PersonalCollection] = []
        for r in rows:
            results.append(
                PersonalCollection(
                    id=r[0], owner=r[1], name=r[2], description_md=r[3],
                    parent_id=r[4], created_at=r[5], updated_at=r[6],
                )
            )
        return results

    def update_collection(
        self,
        collection_id: str,
        username: str,
        updates: dict,
    ) -> PersonalCollection:
        """Update mutable fields of a collection.

        Requires at least ``developer`` role.  Only ``name``, ``description_md``,
        and ``parent_id`` may be updated.

        Returns the updated collection.
        """
        self._require_access(collection_id, username, Role.developer)

        allowed = {"name", "description_md", "parent_id"}
        to_set = {k: v for k, v in updates.items() if k in allowed}
        if not to_set:
            return self.get_collection(collection_id, username)

        to_set["updated_at"] = _utcnow()
        set_clause = ", ".join(f"{k} = ?" for k in to_set)
        params = list(to_set.values()) + [collection_id]

        self.db.execute(
            f"UPDATE personal_collections SET {set_clause} WHERE id = ?",
            params,
        )
        logger.debug("Updated collection {!r} fields {}", collection_id, list(to_set.keys()))
        return self.get_collection(collection_id, username)

    def delete_collection(self, collection_id: str, username: str) -> None:
        """Delete a collection and all associated data.

        Only the owner may delete a collection.
        """
        self._require_access(collection_id, username, Role.owner)

        # Cascade: items, shares, labels, tree nodes referencing this collection.
        for table, col in [
            ("collection_items", "collection_id"),
            ("collection_shares", "collection_id"),
            ("collection_labels", "collection_id"),
        ]:
            self.db.execute(f"DELETE FROM {table} WHERE {col} = ?", [collection_id])

        self.db.execute(
            "DELETE FROM tree_nodes WHERE collection_id = ?", [collection_id]
        )
        self.db.execute(
            "DELETE FROM personal_collections WHERE id = ?", [collection_id]
        )
        logger.debug("Deleted collection {!r}", collection_id)

    # ------------------------------------------------------------------
    # Items
    # ------------------------------------------------------------------

    def add_items(
        self,
        collection_id: str,
        username: str,
        item_ids: list[str],
    ) -> None:
        """Add global catalog item references to a collection.

        Requires at least ``maintainer`` role.
        """
        self._require_access(collection_id, username, Role.maintainer)

        now = _utcnow()
        for item_id in item_ids:
            self.db.execute(
                """
                INSERT OR IGNORE INTO collection_items (collection_id, item_id, added_at)
                VALUES (?, ?, ?)
                """,
                [collection_id, item_id, now],
            )

        # Touch updated_at on the collection.
        self.db.execute(
            "UPDATE personal_collections SET updated_at = ? WHERE id = ?",
            [now, collection_id],
        )
        logger.debug("Added {} item(s) to collection {!r}", len(item_ids), collection_id)

    def remove_item(self, collection_id: str, username: str, item_id: str) -> None:
        """Remove an item reference from a collection.

        Requires at least ``maintainer`` role.
        """
        self._require_access(collection_id, username, Role.maintainer)

        self.db.execute(
            "DELETE FROM collection_items WHERE collection_id = ? AND item_id = ?",
            [collection_id, item_id],
        )
        self.db.execute(
            "UPDATE personal_collections SET updated_at = ? WHERE id = ?",
            [_utcnow(), collection_id],
        )
        logger.debug("Removed item {!r} from collection {!r}", item_id, collection_id)

    def get_items(self, collection_id: str, username: str) -> list[str]:
        """Return the list of item IDs in a collection.

        Requires at least ``viewer`` role.
        """
        self._require_access(collection_id, username, Role.viewer)

        cursor = self.db.cursor()
        try:
            rows = cursor.execute(
                "SELECT item_id FROM collection_items WHERE collection_id = ? ORDER BY added_at",
                [collection_id],
            ).fetchall()
        finally:
            cursor.close()
        return [r[0] for r in rows]

    # ------------------------------------------------------------------
    # Sharing
    # ------------------------------------------------------------------

    def share_collection(
        self,
        collection_id: str,
        owner: str,
        target_user: str,
        role: Role,
    ) -> CollectionShare:
        """Grant *target_user* access to a collection with *role*.

        Only the collection owner may share.  Re-sharing to the same user
        updates the existing role.
        """
        self._require_access(collection_id, owner, Role.owner)

        if role == Role.owner:
            raise ValueError("Cannot share with owner role; transfer ownership instead")

        if target_user == owner:
            raise ValueError("Cannot share a collection with yourself")

        # Upsert: update role if a share already exists.
        cursor = self.db.cursor()
        try:
            existing = cursor.execute(
                "SELECT id FROM collection_shares WHERE collection_id = ? AND username = ?",
                [collection_id, target_user],
            ).fetchone()
        finally:
            cursor.close()

        if existing:
            share_id = existing[0]
            self.db.execute(
                "UPDATE collection_shares SET role = ? WHERE id = ?",
                [role.value, share_id],
            )
            logger.debug("Updated share {!r} to role {!r}", share_id, role.value)
            return CollectionShare(id=share_id, collection_id=collection_id, username=target_user, role=role)

        share = CollectionShare(collection_id=collection_id, username=target_user, role=role)
        self.db.execute(
            "INSERT INTO collection_shares (id, collection_id, username, role) VALUES (?, ?, ?, ?)",
            [share.id, share.collection_id, share.username, share.role.value],
        )
        logger.debug("Shared collection {!r} with {!r} as {!r}", collection_id, target_user, role.value)
        return share

    def revoke_share(self, collection_id: str, owner: str, share_id: str) -> None:
        """Revoke a share grant.  Only the collection owner may revoke."""
        self._require_access(collection_id, owner, Role.owner)

        cursor = self.db.cursor()
        try:
            row = cursor.execute(
                "SELECT id FROM collection_shares WHERE id = ? AND collection_id = ?",
                [share_id, collection_id],
            ).fetchone()
        finally:
            cursor.close()

        if row is None:
            raise KeyError(f"Share {share_id!r} not found on collection {collection_id!r}")

        self.db.execute("DELETE FROM collection_shares WHERE id = ?", [share_id])
        logger.debug("Revoked share {!r} on collection {!r}", share_id, collection_id)

    def list_shares(self, collection_id: str, username: str) -> list[CollectionShare]:
        """Return all shares for a collection.

        Requires at least ``viewer`` access (so shared users can see who else
        has access).
        """
        self._require_access(collection_id, username, Role.viewer)

        cursor = self.db.cursor()
        try:
            rows = cursor.execute(
                "SELECT id, collection_id, username, role FROM collection_shares WHERE collection_id = ?",
                [collection_id],
            ).fetchall()
        finally:
            cursor.close()
        return [
            CollectionShare(id=r[0], collection_id=r[1], username=r[2], role=Role(r[3]))
            for r in rows
        ]

    # ------------------------------------------------------------------
    # Labels
    # ------------------------------------------------------------------

    def create_label(self, owner: str, name: str, color: str = "#808080") -> Label:
        """Create a new label for *owner*."""
        label = Label(owner=owner, name=name, color=color)
        self.db.execute(
            "INSERT INTO labels (id, owner, name, color) VALUES (?, ?, ?, ?)",
            [label.id, label.owner, label.name, label.color],
        )
        logger.debug("Created label {!r} for user {!r}", label.id, owner)
        return label

    def list_labels(self, owner: str) -> list[Label]:
        """Return all labels belonging to *owner*."""
        cursor = self.db.cursor()
        try:
            rows = cursor.execute(
                "SELECT id, owner, name, color FROM labels WHERE owner = ? ORDER BY name",
                [owner],
            ).fetchall()
        finally:
            cursor.close()
        return [Label(id=r[0], owner=r[1], name=r[2], color=r[3]) for r in rows]

    def delete_label(self, owner: str, label_id: str) -> bool:
        """Delete a label.  Returns ``True`` if the label existed.

        Also removes all collection-label associations for this label.
        """
        cursor = self.db.cursor()
        try:
            row = cursor.execute(
                "SELECT id FROM labels WHERE id = ? AND owner = ?",
                [label_id, owner],
            ).fetchone()
        finally:
            cursor.close()
        if row is None:
            return False
        self.db.execute(
            "DELETE FROM collection_labels WHERE label_id = ?", [label_id]
        )
        self.db.execute("DELETE FROM labels WHERE id = ?", [label_id])
        logger.debug("Deleted label {!r}", label_id)
        return True

    # ------------------------------------------------------------------
    # Tree
    # ------------------------------------------------------------------

    def get_tree(self, owner: str) -> list[TreeNode]:
        """Return all tree nodes for *owner* as a flat list.

        The caller (route layer) is responsible for assembling the
        parent/child hierarchy from the ``parent_id`` field.
        """
        cursor = self.db.cursor()
        try:
            rows = cursor.execute(
                "SELECT id, owner, name, type, collection_id, parent_id, position "
                "FROM tree_nodes WHERE owner = ? ORDER BY position",
                [owner],
            ).fetchall()
        finally:
            cursor.close()
        return [
            TreeNode(
                id=r[0], owner=r[1], name=r[2], type=r[3],
                collection_id=r[4], parent_id=r[5], position=r[6],
            )
            for r in rows
        ]

    def update_tree_node(
        self,
        owner: str,
        node_id: str,
        parent_id: str | None,
        position: int,
    ) -> None:
        """Move a tree node to a new parent and/or position.

        Raises ``KeyError`` if the node does not exist or does not belong
        to *owner*.
        """
        cursor = self.db.cursor()
        try:
            row = cursor.execute(
                "SELECT id FROM tree_nodes WHERE id = ? AND owner = ?",
                [node_id, owner],
            ).fetchone()
        finally:
            cursor.close()

        if row is None:
            raise KeyError(f"Tree node {node_id!r} not found for user {owner!r}")

        self.db.execute(
            "UPDATE tree_nodes SET parent_id = ?, position = ? WHERE id = ?",
            [parent_id, position, node_id],
        )
        logger.debug("Moved tree node {!r} to parent={!r} position={}", node_id, parent_id, position)

    # ------------------------------------------------------------------
    # Lifecycle
    # ------------------------------------------------------------------

    def close(self) -> None:
        self.db.close()

    def __enter__(self):
        return self

    def __exit__(self, *_):
        self.close()
