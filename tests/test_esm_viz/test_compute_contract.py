"""
Contract tests for the Dask compute cluster management API.

Endpoint contract:
    POST   /compute/clusters          -> 201, ClusterResponse {id, name, type, ...}
    GET    /compute/clusters          -> 200, {clusters: [...], total: int}
    GET    /compute/clusters/{id}     -> 200, ClusterResponse
    PATCH  /compute/clusters/{id}     -> 200, ClusterResponse (scaled)
    DELETE /compute/clusters/{id}     -> 204
    POST   with missing fields        -> 422
    GET    nonexistent                -> 404
    DELETE nonexistent                -> 404
"""

from __future__ import annotations

import pytest
from fastapi.testclient import TestClient

from esm_viz.compute import ComputeManager
from esm_viz.compute_routes import create_compute_router


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture()
def manager():
    """Fresh ComputeManager per test -- no shared state."""
    return ComputeManager()


@pytest.fixture()
def client(manager):
    """TestClient wired to a minimal FastAPI app with the compute router."""
    from fastapi import FastAPI

    app = FastAPI()
    app.include_router(create_compute_router(manager=manager))
    return TestClient(app)


# ---------------------------------------------------------------------------
# Happy-path CRUD
# ---------------------------------------------------------------------------


class TestCreateCluster:
    def test_create_local_returns_201(self, client):
        r = client.post(
            "/compute/clusters",
            json={"type": "local", "workers": 1, "name": "test-local"},
        )
        assert r.status_code == 201
        data = r.json()
        assert "id" in data
        assert data["name"] == "test-local"
        assert data["type"] == "local"
        assert data["status"] == "running"
        assert "created_at" in data

    def test_create_local_default_workers(self, client):
        r = client.post("/compute/clusters", json={"type": "local"})
        assert r.status_code == 201
        data = r.json()
        assert data["n_workers"] >= 1

    def test_response_has_scheduler_address(self, client):
        r = client.post(
            "/compute/clusters",
            json={"type": "local", "workers": 1},
        )
        data = r.json()
        assert data["scheduler_address"] is not None
        assert data["scheduler_address"].startswith("tcp://")


class TestListClusters:
    def test_empty_list(self, client):
        r = client.get("/compute/clusters")
        assert r.status_code == 200
        data = r.json()
        assert data["clusters"] == []
        assert data["total"] == 0

    def test_list_after_create(self, client):
        client.post("/compute/clusters", json={"type": "local", "workers": 1})
        r = client.get("/compute/clusters")
        assert r.status_code == 200
        data = r.json()
        assert data["total"] == 1
        assert len(data["clusters"]) == 1

    def test_list_multiple(self, client):
        client.post(
            "/compute/clusters",
            json={"type": "local", "workers": 1, "name": "a"},
        )
        client.post(
            "/compute/clusters",
            json={"type": "local", "workers": 1, "name": "b"},
        )
        r = client.get("/compute/clusters")
        data = r.json()
        assert data["total"] == 2


class TestGetCluster:
    def test_get_existing(self, client):
        create_r = client.post(
            "/compute/clusters", json={"type": "local", "workers": 1}
        )
        cluster_id = create_r.json()["id"]

        r = client.get(f"/compute/clusters/{cluster_id}")
        assert r.status_code == 200
        assert r.json()["id"] == cluster_id

    def test_get_nonexistent_returns_404(self, client):
        r = client.get("/compute/clusters/does-not-exist")
        assert r.status_code == 404


class TestScaleCluster:
    def test_scale_fixed(self, client):
        create_r = client.post(
            "/compute/clusters", json={"type": "local", "workers": 1}
        )
        cluster_id = create_r.json()["id"]

        r = client.patch(
            f"/compute/clusters/{cluster_id}",
            json={"workers": 4},
        )
        assert r.status_code == 200
        data = r.json()
        assert data["id"] == cluster_id
        # The cluster should still be running after scaling
        assert data["status"] == "running"

    def test_scale_nonexistent_returns_404(self, client):
        r = client.patch(
            "/compute/clusters/nope",
            json={"workers": 4},
        )
        assert r.status_code == 404


class TestDeleteCluster:
    def test_delete_existing(self, client):
        create_r = client.post(
            "/compute/clusters", json={"type": "local", "workers": 1}
        )
        cluster_id = create_r.json()["id"]

        r = client.delete(f"/compute/clusters/{cluster_id}")
        assert r.status_code == 204

        # Should be gone now
        r = client.get(f"/compute/clusters/{cluster_id}")
        assert r.status_code == 404

    def test_delete_nonexistent_returns_404(self, client):
        r = client.delete("/compute/clusters/nonexistent")
        assert r.status_code == 404

    def test_list_empty_after_delete(self, client):
        create_r = client.post(
            "/compute/clusters", json={"type": "local", "workers": 1}
        )
        cluster_id = create_r.json()["id"]
        client.delete(f"/compute/clusters/{cluster_id}")

        r = client.get("/compute/clusters")
        assert r.json()["total"] == 0


# ---------------------------------------------------------------------------
# Validation / error cases
# ---------------------------------------------------------------------------


class TestValidation:
    def test_invalid_type_returns_422(self, client):
        r = client.post(
            "/compute/clusters",
            json={"type": "imaginary", "workers": 1},
        )
        assert r.status_code == 422

    def test_workers_below_minimum_returns_422(self, client):
        r = client.post(
            "/compute/clusters",
            json={"type": "local", "workers": 0},
        )
        assert r.status_code == 422

    def test_gateway_without_package_returns_400(self, client):
        """Creating a gateway cluster when dask_gateway is not installed."""
        # This may pass or return 400 depending on environment.
        # The contract is: if the package is missing, 400 not 500.
        r = client.post(
            "/compute/clusters",
            json={"type": "gateway", "workers": 1, "gateway_url": "http://fake"},
        )
        # Either 201 (package installed) or 400 (not installed) -- never 500
        assert r.status_code in (201, 400)

    def test_slurm_without_package_returns_400(self, client):
        """Creating a SLURM cluster when dask_jobqueue is not installed."""
        r = client.post(
            "/compute/clusters",
            json={"type": "slurm", "workers": 1, "queue": "compute"},
        )
        assert r.status_code in (201, 400)


# ---------------------------------------------------------------------------
# Full lifecycle
# ---------------------------------------------------------------------------


class TestLifecycle:
    def test_create_get_scale_delete(self, client):
        """End-to-end lifecycle: create -> get -> scale -> delete."""
        # Create
        r = client.post(
            "/compute/clusters",
            json={"type": "local", "workers": 1, "name": "lifecycle-test"},
        )
        assert r.status_code == 201
        cluster_id = r.json()["id"]

        # Get
        r = client.get(f"/compute/clusters/{cluster_id}")
        assert r.status_code == 200
        assert r.json()["name"] == "lifecycle-test"

        # Scale
        r = client.patch(
            f"/compute/clusters/{cluster_id}",
            json={"workers": 2},
        )
        assert r.status_code == 200

        # Delete
        r = client.delete(f"/compute/clusters/{cluster_id}")
        assert r.status_code == 204

        # Verify gone
        r = client.get(f"/compute/clusters/{cluster_id}")
        assert r.status_code == 404
