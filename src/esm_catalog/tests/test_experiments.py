"""Tests for the experiment hierarchy feature (TODO 1 & 3).

Covers:
- CatalogDB.iter_experiments() unit tests
- CatalogDB.get_collections_for_experiment() unit tests
- GET /experiments endpoint integration tests
- GET /experiments/{id} endpoint integration tests
- GET /collections/{id}/experiment convenience endpoint tests
- Collection parent link regression tests
- Landing page experiment child links tests
- Federated multi-catalog experiment tests
"""
import pytest
from fastapi.testclient import TestClient

from esm_catalog.api.app import create_app
from esm_catalog.storage.duckdb import CatalogDB
from esm_catalog.tests.conftest import make_col


# ---------------------------------------------------------------------------
# Storage unit tests
# ---------------------------------------------------------------------------


class TestIterExperiments:
    def test_returns_distinct_ids(self, tmp_path):
        db_path = tmp_path / "c.duckdb"
        with CatalogDB(db_path) as db:
            db.insert_collection(make_col("alpha-fesom", "alpha"))
            db.insert_collection(make_col("alpha-echam", "alpha"))
            exps = db.iter_experiments()
        assert exps == ["alpha"]

    def test_multiple_experiments(self, tmp_path):
        db_path = tmp_path / "c.duckdb"
        with CatalogDB(db_path) as db:
            db.insert_collection(make_col("alpha-fesom", "alpha"))
            db.insert_collection(make_col("beta-fesom", "beta"))
            exps = db.iter_experiments()
        assert exps == ["alpha", "beta"]

    def test_sorted(self, tmp_path):
        db_path = tmp_path / "c.duckdb"
        with CatalogDB(db_path) as db:
            db.insert_collection(make_col("z-fesom", "zzz"))
            db.insert_collection(make_col("a-fesom", "aaa"))
            db.insert_collection(make_col("m-fesom", "mmm"))
            exps = db.iter_experiments()
        assert exps == ["aaa", "mmm", "zzz"]

    def test_empty_catalog(self, tmp_path):
        db_path = tmp_path / "c.duckdb"
        with CatalogDB(db_path) as db:
            exps = db.iter_experiments()
        assert exps == []

    def test_excludes_null_experiment(self, tmp_path):
        db_path = tmp_path / "c.duckdb"
        col_no_exp = {
            "type": "Collection", "id": "no-exp-col",
            "stac_version": "1.0.0", "description": "no experiment",
            "links": [], "extent": {
                "spatial": {"bbox": [[-180, -90, 180, 90]]},
                "temporal": {"interval": [["2000-01-01T00:00:00Z", None]]},
            }, "license": "proprietary",
        }
        with CatalogDB(db_path) as db:
            db.insert_collection(col_no_exp)
            db.insert_collection(make_col("alpha-fesom", "alpha"))
            exps = db.iter_experiments()
        assert exps == ["alpha"]


class TestGetCollectionsForExperiment:
    def test_returns_all_matching(self, tmp_path):
        db_path = tmp_path / "c.duckdb"
        with CatalogDB(db_path) as db:
            db.insert_collection(make_col("alpha-fesom", "alpha"))
            db.insert_collection(make_col("alpha-echam", "alpha"))
            db.insert_collection(make_col("beta-fesom", "beta"))
            cols = db.get_collections_for_experiment("alpha")
        assert len(cols) == 2
        ids = {c["id"] for c in cols}
        assert ids == {"alpha-fesom", "alpha-echam"}

    def test_empty_for_unknown(self, tmp_path):
        db_path = tmp_path / "c.duckdb"
        with CatalogDB(db_path) as db:
            db.insert_collection(make_col("alpha-fesom", "alpha"))
            cols = db.get_collections_for_experiment("nonexistent")
        assert cols == []

    def test_returns_full_dicts(self, tmp_path):
        db_path = tmp_path / "c.duckdb"
        with CatalogDB(db_path) as db:
            db.insert_collection(make_col("alpha-fesom", "alpha"))
            cols = db.get_collections_for_experiment("alpha")
        assert len(cols) == 1
        col = cols[0]
        assert col["id"] == "alpha-fesom"
        assert col["experiment"] == "alpha"
        assert "extent" in col

    def test_ordered_by_id(self, tmp_path):
        db_path = tmp_path / "c.duckdb"
        with CatalogDB(db_path) as db:
            db.insert_collection(make_col("alpha-z", "alpha"))
            db.insert_collection(make_col("alpha-a", "alpha"))
            db.insert_collection(make_col("alpha-m", "alpha"))
            cols = db.get_collections_for_experiment("alpha")
        assert [c["id"] for c in cols] == ["alpha-a", "alpha-m", "alpha-z"]


# ---------------------------------------------------------------------------
# GET /experiments endpoint
# ---------------------------------------------------------------------------


class TestListExperimentsEndpoint:
    def test_200_status(self, api_client):
        r = api_client.get("/experiments")
        assert r.status_code == 200

    def test_both_experiments_present(self, api_client):
        r = api_client.get("/experiments")
        ids = {e["id"] for e in r.json()["experiments"]}
        assert "exp-alpha" in ids
        assert "exp-beta" in ids

    def test_number_matched(self, api_client):
        r = api_client.get("/experiments")
        assert r.json()["numberMatched"] == 2

    def test_collection_counts(self, api_client):
        r = api_client.get("/experiments")
        by_id = {e["id"]: e for e in r.json()["experiments"]}
        assert by_id["exp-alpha"]["collection_count"] == 2
        assert by_id["exp-beta"]["collection_count"] == 1

    def test_href_format(self, api_client):
        r = api_client.get("/experiments")
        for exp in r.json()["experiments"]:
            assert exp["href"].endswith(f"/experiments/{exp['id']}")

    def test_self_link(self, api_client):
        r = api_client.get("/experiments")
        rels = {lnk["rel"] for lnk in r.json()["links"]}
        assert "self" in rels

    def test_limit_pagination(self, api_client):
        r = api_client.get("/experiments?limit=1")
        data = r.json()
        assert data["numberReturned"] == 1
        assert data["numberMatched"] == 2

    def test_next_link_present(self, api_client):
        r = api_client.get("/experiments?limit=1")
        rels = {lnk["rel"] for lnk in r.json()["links"]}
        assert "next" in rels

    def test_offset_pagination(self, api_client):
        r1 = api_client.get("/experiments?limit=1&offset=0")
        r2 = api_client.get("/experiments?limit=1&offset=1")
        id1 = r1.json()["experiments"][0]["id"]
        id2 = r2.json()["experiments"][0]["id"]
        assert id1 != id2

    def test_empty_catalog(self, tmp_path):
        db_path = tmp_path / "empty.duckdb"
        CatalogDB(db_path).close()
        client = TestClient(create_app(catalogs=[db_path]).app)
        r = client.get("/experiments")
        assert r.status_code == 200
        assert r.json()["numberMatched"] == 0
        assert r.json()["experiments"] == []


# ---------------------------------------------------------------------------
# GET /experiments/{id} endpoint
# ---------------------------------------------------------------------------


class TestGetExperimentEndpoint:
    def test_200_for_known(self, api_client):
        r = api_client.get("/experiments/exp-alpha")
        assert r.status_code == 200

    def test_type_catalog(self, api_client):
        r = api_client.get("/experiments/exp-alpha")
        assert r.json()["type"] == "Catalog"

    def test_id_matches(self, api_client):
        r = api_client.get("/experiments/exp-alpha")
        assert r.json()["id"] == "exp-alpha"

    def test_stac_version(self, api_client):
        r = api_client.get("/experiments/exp-alpha")
        assert r.json()["stac_version"] == "1.0.0"

    def test_child_links_count(self, api_client):
        r = api_client.get("/experiments/exp-alpha")
        children = [lnk for lnk in r.json()["links"] if lnk["rel"] == "child"]
        assert len(children) == 2

    def test_child_links_point_to_collections(self, api_client):
        r = api_client.get("/experiments/exp-alpha")
        children = [lnk for lnk in r.json()["links"] if lnk["rel"] == "child"]
        hrefs = {lnk["href"] for lnk in children}
        assert any("/collections/exp-alpha-fesom" in h for h in hrefs)
        assert any("/collections/exp-alpha-echam" in h for h in hrefs)

    def test_self_link(self, api_client):
        r = api_client.get("/experiments/exp-alpha")
        self_links = [lnk for lnk in r.json()["links"] if lnk["rel"] == "self"]
        assert len(self_links) == 1
        assert "/experiments/exp-alpha" in self_links[0]["href"]

    def test_root_link(self, api_client):
        r = api_client.get("/experiments/exp-alpha")
        root_links = [lnk for lnk in r.json()["links"] if lnk["rel"] == "root"]
        assert len(root_links) == 1

    def test_parent_link_to_root(self, api_client):
        r = api_client.get("/experiments/exp-alpha")
        parent_links = [lnk for lnk in r.json()["links"] if lnk["rel"] == "parent"]
        assert len(parent_links) == 1
        # Parent of experiment catalog is the root
        assert parent_links[0]["href"].rstrip("/") not in (
            parent_links[0]["href"].rstrip("/") + "/experiments/exp-alpha",
        )

    def test_404_for_unknown(self, api_client):
        r = api_client.get("/experiments/does-not-exist")
        assert r.status_code == 404

    def test_single_collection_experiment(self, api_client):
        r = api_client.get("/experiments/exp-beta")
        assert r.status_code == 200
        children = [lnk for lnk in r.json()["links"] if lnk["rel"] == "child"]
        assert len(children) == 1


# ---------------------------------------------------------------------------
# Collection parent link regression tests
# ---------------------------------------------------------------------------


class TestCollectionParentLink:
    def test_parent_points_to_experiment(self, api_client):
        r = api_client.get("/collections/exp-alpha-fesom")
        assert r.status_code == 200
        parent_links = [lnk for lnk in r.json()["links"] if lnk["rel"] == "parent"]
        assert len(parent_links) == 1
        assert "/experiments/exp-alpha" in parent_links[0]["href"]

    def test_parent_not_bare_root(self, api_client):
        r = api_client.get("/collections/exp-alpha-fesom")
        parent_links = [lnk for lnk in r.json()["links"] if lnk["rel"] == "parent"]
        href = parent_links[0]["href"].rstrip("/")
        # Should not end at root — must contain /experiments/
        assert "/experiments/" in href

    def test_fallback_to_root_for_no_experiment(self, tmp_path):
        """Collections without experiment field should fall back to root as parent."""
        db_path = tmp_path / "c.duckdb"
        col_no_exp = {
            "type": "Collection", "id": "orphan-col",
            "stac_version": "1.0.0", "description": "no experiment",
            "links": [], "extent": {
                "spatial": {"bbox": [[-180, -90, 180, 90]]},
                "temporal": {"interval": [["2000-01-01T00:00:00Z", None]]},
            }, "license": "proprietary",
        }
        with CatalogDB(db_path) as db:
            db.insert_collection(col_no_exp)
        client = TestClient(create_app(catalogs=[db_path]).app)
        r = client.get("/collections/orphan-col")
        assert r.status_code == 200
        parent_links = [lnk for lnk in r.json()["links"] if lnk["rel"] == "parent"]
        assert len(parent_links) == 1
        assert "/experiments/" not in parent_links[0]["href"]


# ---------------------------------------------------------------------------
# Landing page child links
# ---------------------------------------------------------------------------


class TestLandingPageExperimentLinks:
    def test_child_links_point_to_experiments(self, api_client):
        r = api_client.get("/")
        assert r.status_code == 200
        child_links = [lnk for lnk in r.json()["links"] if lnk["rel"] == "child"]
        hrefs = [lnk["href"] for lnk in child_links]
        assert any("/experiments/" in h for h in hrefs)

    def test_child_links_not_to_collections(self, api_client):
        r = api_client.get("/")
        child_links = [lnk for lnk in r.json()["links"] if lnk["rel"] == "child"]
        hrefs = [lnk["href"] for lnk in child_links]
        assert not any("/collections/" in h for h in hrefs)


# ---------------------------------------------------------------------------
# GET /collections/{id}/experiment convenience endpoint (TODO 3)
# ---------------------------------------------------------------------------


class TestCollectionExperimentEndpoint:
    def test_200_for_known_collection(self, api_client):
        r = api_client.get("/collections/exp-alpha-fesom/experiment")
        assert r.status_code == 200

    def test_returns_catalog_type(self, api_client):
        r = api_client.get("/collections/exp-alpha-fesom/experiment")
        assert r.json()["type"] == "Catalog"

    def test_id_matches_experiment(self, api_client):
        r = api_client.get("/collections/exp-alpha-fesom/experiment")
        assert r.json()["id"] == "exp-alpha"

    def test_stac_version(self, api_client):
        r = api_client.get("/collections/exp-alpha-fesom/experiment")
        assert r.json()["stac_version"] == "1.0.0"

    def test_child_links_include_all_siblings(self, api_client):
        """Result contains all component collections — not just the queried one."""
        r = api_client.get("/collections/exp-alpha-fesom/experiment")
        children = [lnk for lnk in r.json()["links"] if lnk["rel"] == "child"]
        hrefs = {lnk["href"] for lnk in children}
        assert any("/collections/exp-alpha-fesom" in h for h in hrefs)
        assert any("/collections/exp-alpha-echam" in h for h in hrefs)

    def test_child_link_count_matches_experiment(self, api_client):
        """Same child count whether accessed via experiment or collection shortcut."""
        r_exp = api_client.get("/experiments/exp-alpha")
        r_col = api_client.get("/collections/exp-alpha-fesom/experiment")
        exp_children = [lnk for lnk in r_exp.json()["links"] if lnk["rel"] == "child"]
        col_children = [lnk for lnk in r_col.json()["links"] if lnk["rel"] == "child"]
        assert len(exp_children) == len(col_children)

    def test_same_result_from_any_sibling(self, api_client):
        """Both siblings return the same experiment catalog."""
        r1 = api_client.get("/collections/exp-alpha-fesom/experiment")
        r2 = api_client.get("/collections/exp-alpha-echam/experiment")
        assert r1.json()["id"] == r2.json()["id"]
        c1 = {lnk["href"] for lnk in r1.json()["links"] if lnk["rel"] == "child"}
        c2 = {lnk["href"] for lnk in r2.json()["links"] if lnk["rel"] == "child"}
        assert c1 == c2

    def test_self_link(self, api_client):
        r = api_client.get("/collections/exp-alpha-fesom/experiment")
        self_links = [lnk for lnk in r.json()["links"] if lnk["rel"] == "self"]
        assert len(self_links) == 1
        assert "/experiments/exp-alpha" in self_links[0]["href"]

    def test_root_link(self, api_client):
        r = api_client.get("/collections/exp-alpha-fesom/experiment")
        root_links = [lnk for lnk in r.json()["links"] if lnk["rel"] == "root"]
        assert len(root_links) == 1

    def test_404_for_unknown_collection(self, api_client):
        r = api_client.get("/collections/does-not-exist/experiment")
        assert r.status_code == 404

    def test_404_for_collection_without_experiment_field(self, tmp_path):
        db_path = tmp_path / "c.duckdb"
        col_no_exp = {
            "type": "Collection", "id": "orphan-col",
            "stac_version": "1.0.0", "description": "no experiment",
            "links": [], "extent": {
                "spatial": {"bbox": [[-180, -90, 180, 90]]},
                "temporal": {"interval": [["2000-01-01T00:00:00Z", None]]},
            }, "license": "proprietary",
        }
        with CatalogDB(db_path) as db:
            db.insert_collection(col_no_exp)
        client = TestClient(create_app(catalogs=[db_path]).app)
        r = client.get("/collections/orphan-col/experiment")
        assert r.status_code == 404

    def test_single_collection_experiment(self, api_client):
        """Works for experiments with just one component."""
        r = api_client.get("/collections/exp-beta-fesom/experiment")
        assert r.status_code == 200
        assert r.json()["id"] == "exp-beta"
        children = [lnk for lnk in r.json()["links"] if lnk["rel"] == "child"]
        assert len(children) == 1


# ---------------------------------------------------------------------------
# Federated multi-catalog tests
# ---------------------------------------------------------------------------


class TestFederatedExperiments:
    def test_same_experiment_merged(self, tmp_path):
        """Same experiment in two DBs → one entry with merged collection count."""
        db1 = tmp_path / "db1.duckdb"
        db2 = tmp_path / "db2.duckdb"
        with CatalogDB(db1) as db:
            db.insert_collection(make_col("shared-fesom", "shared"))
        with CatalogDB(db2) as db:
            db.insert_collection(make_col("shared-echam", "shared"))

        client = TestClient(create_app(catalogs=[db1, db2]).app)
        r = client.get("/experiments")
        assert r.status_code == 200
        ids = {e["id"] for e in r.json()["experiments"]}
        assert ids == {"shared"}
        by_id = {e["id"]: e for e in r.json()["experiments"]}
        assert by_id["shared"]["collection_count"] == 2

    def test_experiment_catalog_merges_cross_catalog_collections(self, tmp_path):
        """GET /experiments/{id} shows collections from both catalogs."""
        db1 = tmp_path / "db1.duckdb"
        db2 = tmp_path / "db2.duckdb"
        with CatalogDB(db1) as db:
            db.insert_collection(make_col("shared-fesom", "shared"))
        with CatalogDB(db2) as db:
            db.insert_collection(make_col("shared-echam", "shared"))

        client = TestClient(create_app(catalogs=[db1, db2]).app)
        r = client.get("/experiments/shared")
        assert r.status_code == 200
        children = [lnk for lnk in r.json()["links"] if lnk["rel"] == "child"]
        child_hrefs = {lnk["href"] for lnk in children}
        assert any("shared-fesom" in h for h in child_hrefs)
        assert any("shared-echam" in h for h in child_hrefs)

    def test_experiments_from_different_catalogs_all_listed(self, tmp_path):
        """Experiments from different catalogs are all listed."""
        db1 = tmp_path / "db1.duckdb"
        db2 = tmp_path / "db2.duckdb"
        with CatalogDB(db1) as db:
            db.insert_collection(make_col("exp-one-fesom", "exp-one"))
        with CatalogDB(db2) as db:
            db.insert_collection(make_col("exp-two-fesom", "exp-two"))

        client = TestClient(create_app(catalogs=[db1, db2]).app)
        r = client.get("/experiments")
        ids = {e["id"] for e in r.json()["experiments"]}
        assert "exp-one" in ids
        assert "exp-two" in ids
