"""Tests for stac/item.py, stac/collection.py, and stac/extensions/."""

import pytest

from esm_catalog.stac.collection import make_collection, update_collection_extent
from esm_catalog.stac.extensions.contacts import add_contacts_extension
from esm_catalog.stac.extensions.datacube import add_datacube_extension
from esm_catalog.stac.extensions.hpc import add_hpc_extension
from esm_catalog.stac.extensions.registry import EXTENSION_URLS
from esm_catalog.stac.item import make_item


# ---------------------------------------------------------------------------
# stac/extensions/registry.py
# ---------------------------------------------------------------------------

class TestExtensionRegistry:
    def test_all_required_extensions_present(self):
        for key in ("datacube", "cf", "file", "contacts", "scientific", "hpc"):
            assert key in EXTENSION_URLS

    def test_all_urls_are_strings(self):
        for key, url in EXTENSION_URLS.items():
            assert isinstance(url, str), f"{key} URL is not a string"
            assert url.startswith("https://"), f"{key} URL missing https scheme"

    def test_datacube_url_references_schema(self):
        assert "datacube" in EXTENSION_URLS["datacube"]
        assert "schema.json" in EXTENSION_URLS["datacube"]


# ---------------------------------------------------------------------------
# stac/extensions/datacube.py
# ---------------------------------------------------------------------------

class TestAddDatacubeExtension:
    def test_adds_cube_dimensions(self, sample_item, sample_metadata):
        item = add_datacube_extension(sample_item.copy(), sample_metadata)
        assert "cube:dimensions" in item["properties"]
        dims = item["properties"]["cube:dimensions"]
        assert "time" in dims
        assert dims["time"]["type"] == "temporal"

    def test_adds_cube_variables(self, sample_item, sample_metadata):
        item = add_datacube_extension(sample_item.copy(), sample_metadata)
        assert "cube:variables" in item["properties"]
        assert "ssh" in item["properties"]["cube:variables"]

    def test_appends_extension_url(self, sample_item, sample_metadata):
        item = add_datacube_extension(sample_item.copy(), sample_metadata)
        assert EXTENSION_URLS["datacube"] in item["stac_extensions"]

    def test_no_duplicate_extension_url(self, sample_item, sample_metadata):
        item = add_datacube_extension(sample_item.copy(), sample_metadata)
        item = add_datacube_extension(item, sample_metadata)  # called twice
        urls = item["stac_extensions"]
        assert urls.count(EXTENSION_URLS["datacube"]) == 1

    def test_empty_metadata_does_not_crash(self):
        # Build a bare item (not via make_item) so no extensions are pre-added
        bare_item = {
            "type": "Feature",
            "stac_version": "1.0.0",
            "stac_extensions": [],
            "id": "bare-item",
            "geometry": None,
            "bbox": None,
            "properties": {},
            "assets": {},
            "links": [],
        }
        item = add_datacube_extension(bare_item, {})
        # Empty metadata → no extension URL added
        assert EXTENSION_URLS["datacube"] not in item["stac_extensions"]


# ---------------------------------------------------------------------------
# stac/extensions/contacts.py
# ---------------------------------------------------------------------------

class TestAddContactsExtension:
    def test_adds_contact_from_config(self, sample_item, esm_config):
        item = add_contacts_extension(sample_item.copy(), esm_config)
        contacts = item["properties"].get("contacts", [])
        assert len(contacts) == 1
        assert contacts[0]["name"] == "Paul Gierz"

    def test_adds_orcid_identifier(self, sample_item, esm_config):
        item = add_contacts_extension(sample_item.copy(), esm_config)
        contact = item["properties"]["contacts"][0]
        assert contact["identifier"]["scheme"] == "orcid"
        assert contact["identifier"]["identifier"] == "0000-0003-3163-4650"

    def test_appends_extension_url(self, sample_item, esm_config):
        item = add_contacts_extension(sample_item.copy(), esm_config)
        assert EXTENSION_URLS["contacts"] in item["stac_extensions"]

    def test_no_config_leaves_item_unchanged(self, sample_item):
        before = sample_item["stac_extensions"][:]
        item = add_contacts_extension(sample_item.copy(), None)
        assert "contacts" not in item["properties"]
        assert item["stac_extensions"] == before

    def test_config_without_pi_name_leaves_item_unchanged(self, sample_item):
        item = add_contacts_extension(sample_item.copy(), {"general": {}})
        assert "contacts" not in item["properties"]


# ---------------------------------------------------------------------------
# stac/extensions/hpc.py
# ---------------------------------------------------------------------------

class TestAddHpcExtension:
    def test_adds_storage_tier_property(self, sample_item, fesom_nc):
        item = add_hpc_extension(sample_item.copy(), fesom_nc)
        assert "hpc:storage_tier" in item["properties"]

    def test_adds_hpc_state_to_asset(self, sample_item, fesom_nc):
        item = add_hpc_extension(sample_item.copy(), fesom_nc)
        assert "hpc:state" in item["assets"]["data"]
        assert item["assets"]["data"]["hpc:state"] in (
            "online", "nearline", "offline", "migrating", "unknown"
        )

    def test_adds_storage_type_to_asset(self, sample_item, fesom_nc):
        item = add_hpc_extension(sample_item.copy(), fesom_nc)
        assert "hpc:storage_type" in item["assets"]["data"]

    def test_adds_last_access(self, sample_item, fesom_nc):
        item = add_hpc_extension(sample_item.copy(), fesom_nc)
        assert "hpc:last_access" in item["properties"]

    def test_appends_hpc_extension_url(self, sample_item, fesom_nc):
        item = add_hpc_extension(sample_item.copy(), fesom_nc)
        assert EXTENSION_URLS["hpc"] in item["stac_extensions"]

    def test_albedo_path_detected_as_hot(self, sample_item):
        from pathlib import Path
        from unittest.mock import patch

        with patch(
            "esm_catalog.hpc.detect.detect_hpc_storage",
            return_value={
                "hpc:facility": "AWI",
                "hpc:system": "albedo",
                "hpc:storage_type": "lustre",
                "hpc:state": "online",
            },
        ):
            item = add_hpc_extension(sample_item.copy(), Path("/albedo/work/file.nc"))
        assert item["properties"]["hpc:storage_tier"] == "hot"
        assert item["properties"]["hpc:facility"] == "AWI"


# ---------------------------------------------------------------------------
# stac/collection.py
# ---------------------------------------------------------------------------

class TestMakeCollection:
    def test_returns_stac_collection(self, ctx):
        col = make_collection(ctx)
        assert col["type"] == "Collection"
        assert col["stac_version"] == "1.0.0"

    def test_id_matches_context(self, ctx):
        col = make_collection(ctx)
        assert col["id"] == "basic-001-fesom"

    def test_has_spatial_extent(self, ctx):
        col = make_collection(ctx)
        assert "spatial" in col["extent"]
        assert "bbox" in col["extent"]["spatial"]

    def test_has_temporal_extent(self, ctx):
        col = make_collection(ctx)
        assert "temporal" in col["extent"]
        assert "interval" in col["extent"]["temporal"]

    def test_experiment_and_model_fields(self, ctx):
        col = make_collection(ctx)
        assert col["experiment"] == "basic-001"
        assert col["model"] == "fesom"

    def test_parent_link_present(self, ctx):
        col = make_collection(ctx)
        rels = [lnk["rel"] for lnk in col["links"]]
        assert "parent" in rels


class TestUpdateCollectionExtent:
    def _make_item(self, dt, bbox=None):
        return {
            "bbox": bbox or [-180.0, -90.0, 180.0, 90.0],
            "properties": {"datetime": dt},
        }

    def test_temporal_extent_updated(self, ctx):
        col = make_collection(ctx)
        item = self._make_item("1850-01-31T00:00:00+00:00")
        col = update_collection_extent(col, item)
        interval = col["extent"]["temporal"]["interval"][0]
        assert interval[0] is not None
        assert interval[1] is not None

    def test_earlier_date_expands_start(self, ctx):
        col = make_collection(ctx)
        col = update_collection_extent(col, self._make_item("1850-06-30T00:00:00+00:00"))
        col = update_collection_extent(col, self._make_item("1850-01-31T00:00:00+00:00"))
        start = col["extent"]["temporal"]["interval"][0][0]
        assert "1850-01" in start

    def test_later_date_expands_end(self, ctx):
        col = make_collection(ctx)
        col = update_collection_extent(col, self._make_item("1850-01-31T00:00:00+00:00"))
        col = update_collection_extent(col, self._make_item("1850-12-31T00:00:00+00:00"))
        end = col["extent"]["temporal"]["interval"][0][1]
        assert "1850-12" in end

    def test_item_without_datetime_does_not_crash(self, ctx):
        col = make_collection(ctx)
        update_collection_extent(col, {"properties": {}})  # no datetime key


# ---------------------------------------------------------------------------
# stac/item.py
# ---------------------------------------------------------------------------

class TestMakeItem:
    def test_returns_geojson_feature(self, fesom_nc, sample_metadata, ctx):
        item = make_item(fesom_nc, sample_metadata, ctx)
        assert item["type"] == "Feature"

    def test_stac_version(self, fesom_nc, sample_metadata, ctx):
        item = make_item(fesom_nc, sample_metadata, ctx)
        assert item["stac_version"] == "1.0.0"

    def test_collection_is_not_null(self, fesom_nc, sample_metadata, ctx):
        item = make_item(fesom_nc, sample_metadata, ctx)
        assert item["collection"] is not None
        assert item["collection"] == "basic-001-fesom"

    def test_id_contains_variable_and_component(self, fesom_nc, sample_metadata, ctx):
        item = make_item(fesom_nc, sample_metadata, ctx)
        assert "ssh" in item["id"]
        assert "fesom" in item["id"]

    def test_geometry_is_polygon(self, fesom_nc, sample_metadata, ctx):
        item = make_item(fesom_nc, sample_metadata, ctx)
        assert item["geometry"]["type"] == "Polygon"

    def test_bbox_has_four_elements(self, fesom_nc, sample_metadata, ctx):
        item = make_item(fesom_nc, sample_metadata, ctx)
        assert len(item["bbox"]) == 4

    def test_data_asset_present(self, fesom_nc, sample_metadata, ctx):
        item = make_item(fesom_nc, sample_metadata, ctx)
        assert "data" in item["assets"]
        assert "href" in item["assets"]["data"]

    def test_data_asset_href_is_uri(self, fesom_nc, sample_metadata, ctx):
        item = make_item(fesom_nc, sample_metadata, ctx)
        href = item["assets"]["data"]["href"]
        # href should be a valid URI (file:// for local, or other protocols)
        assert href.startswith("file://") or "://" in href
        # For file:// URIs, the path after the protocol should be absolute
        if href.startswith("file://"):
            path_part = href[7:]  # Remove "file://"
            assert path_part.startswith("/"), f"file:// URI path should be absolute: {href}"

    def test_properties_contain_experiment(self, fesom_nc, sample_metadata, ctx):
        item = make_item(fesom_nc, sample_metadata, ctx)
        assert item["properties"]["experiment"] == "basic-001"

    def test_properties_contain_component(self, fesom_nc, sample_metadata, ctx):
        item = make_item(fesom_nc, sample_metadata, ctx)
        assert item["properties"]["component"] == "fesom"

    def test_cf_parameters_in_properties(self, fesom_nc, sample_metadata, ctx):
        item = make_item(fesom_nc, sample_metadata, ctx)
        assert "cf:parameter" in item["properties"]
        assert item["properties"]["cf:parameter"][0]["name"] == "sea_surface_height_above_geoid"

    def test_cf_extension_url_in_stac_extensions(self, fesom_nc, sample_metadata, ctx):
        item = make_item(fesom_nc, sample_metadata, ctx)
        assert EXTENSION_URLS["cf"] in item["stac_extensions"]

    def test_collection_link_present(self, fesom_nc, sample_metadata, ctx):
        item = make_item(fesom_nc, sample_metadata, ctx)
        rels = [lnk["rel"] for lnk in item["links"]]
        assert "collection" in rels

    def test_stable_id_for_same_file(self, fesom_nc, sample_metadata, ctx):
        id1 = make_item(fesom_nc, sample_metadata, ctx)["id"]
        id2 = make_item(fesom_nc, sample_metadata, ctx)["id"]
        assert id1 == id2
