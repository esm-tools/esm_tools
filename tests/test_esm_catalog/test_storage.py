"""Tests for the storage STAC extension."""

from __future__ import annotations

import json
from datetime import datetime, timezone
from pathlib import Path

import pytest
from pystac import Asset, Item

from esm_catalog.context import CollectionContext
from esm_catalog.item import make_item
from esm_catalog.registry import EXTENSION_URLS
from esm_catalog.storage import add_storage_extension

STORAGE_URL = EXTENSION_URLS["storage"]
SCHEMA_PATH = (
    Path(__file__).parents[2] / "configs" / "stac-extensions" / "storage" / "v1.0.0" / "schema.json"
)


@pytest.fixture
def make_bare_item():
    def _make(with_asset=True):
        item = Item(
            id="x",
            geometry=None,
            bbox=None,
            datetime=datetime(2000, 1, 1, tzinfo=timezone.utc),
            properties={},
        )
        if with_asset:
            item.add_asset("data", Asset(href="file:///tmp/x.nc"))
        return item

    return _make


def test_noop_without_machine_config_or_probe(tmp_path, make_bare_item):
    item = make_bare_item()
    missing = tmp_path / "does-not-exist.nc"
    add_storage_extension(item, missing, machine_config=None)
    assert item.properties == {}
    assert item.stac_extensions == []


def test_missing_path_with_probe_omits_last_access_without_raising(tmp_path, make_bare_item):
    item = make_bare_item()
    missing = tmp_path / "does-not-exist.nc"
    add_storage_extension(item, missing, machine_config=None, probe_last_access=True)
    assert "storage:last_access" not in item.properties
    assert item.stac_extensions == []


def test_non_path_argument_with_probe_raises(make_bare_item):
    # A caller bug (wrong argument type has no .stat()) should surface as an
    # error, not silently disappear the way a genuinely missing file does.
    item = make_bare_item()
    with pytest.raises(AttributeError):
        add_storage_extension(item, object(), machine_config=None, probe_last_access=True)


def test_last_access_not_probed_by_default(tmp_path, make_bare_item):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_bare_item()
    add_storage_extension(item, f, machine_config=None)
    assert "storage:last_access" not in item.properties
    assert item.stac_extensions == []


def test_last_access_populated_when_probing_opted_in(tmp_path, make_bare_item):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_bare_item()
    add_storage_extension(item, f, machine_config=None, probe_last_access=True)
    assert "storage:last_access" in item.properties
    assert STORAGE_URL in item.stac_extensions
    # institution/system/tier are not set without machine_config
    assert "storage:institution" not in item.properties
    assert "storage:tier" not in item.properties


def test_machine_config_sets_institution_system_and_tier(tmp_path, make_bare_item):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_bare_item()
    add_storage_extension(
        item,
        f,
        machine_config={"institution": "AWI", "system": "example-hpc", "storage_type": "lustre"},
    )
    assert item.properties["storage:institution"] == "AWI"
    assert item.properties["storage:system"] == "example-hpc"
    assert item.properties["storage:tier"] == "hot"
    assert item.assets["data"].extra_fields["storage:type"] == "lustre"
    assert STORAGE_URL in item.stac_extensions


def test_hpss_storage_type_maps_to_cold_tier(tmp_path, make_bare_item):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_bare_item()
    add_storage_extension(item, f, machine_config={"storage_type": "hpss"})
    assert item.properties["storage:tier"] == "cold"


def test_gpfs_storage_type_maps_to_warm_tier(tmp_path, make_bare_item):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_bare_item()
    add_storage_extension(item, f, machine_config={"storage_type": "gpfs"})
    assert item.properties["storage:tier"] == "warm"


def test_unrecognized_storage_type_leaves_tier_unset(tmp_path, make_bare_item):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_bare_item()
    add_storage_extension(item, f, machine_config={"storage_type": "ceph"})
    assert "storage:tier" not in item.properties
    assert item.assets["data"].extra_fields["storage:type"] == "ceph"
    assert STORAGE_URL in item.stac_extensions


def test_explicit_storage_tier_overrides_heuristic(tmp_path, make_bare_item):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_bare_item()
    add_storage_extension(
        item, f, machine_config={"storage_type": "gpfs", "storage_tier": "cold"}
    )
    assert item.properties["storage:tier"] == "cold"


def test_explicit_storage_tier_without_storage_type(tmp_path, make_bare_item):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_bare_item()
    add_storage_extension(item, f, machine_config={"storage_tier": "warm"})
    assert item.properties["storage:tier"] == "warm"
    assert "storage:type" not in item.assets["data"].extra_fields


def test_missing_data_asset_does_not_raise(tmp_path, make_bare_item):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_bare_item(with_asset=False)
    add_storage_extension(item, f, machine_config={"storage_type": "lustre"})
    assert item.properties["storage:tier"] == "hot"


def test_url_appended_once(tmp_path, make_bare_item):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_bare_item()
    add_storage_extension(item, f, machine_config={"institution": "AWI"})
    add_storage_extension(item, f, machine_config={"institution": "AWI"})
    assert item.stac_extensions.count(STORAGE_URL) == 1


# --- wiring through make_item ---


def _ctx(**kwargs):
    return CollectionContext(
        experiment_id="exp-alpha", component="echam", collection_id="exp-alpha", **kwargs
    )


def _metadata(**kwargs):
    base = {
        "variable": "temp",
        "format": "netcdf",
        "datetime_start": datetime(2000, 1, 1, tzinfo=timezone.utc),
        "datetime_end": datetime(2000, 1, 1, tzinfo=timezone.utc),
    }
    base.update(kwargs)
    return base


def test_make_item_without_machine_config_or_probe_sets_no_storage_fields(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_item(f, _metadata(), _ctx())
    assert "storage:last_access" not in item.properties
    assert "storage:institution" not in item.properties


def test_make_item_with_probe_last_access_sets_last_access(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_item(f, _metadata(), _ctx(probe_last_access=True))
    assert "storage:last_access" in item.properties


def test_make_item_with_machine_config_applies_storage_extension(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    ctx = _ctx(machine_config={"institution": "AWI", "system": "example-hpc", "storage_type": "lustre"})
    item = make_item(f, _metadata(), ctx)
    assert item.properties["storage:institution"] == "AWI"
    assert item.properties["storage:system"] == "example-hpc"
    assert item.assets["data"].extra_fields["storage:type"] == "lustre"
    assert STORAGE_URL in item.stac_extensions


def test_item_validates_against_storage_schema(tmp_path):
    jsonschema = pytest.importorskip("jsonschema")
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    ctx = _ctx(machine_config={"institution": "AWI", "system": "example-hpc", "storage_type": "lustre"})
    item_dict = make_item(f, _metadata(), ctx).to_dict()
    schema = json.loads(SCHEMA_PATH.read_text())
    jsonschema.validate(instance=item_dict, schema=schema)
