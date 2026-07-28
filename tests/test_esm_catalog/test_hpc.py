"""Tests for the hpc STAC extension."""

from __future__ import annotations

import json
from datetime import datetime, timezone
from pathlib import Path

import pytest
from pystac import Asset, Item

from esm_catalog.context import CollectionContext
from esm_catalog.hpc import add_hpc_extension
from esm_catalog.item import make_item
from esm_catalog.registry import EXTENSION_URLS

HPC_URL = EXTENSION_URLS["hpc"]
SCHEMA_PATH = (
    Path(__file__).parents[2] / "configs" / "stac-extensions" / "hpc" / "v1.0.0" / "schema.json"
)


def _bare_item(with_asset=True):
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


def test_noop_without_machine_config_or_stat(tmp_path):
    item = _bare_item()
    missing = tmp_path / "does-not-exist.nc"
    add_hpc_extension(item, missing, machine_config=None)
    assert item.properties == {}
    assert item.stac_extensions == []


def test_last_access_populated_from_real_path(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = _bare_item()
    add_hpc_extension(item, f, machine_config=None)
    assert "hpc:last_access" in item.properties
    assert HPC_URL in item.stac_extensions
    # facility/system/tier are not set without machine_config
    assert "hpc:facility" not in item.properties
    assert "hpc:storage_tier" not in item.properties


def test_machine_config_sets_facility_system_and_tier(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = _bare_item()
    add_hpc_extension(
        item,
        f,
        machine_config={"facility": "AWI", "system": "albedo", "storage_type": "lustre"},
    )
    assert item.properties["hpc:facility"] == "AWI"
    assert item.properties["hpc:system"] == "albedo"
    assert item.properties["hpc:storage_tier"] == "hot"
    assert item.assets["data"].extra_fields["hpc:storage_type"] == "lustre"
    assert HPC_URL in item.stac_extensions


def test_hpss_storage_type_maps_to_cold_tier(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = _bare_item()
    add_hpc_extension(item, f, machine_config={"storage_type": "hpss"})
    assert item.properties["hpc:storage_tier"] == "cold"


def test_gpfs_storage_type_maps_to_warm_tier(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = _bare_item()
    add_hpc_extension(item, f, machine_config={"storage_type": "gpfs"})
    assert item.properties["hpc:storage_tier"] == "warm"


def test_missing_data_asset_does_not_raise(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = _bare_item(with_asset=False)
    add_hpc_extension(item, f, machine_config={"storage_type": "lustre"})
    assert item.properties["hpc:storage_tier"] == "hot"


def test_url_appended_once(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = _bare_item()
    add_hpc_extension(item, f, machine_config={"facility": "AWI"})
    add_hpc_extension(item, f, machine_config={"facility": "AWI"})
    assert item.stac_extensions.count(HPC_URL) == 1


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


def test_make_item_without_machine_config_only_sets_last_access(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_item(f, _metadata(), _ctx())
    assert "hpc:last_access" in item.properties
    assert "hpc:facility" not in item.properties


def test_make_item_with_machine_config_applies_hpc_extension(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    ctx = _ctx(machine_config={"facility": "AWI", "system": "albedo", "storage_type": "lustre"})
    item = make_item(f, _metadata(), ctx)
    assert item.properties["hpc:facility"] == "AWI"
    assert item.properties["hpc:system"] == "albedo"
    assert item.assets["data"].extra_fields["hpc:storage_type"] == "lustre"
    assert HPC_URL in item.stac_extensions


def test_item_validates_against_hpc_schema(tmp_path):
    jsonschema = pytest.importorskip("jsonschema")
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    ctx = _ctx(machine_config={"facility": "AWI", "system": "albedo", "storage_type": "lustre"})
    item_dict = make_item(f, _metadata(), ctx).to_dict()
    schema = json.loads(SCHEMA_PATH.read_text())
    jsonschema.validate(instance=item_dict, schema=schema)
