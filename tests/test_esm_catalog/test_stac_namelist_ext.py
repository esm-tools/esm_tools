"""add_namelist_item_extension reads pre-scanned ctx data; it never scans."""

from __future__ import annotations

from esm_catalog.context import CollectionContext
from esm_catalog.stac.extensions.namelist import (
    add_namelist_extension,
    add_namelist_item_extension,
)


def test_collection_extension_flattens_parameters():
    col = {"type": "Collection", "id": "x", "stac_extensions": []}
    col = add_namelist_extension(
        col, {"namelist.echam": {"runctl": {"delta_time": 450, "lcouple": True}}}
    )
    assert col["nml:parameters"]["runctl:delta_time"] == 450
    assert col["nml:parameters"]["runctl:lcouple"] is True


def test_item_extension_uses_prescanned_context():
    ctx = CollectionContext(
        experiment_id="exp", component="echam", collection_id="exp",
        namelists_by_component={
            "echam": {"namelist.echam": {"runctl": {"co2vmr": 0.000284}}},
        },
    )
    item = {"id": "i", "properties": {}, "stac_extensions": []}
    item = add_namelist_item_extension(item, ctx)
    assert item["properties"]["nml:echam:runctl:co2vmr"] == 0.000284


def test_item_extension_noop_without_namelists():
    ctx = CollectionContext(experiment_id="e", component="c", collection_id="e")
    item = {"id": "i", "properties": {}, "stac_extensions": []}
    assert add_namelist_item_extension(item, ctx) == item
