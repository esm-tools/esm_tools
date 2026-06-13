"""make_collection is pure: accepts pre-scanned namelist data, never scans."""

from __future__ import annotations

from esm_catalog.context import CollectionContext
from esm_catalog.stac.collection import make_collection, update_collection_extent


def _ctx():
    return CollectionContext(
        experiment_id="exp-alpha", component="echam", collection_id="exp-alpha"
    )


def test_make_collection_minimal_skeleton():
    col = make_collection(_ctx())
    assert col["type"] == "Collection"
    assert col["id"] == "exp-alpha"
    assert col["components"] == ["echam"]
    assert "nml:files" not in col  # no namelists passed -> none added


def test_make_collection_applies_prescanned_namelists():
    namelists = {"namelist.echam": {"runctl": {"delta_time": 450, "lcouple": True}}}
    col = make_collection(_ctx(), namelists=namelists)
    assert col["nml:files"] == ["namelist.echam"]
    assert col["nml:parameters"]["runctl:delta_time"] == 450


def test_update_collection_extent_expands_temporal():
    col = make_collection(_ctx())
    item = {"bbox": [0, 0, 1, 1],
            "properties": {"datetime": "2000-01-01T00:00:00"}}
    col = update_collection_extent(col, item)
    assert col["extent"]["temporal"]["interval"][0][0] == "2000-01-01T00:00:00"


def test_make_collection_uses_collection_title_when_set():
    from esm_catalog.context import CollectionContext
    ctx = CollectionContext(experiment_id="exp", component="echam",
                            collection_id="exp", collection_title="Nice Title")
    col = make_collection(ctx)
    assert col["title"] == "Nice Title"
