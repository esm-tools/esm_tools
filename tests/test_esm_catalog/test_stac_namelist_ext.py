"""Tests for the namelist STAC extension."""

from __future__ import annotations

import json
from datetime import datetime, timezone
from pathlib import Path

import esm_tools
import pytest
from pystac import Item

from esm_catalog.collection import make_collection
from esm_catalog.context import CollectionContext
from esm_catalog.item import make_item
from esm_catalog.namelist import add_namelist_extension, add_namelist_item_extension
from esm_catalog.registry import EXTENSION_URLS

NAMELIST_URL = EXTENSION_URLS["namelist"]
SCHEMA_PATH = Path(esm_tools.get_config_filepath("stac-extensions/namelist/v1.0.0/schema.json"))


def _ctx(**kwargs):
    return CollectionContext(
        experiment_id="exp", component="echam", collection_id="exp", **kwargs
    )


def _bare_item():
    return Item(
        id="i",
        geometry=None,
        bbox=None,
        datetime=datetime(2000, 1, 1, tzinfo=timezone.utc),
        properties={},
    )


# --- add_namelist_extension (collection-level) ---


def test_collection_noop_without_namelists():
    col = make_collection(_ctx())
    add_namelist_extension(col, {})
    assert "nml:files" not in col.extra_fields
    assert col.stac_extensions == []


def test_collection_extension_flattens_parameters():
    col = make_collection(_ctx())
    add_namelist_extension(
        col, {"namelist.echam": {"runctl": {"delta_time": 450, "lcouple": True}}}
    )
    assert col.extra_fields["nml:parameters"]["namelist.echam:runctl:delta_time"] == 450
    assert col.extra_fields["nml:parameters"]["namelist.echam:runctl:lcouple"] is True


def test_collection_extension_lists_files_and_groups():
    col = make_collection(_ctx())
    add_namelist_extension(
        col,
        {
            "namelist.echam": {"runctl": {"delta_time": 450}},
            "namelist.jsbach": {"jsbach_ctl": {"use_dynveg": True}},
        },
    )
    assert col.extra_fields["nml:files"] == ["namelist.echam", "namelist.jsbach"]
    assert col.extra_fields["nml:groups"] == ["jsbach_ctl", "runctl"]
    assert NAMELIST_URL in col.stac_extensions


def test_collection_extension_drops_raw():
    # nml:raw was in the pr-a1a version; dropped here (may be megabytes).
    col = make_collection(_ctx())
    add_namelist_extension(
        col, {"namelist.echam": {"runctl": {"delta_time": 450}}}
    )
    assert "nml:raw" not in col.extra_fields


def test_collection_extension_skips_nested_dicts_and_none():
    col = make_collection(_ctx())
    add_namelist_extension(
        col,
        {
            "namelist.echam": {
                "runctl": {
                    "delta_time": 450,
                    "nested": {"a": 1},
                    "levels": list(range(20)),
                    "none_value": None,
                }
            }
        },
    )
    params = col.extra_fields["nml:parameters"]
    assert params["namelist.echam:runctl:delta_time"] == 450
    assert params["namelist.echam:runctl:levels"] == list(range(20))  # scalar lists kept, no length cap
    assert "namelist.echam:runctl:nested" not in params
    assert "namelist.echam:runctl:none_value" not in params


def test_collection_extension_url_appended_once():
    col = make_collection(_ctx())
    add_namelist_extension(col, {"namelist.echam": {"runctl": {"delta_time": 450}}})
    add_namelist_extension(col, {"namelist.echam": {"runctl": {"delta_time": 450}}})
    assert col.stac_extensions.count(NAMELIST_URL) == 1


def test_collection_extension_accepts_real_f90nml_namelist():
    # namelists_by_component maps filename -> parsed namelist; the scan layer
    # hands us the f90nml.Namelist object itself, not a plain dict copy of it.
    f90nml = pytest.importorskip("f90nml")
    nml = f90nml.reads(
        """
        &runctl
          delta_time = 450
          lcouple = .true.
        /
        """
    )
    col = make_collection(_ctx())
    add_namelist_extension(col, {"namelist.echam": nml})
    assert col.extra_fields["nml:parameters"]["namelist.echam:runctl:delta_time"] == 450
    assert col.extra_fields["nml:parameters"]["namelist.echam:runctl:lcouple"] is True
    assert col.extra_fields["nml:groups"] == ["runctl"]


def test_collection_extension_with_shipped_namelist():
    # Flatten a real namelist file shipped in the repo, read the way the scan
    # layer would (esm_tools packaging accessor + f90nml).
    f90nml = pytest.importorskip("f90nml")
    nml = f90nml.read(esm_tools.get_namelist_filepath("amip/namelist.amip"))
    col = make_collection(_ctx())
    add_namelist_extension(col, {"namelist.amip": nml})
    params = col.extra_fields["nml:parameters"]
    assert params["namelist.amip:namamip:runlengthsec"] == 86400
    assert params["namelist.amip:namamip:startyear"] == 1850
    assert col.extra_fields["nml:files"] == ["namelist.amip"]
    assert col.extra_fields["nml:groups"] == ["namamip"]
    assert NAMELIST_URL in col.stac_extensions


def test_collection_repeated_group_is_indexed():
    # A group repeated in a file (an f90nml Cogroup) must not collapse onto one
    # key; each occurrence gets a [(index)] suffix (like esm_environment's
    # PATH[(0)]), so both values survive.
    f90nml = pytest.importorskip("f90nml")
    nml = f90nml.reads("&rep\n x=1\n/\n&rep\n x=2\n/\n&solo\n y=9\n/")
    col = make_collection(_ctx())
    add_namelist_extension(col, {"namelist.bgc": nml})
    params = col.extra_fields["nml:parameters"]
    assert params["namelist.bgc:rep[(0)]:x"] == 1
    assert params["namelist.bgc:rep[(1)]:x"] == 2
    assert params["namelist.bgc:solo:y"] == 9


# --- add_namelist_item_extension (item-level, all components) ---


def test_item_extension_noop_without_namelists():
    ctx = _ctx()
    item = _bare_item()
    add_namelist_item_extension(item, ctx.namelists_by_component)
    assert item.properties == {}
    assert item.stac_extensions == []


def test_item_extension_uses_prescanned_context():
    ctx = _ctx(
        namelists_by_component={
            "echam": {"namelist.echam": {"runctl": {"co2vmr": 0.000284}}},
        }
    )
    item = _bare_item()
    add_namelist_item_extension(item, ctx.namelists_by_component)
    assert item.properties["nml:echam:namelist.echam:runctl:co2vmr"] == 0.000284
    assert NAMELIST_URL in item.stac_extensions


def test_item_extension_covers_all_components():
    ctx = _ctx(
        namelists_by_component={
            "echam": {"namelist.echam": {"runctl": {"delta_time": 450}}},
            "jsbach": {"namelist.jsbach": {"jsbach_ctl": {"use_dynveg": True}}},
        }
    )
    item = _bare_item()
    add_namelist_item_extension(item, ctx.namelists_by_component)
    assert item.properties["nml:echam:namelist.echam:runctl:delta_time"] == 450
    assert item.properties["nml:jsbach:namelist.jsbach:jsbach_ctl:use_dynveg"] is True


# --- schema validation ---


def test_collection_validates_against_namelist_schema():
    jsonschema = pytest.importorskip("jsonschema")
    ctx = _ctx(
        namelists_by_component={
            "echam": {"namelist.echam": {"runctl": {"delta_time": 450}}},
        }
    )
    col_dict = make_collection(ctx).to_dict()
    schema = json.loads(SCHEMA_PATH.read_text())
    jsonschema.validate(instance=col_dict, schema=schema)


def test_item_validates_against_namelist_schema(tmp_path):
    jsonschema = pytest.importorskip("jsonschema")
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    ctx = _ctx(
        namelists_by_component={
            "echam": {"namelist.echam": {"runctl": {"delta_time": 450}}},
        }
    )
    metadata = {
        "variable": "temp",
        "format": "netcdf",
        "datetime_start": datetime(2000, 1, 1, tzinfo=timezone.utc),
        "datetime_end": datetime(2000, 1, 1, tzinfo=timezone.utc),
    }
    item_dict = make_item(f, metadata, ctx).to_dict()
    schema = json.loads(SCHEMA_PATH.read_text())
    jsonschema.validate(instance=item_dict, schema=schema)
