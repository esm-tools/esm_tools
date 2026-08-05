"""Tests for the datacube STAC extension."""

from __future__ import annotations

import json
from pathlib import Path

from esm_catalog.datacube import add_datacube_item_extension
from esm_catalog.item import make_item
from esm_catalog.registry import EXTENSION_URLS

from .helpers import assert_valid, bare_item, make_ctx, metadata

DATACUBE_URL = EXTENSION_URLS["datacube"]
SCHEMA_PATH = Path(__file__).parent / "schemas" / "datacube-v2.2.0.json"


def _dims():
    return {
        "time": {
            "type": "temporal",
            "extent": ["2000-01-01T00:00:00Z", "2000-12-31T00:00:00Z"],
        },
        "lat": {"type": "spatial", "axis": "y", "extent": [-90.0, 90.0]},
        "lon": {"type": "spatial", "axis": "x", "extent": [-180.0, 180.0]},
    }


def test_noop_without_dimensions_or_variables():
    item = bare_item()
    add_datacube_item_extension(item, {})
    assert "cube:dimensions" not in item.properties
    assert "cube:variables" not in item.properties
    assert item.stac_extensions == []


def test_noop_with_variables_but_no_dimensions():
    # v2.2.0 requires cube:dimensions whenever the extension is declared,
    # so variables alone must not trigger it.
    item = bare_item()
    add_datacube_item_extension(item, {"variables": [{"name": "temp"}]})
    assert item.properties == {}
    assert item.stac_extensions == []


def test_dimensions_only():
    item = bare_item()
    add_datacube_item_extension(item, {"dimensions": _dims()})
    assert item.properties["cube:dimensions"] == _dims()
    assert "cube:variables" not in item.properties
    assert DATACUBE_URL in item.stac_extensions


def test_variable_mapping():
    item = bare_item()
    variables = [
        {
            "name": "temp",
            "units": "K",
            "long_name": "air temperature",
            "standard_name": "air_temperature",
            "dimensions": ["time", "lat", "lon"],
        },
        {"name": "precip", "description": "explicit", "long_name": "ignored"},
        {"name": "u10", "standard_name": "eastward_wind"},
    ]
    add_datacube_item_extension(item, {"dimensions": _dims(), "variables": variables})
    cube_vars = item.properties["cube:variables"]
    assert cube_vars["temp"] == {
        "dimensions": ["time", "lat", "lon"],
        "unit": "K",
        "description": "air temperature",
    }
    assert cube_vars["precip"]["description"] == "explicit"
    assert cube_vars["precip"]["dimensions"] == []
    assert cube_vars["u10"]["description"] == "eastward_wind"


def test_variable_without_name_is_skipped():
    item = bare_item()
    variables = [{"units": "K"}, {"name": "temp"}]
    add_datacube_item_extension(item, {"dimensions": _dims(), "variables": variables})
    assert list(item.properties["cube:variables"]) == ["temp"]


def test_url_appended_once():
    item = bare_item()
    add_datacube_item_extension(item, {"dimensions": _dims()})
    add_datacube_item_extension(item, {"dimensions": _dims()})
    assert item.stac_extensions.count(DATACUBE_URL) == 1


# --- wiring through make_item ---


def test_make_item_without_dims_has_no_datacube(temp_nc):
    item = make_item(temp_nc, metadata(), make_ctx())
    assert "cube:dimensions" not in item.properties
    assert item.stac_extensions == []


def test_make_item_with_dims_applies_datacube(temp_nc):
    meta = metadata(dimensions=_dims(), variables=[{"name": "temp", "units": "K"}])
    item = make_item(temp_nc, meta, make_ctx())
    assert item.properties["cube:dimensions"] == _dims()
    assert item.properties["cube:variables"]["temp"]["unit"] == "K"
    assert DATACUBE_URL in item.stac_extensions


def test_item_validates_against_datacube_schema(temp_nc):
    meta = metadata(
        dimensions=_dims(),
        variables=[
            {
                "name": "temp",
                "units": "K",
                "long_name": "air temperature",
                "dimensions": ["time", "lat", "lon"],
            }
        ],
    )
    item = make_item(temp_nc, meta, make_ctx())
    assert_valid(item, json.loads(SCHEMA_PATH.read_text()))


def test_all_nameless_variables_omit_cube_variables():
    item = bare_item()
    add_datacube_item_extension(
        item, {"dimensions": _dims(), "variables": [{"units": "K"}]}
    )
    assert "cube:variables" not in item.properties
    assert item.properties["cube:dimensions"] == _dims()
    assert DATACUBE_URL in item.stac_extensions
