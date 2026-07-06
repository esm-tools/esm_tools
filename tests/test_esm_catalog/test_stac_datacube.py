"""Tests for the datacube STAC extension."""

from __future__ import annotations

from datetime import datetime, timezone

from pystac import Item

from esm_catalog.datacube import add_datacube_extension
from esm_catalog.registry import EXTENSION_URLS

DATACUBE_URL = EXTENSION_URLS["datacube"]


def _bare_item():
    return Item(
        id="x",
        geometry=None,
        bbox=None,
        datetime=datetime(2000, 1, 1, tzinfo=timezone.utc),
        properties={},
    )


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
    item = _bare_item()
    add_datacube_extension(item, {})
    assert "cube:dimensions" not in item.properties
    assert "cube:variables" not in item.properties
    assert item.stac_extensions == []


def test_noop_with_variables_but_no_dimensions():
    # v2.2.0 requires cube:dimensions whenever the extension is declared,
    # so variables alone must not trigger it.
    item = _bare_item()
    add_datacube_extension(item, {"variables": [{"name": "temp"}]})
    assert item.properties == {}
    assert item.stac_extensions == []


def test_dimensions_only():
    item = _bare_item()
    add_datacube_extension(item, {"dimensions": _dims()})
    assert item.properties["cube:dimensions"] == _dims()
    assert "cube:variables" not in item.properties
    assert DATACUBE_URL in item.stac_extensions


def test_variable_mapping():
    item = _bare_item()
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
    add_datacube_extension(
        item, {"dimensions": _dims(), "variables": variables}
    )
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
    item = _bare_item()
    variables = [{"units": "K"}, {"name": "temp"}]
    add_datacube_extension(
        item, {"dimensions": _dims(), "variables": variables}
    )
    assert list(item.properties["cube:variables"]) == ["temp"]


def test_url_appended_once():
    item = _bare_item()
    add_datacube_extension(item, {"dimensions": _dims()})
    add_datacube_extension(item, {"dimensions": _dims()})
    assert item.stac_extensions.count(DATACUBE_URL) == 1
