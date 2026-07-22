"""Tests for the paleo STAC extension and experiment_type classification."""

from __future__ import annotations

import json
from datetime import datetime, timezone
from pathlib import Path

import pytest
from pystac import Item

from esm_catalog.context import CollectionContext
from esm_catalog.item import make_item
from esm_catalog.paleo import (
    _format_geological,
    _parse_year_from_iso,
    add_experiment_type,
    add_paleo_extension,
)
from esm_catalog.registry import EXTENSION_URLS

PALEO_URL = EXTENSION_URLS["paleo"]
SCHEMA_PATH = Path(__file__).parent / "schemas" / "paleo-v1.0.0.json"


def _bare_item(dt=datetime(2000, 1, 1, tzinfo=timezone.utc)):
    return Item(
        id="i",
        geometry=None,
        bbox=None,
        datetime=dt,
        properties={},
    )


# --- add_paleo_extension ---


def test_noop_without_config_or_year():
    item = _bare_item()
    add_paleo_extension(item)
    assert "paleo:year" not in item.properties
    assert item.stac_extensions == []


def test_explicit_paleo_year():
    item = _bare_item()
    add_paleo_extension(item, paleo_year=-20000)
    assert item.properties["paleo:year"] == -20000
    assert item.properties["paleo:display"] == "22.0 ka"
    assert item.properties["paleo:reference_year"] == 2024
    assert PALEO_URL in item.stac_extensions


def test_config_reference_year_and_epoch_period():
    item = _bare_item()
    add_paleo_extension(
        item,
        paleo_config={
            "reference_year": -20000,
            "epoch": "Pleistocene",
            "period": "Quaternary",
        },
    )
    assert item.properties["paleo:year"] == -20000
    assert item.properties["paleo:epoch"] == "Pleistocene"
    assert item.properties["paleo:period"] == "Quaternary"


def test_bare_epoch_period_do_not_write_null():
    # A bare `epoch:`/`period:` in YAML is present-but-None; it must be skipped,
    # not written through as a schema-invalid null.
    item = _bare_item()
    add_paleo_extension(
        item,
        paleo_config={"reference_year": -20000, "epoch": None, "period": None},
    )
    assert item.properties["paleo:year"] == -20000
    assert "paleo:epoch" not in item.properties
    assert "paleo:period" not in item.properties


def test_explicit_year_overrides_config():
    item = _bare_item()
    add_paleo_extension(item, paleo_config={"reference_year": -100}, paleo_year=-20000)
    assert item.properties["paleo:year"] == -20000


def test_deep_time_from_start_datetime_string():
    # Years outside 0-9999 can only arrive as pre-formatted ISO strings.
    item = _bare_item()
    item.properties["start_datetime"] = "-21000-01-01T00:00:00Z"
    add_paleo_extension(item)
    assert item.properties["paleo:year"] == -21000


def test_normal_datetime_does_not_trigger_paleo():
    item = _bare_item()
    item.properties["start_datetime"] = "2000-01-01T00:00:00Z"
    add_paleo_extension(item)
    assert "paleo:year" not in item.properties


def test_url_appended_once():
    item = _bare_item()
    add_paleo_extension(item, paleo_year=-20000)
    add_paleo_extension(item, paleo_year=-20000)
    assert item.stac_extensions.count(PALEO_URL) == 1


# --- _format_geological ---


@pytest.mark.parametrize(
    "year,expected",
    [
        (-66_000_000, "66.0 Ma"),
        (-20000, "22.0 ka"),
        (-500, "501 BCE"),
        (0, "1 BCE"),
        (1492, "1492 CE"),
    ],
)
def test_format_geological(year, expected):
    assert _format_geological(year, reference_year=2024) == expected


# --- _parse_year_from_iso ---


@pytest.mark.parametrize(
    "dt_str,expected",
    [
        ("2000-01-01T00:00:00Z", 2000),
        ("-21000-01-01T00:00:00", -21000),
        ("", None),
        ("garbage", None),
    ],
)
def test_parse_year_from_iso(dt_str, expected):
    assert _parse_year_from_iso(dt_str) == expected


# --- add_experiment_type ---


def test_experiment_type_historical():
    item = _bare_item(datetime(2000, 1, 1, tzinfo=timezone.utc))
    add_experiment_type(item)
    assert item.properties["experiment_type"] == "historical"
    assert "paleo:years_bp" not in item.properties


def test_experiment_type_control():
    item = _bare_item(datetime(1850, 1, 1, tzinfo=timezone.utc))
    add_experiment_type(item)
    assert item.properties["experiment_type"] == "control"
    # years_bp only for paleo items, per plan
    assert "paleo:years_bp" not in item.properties


def test_experiment_type_paleo_sets_years_bp():
    item = _bare_item(datetime(1000, 1, 1, tzinfo=timezone.utc))
    add_experiment_type(item)
    assert item.properties["experiment_type"] == "paleo"
    assert item.properties["paleo:years_bp"] == 950
    assert PALEO_URL in item.stac_extensions


def test_experiment_type_prefers_paleo_year_property():
    # Deep-time run: model datetime says 2000 but paleo:year says LGM.
    item = _bare_item(datetime(2000, 1, 1, tzinfo=timezone.utc))
    add_paleo_extension(item, paleo_year=-21000)
    add_experiment_type(item)
    assert item.properties["experiment_type"] == "paleo"
    assert item.properties["paleo:years_bp"] == 1950 - (-21000)


def test_experiment_type_defaults_to_control_without_dates():
    item = Item(
        id="i", geometry=None, bbox=None, datetime=None,
        properties={"start_datetime": "garbage", "end_datetime": "garbage"},
    )
    add_experiment_type(item)
    assert item.properties["experiment_type"] == "control"
    assert "paleo:years_bp" not in item.properties


def test_experiment_type_does_not_read_namelist_properties():
    # Plan fix: nml:echam:runctl:dt_start must NOT influence classification.
    item = _bare_item(datetime(2000, 1, 1, tzinfo=timezone.utc))
    item.properties["nml:echam:runctl:dt_start"] = [1000, 1, 1]
    add_experiment_type(item)
    assert item.properties["experiment_type"] == "historical"


# --- wiring through make_item ---


def _ctx(**kwargs):
    return CollectionContext(
        experiment_id="exp", component="echam", collection_id="exp", **kwargs
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


def test_make_item_sets_experiment_type(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_item(f, _metadata(), _ctx())
    assert item.properties["experiment_type"] == "historical"
    assert "paleo:year" not in item.properties


def test_make_item_with_paleo_config(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    ctx = _ctx(paleo_config={"reference_year": -20000, "epoch": "Pleistocene"})
    item = make_item(f, _metadata(), ctx)
    assert item.properties["paleo:year"] == -20000
    assert item.properties["paleo:epoch"] == "Pleistocene"
    assert item.properties["experiment_type"] == "paleo"
    assert item.properties["paleo:years_bp"] == 1950 - (-20000)
    assert PALEO_URL in item.stac_extensions


def test_make_item_paleo_validates_against_schema(tmp_path):
    jsonschema = pytest.importorskip("jsonschema")
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    ctx = _ctx(paleo_config={"reference_year": -20000, "epoch": "Pleistocene"})
    item_dict = make_item(f, _metadata(), ctx).to_dict()
    schema = json.loads(SCHEMA_PATH.read_text())
    jsonschema.validate(instance=item_dict, schema=schema)
