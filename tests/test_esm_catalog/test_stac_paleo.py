"""Tests for the paleo STAC extension (geological time)."""

from __future__ import annotations

import json
from datetime import datetime, timezone
from pathlib import Path

import pytest
from pystac import Item

from esm_catalog.context import CollectionContext
from esm_catalog.item import make_item
from esm_catalog.paleo import add_paleo_data
from esm_catalog.registry import EXTENSION_URLS

PALEO_URL = EXTENSION_URLS["paleo"]
SCHEMA_PATH = (
    Path(__file__).parents[2] / "configs" / "stac-extensions" / "paleo" / "v1.0.0" / "schema.json"
)

LGM = "-21000-01-01T00:00:00"


@pytest.fixture
def item():
    return Item(
        id="i",
        geometry=None,
        bbox=None,
        datetime=datetime(2000, 1, 1, tzinfo=timezone.utc),
        properties={},
    )


# --- add_paleo_data ---


def test_noop_without_config(item):
    add_paleo_data(item)
    assert "paleo:datetime" not in item.properties
    assert item.stac_extensions == []


def test_config_datetime(item):
    add_paleo_data(item, paleo_config={"datetime": LGM})
    assert item.properties["paleo:datetime"] == LGM
    assert PALEO_URL in item.stac_extensions


def test_ce_datetime_passes_through(item):
    add_paleo_data(item, paleo_config={"datetime": "1850-01-01T00:00:00"})
    assert item.properties["paleo:datetime"] == "1850-01-01T00:00:00"


def test_missing_datetime_is_noop(item):
    # A paleo_config with other keys but no datetime is not a paleo run.
    add_paleo_data(item, paleo_config={"description": "some paleo setup"})
    assert "paleo:datetime" not in item.properties
    assert item.stac_extensions == []


def test_malformed_datetime_raises(item):
    with pytest.raises(ValueError):
        add_paleo_data(item, paleo_config={"datetime": "21 ka"})


def test_url_appended_once(item):
    add_paleo_data(item, paleo_config={"datetime": LGM})
    add_paleo_data(item, paleo_config={"datetime": LGM})
    assert item.stac_extensions.count(PALEO_URL) == 1


def test_transient_range_sets_start_and_end(item):
    # A deglaciation run: 21 ka BP -> 1850 CE. Separate start_datetime/
    # end_datetime scalars -> paleo:start/end_datetime, no single paleo:datetime.
    add_paleo_data(
        item,
        paleo_config={"start_datetime": LGM, "end_datetime": "1850-01-01T00:00:00"},
    )
    assert item.properties["paleo:start_datetime"] == LGM
    assert item.properties["paleo:end_datetime"] == "1850-01-01T00:00:00"
    assert "paleo:datetime" not in item.properties
    assert PALEO_URL in item.stac_extensions


def test_half_range_raises(item):
    # Like STAC's start_datetime/end_datetime, the two must be given together.
    with pytest.raises(ValueError):
        add_paleo_data(item, paleo_config={"start_datetime": LGM})


@pytest.mark.parametrize("year", [-65_000_000, -1_070_000, -21000, 0, 850, 1850])
def test_stored_datetime_parses_in_paleodatetime(item, year):
    # A paleodatetime-produced string round-trips through the catalog unchanged
    # and re-parses to the same year on the consumer side.
    pdt = pytest.importorskip("paleodatetime")
    s = pdt.PaleoDateTime(year=year, month=1, day=1).isoformat()
    add_paleo_data(item, paleo_config={"datetime": s})
    assert pdt.PaleoDateTime.fromisoformat(item.properties["paleo:datetime"]).year == year


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


def test_make_item_without_paleo_config_sets_no_paleo_fields(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_item(f, _metadata(), _ctx())
    assert "paleo:datetime" not in item.properties
    assert PALEO_URL not in item.stac_extensions


def test_make_item_with_paleo_config(tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item = make_item(f, _metadata(), _ctx(paleo_config={"datetime": LGM}))
    assert item.properties["paleo:datetime"] == LGM
    assert PALEO_URL in item.stac_extensions


def test_make_item_paleo_validates_against_schema(tmp_path):
    jsonschema = pytest.importorskip("jsonschema")
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    item_dict = make_item(f, _metadata(), _ctx(paleo_config={"datetime": LGM})).to_dict()
    schema = json.loads(SCHEMA_PATH.read_text())
    jsonschema.validate(instance=item_dict, schema=schema)


def test_schema_rejects_malformed_paleo_datetime():
    jsonschema = pytest.importorskip("jsonschema")
    schema = json.loads(SCHEMA_PATH.read_text())
    bad = {
        "type": "Feature",
        "stac_extensions": [PALEO_URL],
        "properties": {"paleo:datetime": "21 ka"},
    }
    with pytest.raises(jsonschema.ValidationError):
        jsonschema.validate(instance=bad, schema=schema)
