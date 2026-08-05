"""Tests for the paleo STAC extension (geological time)."""

from __future__ import annotations

import jsonschema
import pytest

from esm_catalog.collection import make_collection
from esm_catalog.item import make_item
from esm_catalog.paleo import add_paleo_collection_extension, add_paleo_item_extension
from esm_catalog.registry import EXTENSION_URLS
from esm_catalog.stac_ext import load_schema

from .helpers import assert_valid, bare_collection, bare_item, make_ctx, metadata

PALEO_URL = EXTENSION_URLS["paleo"]

LGM = "-21000-01-01T00:00:00"
CE1850 = "1850-01-01T00:00:00"


@pytest.fixture
def item():
    return bare_item()


@pytest.fixture
def collection():
    return bare_collection()


@pytest.fixture
def schema():
    return load_schema("paleo")


def _instance(props):
    return {"type": "Feature", "stac_extensions": [PALEO_URL], "properties": props}


# --- add_paleo_item_extension ---


def test_noop_without_config(item):
    add_paleo_item_extension(item)
    assert "paleo:datetime" not in item.properties
    assert item.stac_extensions == []


def test_config_datetime(item):
    add_paleo_item_extension(item, paleo_config={"datetime": LGM})
    assert item.properties["paleo:datetime"] == LGM
    assert PALEO_URL in item.stac_extensions


def test_ce_datetime_passes_through(item):
    add_paleo_item_extension(item, paleo_config={"datetime": CE1850})
    assert item.properties["paleo:datetime"] == CE1850


def test_missing_datetime_is_noop(item):
    add_paleo_item_extension(item, paleo_config={"description": "some paleo setup"})
    assert "paleo:datetime" not in item.properties
    assert item.stac_extensions == []


def test_malformed_datetime_raises(item):
    with pytest.raises(jsonschema.ValidationError):
        add_paleo_item_extension(item, paleo_config={"datetime": "21 ka"})


def test_label_alongside_datetime(item):
    add_paleo_item_extension(item, paleo_config={"datetime": LGM, "label": "LGM"})
    assert item.properties["paleo:datetime"] == LGM
    assert item.properties["paleo:label"] == "LGM"


def test_label_only(item):
    # label is independent of the datetimes; a label-only config is allowed.
    add_paleo_item_extension(item, paleo_config={"label": "mid-Holocene"})
    assert item.properties["paleo:label"] == "mid-Holocene"
    assert "paleo:datetime" not in item.properties
    assert PALEO_URL in item.stac_extensions


def test_url_appended_once(item):
    add_paleo_item_extension(item, paleo_config={"datetime": LGM})
    add_paleo_item_extension(item, paleo_config={"datetime": LGM})
    assert item.stac_extensions.count(PALEO_URL) == 1


def test_transient_range_sets_start_and_end(item):
    # Transient run (deglaciation, 21 ka BP -> 1850 CE): start/end, no datetime.
    add_paleo_item_extension(
        item, paleo_config={"start_datetime": LGM, "end_datetime": CE1850}
    )
    assert item.properties["paleo:start_datetime"] == LGM
    assert item.properties["paleo:end_datetime"] == CE1850
    assert "paleo:datetime" not in item.properties
    assert PALEO_URL in item.stac_extensions


def test_half_range_raises(item):
    # Like STAC's start_datetime/end_datetime, the two must be given together.
    with pytest.raises(jsonschema.ValidationError):
        add_paleo_item_extension(item, paleo_config={"start_datetime": LGM})


@pytest.mark.parametrize("year", [-65_000_000, -1_070_000, -21000, 0, 850, 1850])
def test_stored_datetime_parses_in_paleodatetime(item, year):
    # A paleodatetime string round-trips through the catalog and re-parses.
    pdt = pytest.importorskip("paleodatetime")
    s = pdt.PaleoDateTime(year=year, month=1, day=1).isoformat()
    add_paleo_item_extension(item, paleo_config={"datetime": s})
    assert (
        pdt.PaleoDateTime.fromisoformat(item.properties["paleo:datetime"]).year == year
    )


# --- wiring through make_item ---


def test_make_item_without_paleo_config_sets_no_paleo_fields(temp_nc):
    item = make_item(temp_nc, metadata(), make_ctx())
    assert "paleo:datetime" not in item.properties
    assert PALEO_URL not in item.stac_extensions


def test_make_item_with_paleo_config(temp_nc):
    item = make_item(temp_nc, metadata(), make_ctx(paleo_config={"datetime": LGM}))
    assert item.properties["paleo:datetime"] == LGM
    assert PALEO_URL in item.stac_extensions


def test_make_item_paleo_validates_against_schema(temp_nc, schema):
    item = make_item(temp_nc, metadata(), make_ctx(paleo_config={"datetime": LGM}))
    assert_valid(item, schema)


@pytest.mark.parametrize(
    "props, valid",
    [
        ({"paleo:datetime": LGM}, True),
        ({"paleo:start_datetime": LGM, "paleo:end_datetime": CE1850}, True),
        ({"paleo:datetime": LGM, "paleo:label": "LGM"}, True),  # label + datetime
        ({"paleo:label": "LGM"}, True),  # label alone
        ({"paleo:datetime": "21 ka"}, False),  # malformed
        ({"paleo:start_datetime": LGM}, False),  # half range
        (
            {
                "paleo:datetime": LGM,
                "paleo:start_datetime": LGM,
                "paleo:end_datetime": CE1850,
            },
            False,
        ),
        ({"paleo:label": 123}, False),  # label must be a string
        ({"paleo:bogus": "x"}, False),  # invented key rejected by the namespace lock
    ],
)
def test_schema_constraints(schema, props, valid):
    if valid:
        jsonschema.validate(instance=_instance(props), schema=schema)
    else:
        with pytest.raises(jsonschema.ValidationError):
            jsonschema.validate(instance=_instance(props), schema=schema)


# --- collection level (add_paleo_collection_extension / make_collection) ---


def test_summary_noop_without_config(collection):
    add_paleo_collection_extension(collection)
    assert collection.summaries.is_empty()
    assert collection.stac_extensions == []


def test_summary_single(collection):
    add_paleo_collection_extension(collection, {"datetime": LGM})
    assert collection.summaries.get_list("paleo:datetime") == [LGM]
    assert PALEO_URL in collection.stac_extensions


def test_summary_includes_label(collection):
    add_paleo_collection_extension(collection, {"datetime": LGM, "label": "LGM"})
    assert collection.summaries.get_list("paleo:label") == ["LGM"]


def test_summary_malformed_raises(collection):
    with pytest.raises(jsonschema.ValidationError):
        add_paleo_collection_extension(collection, {"datetime": "21 ka"})


def test_summary_transient(collection):
    add_paleo_collection_extension(
        collection, {"start_datetime": LGM, "end_datetime": CE1850}
    )
    assert collection.summaries.get_list("paleo:start_datetime") == [LGM]
    assert collection.summaries.get_list("paleo:end_datetime") == [CE1850]
    assert collection.summaries.get_list("paleo:datetime") is None


def test_collection_validates_against_schema(collection, schema):
    add_paleo_collection_extension(collection, {"datetime": LGM})
    assert_valid(collection, schema)


def test_make_collection_with_paleo_config():
    col = make_collection(make_ctx(description="d", paleo_config={"datetime": LGM}))
    assert col.summaries.get_list("paleo:datetime") == [LGM]
    assert PALEO_URL in col.stac_extensions


def test_make_collection_without_paleo_config():
    col = make_collection(make_ctx(description="d"))
    assert col.summaries.is_empty()
    assert PALEO_URL not in col.stac_extensions
