"""Tests for the namelist STAC extension.

Namelist inputs are real Fortran namelists read from data/namelists/*.nml — the
same f90nml.Namelist objects the scan layer produces — assembled into the
extension's shapes by two small builders:

    component_namelists("echam", "jsbach")  -> one component's files
    by_component("echam", "jsbach")         -> all components, keyed by component
"""

from __future__ import annotations

from pathlib import Path
from typing import NamedTuple

import f90nml
import jsonschema
import pytest

import esm_tools
from esm_catalog.collection import make_collection
from esm_catalog.item import make_item
from esm_catalog.namelist import (
    Namelist,
    NamelistFilename,
    _arrow_safe,
    _is_queryable,
    add_namelist_collection_extension,
    add_namelist_item_extension,
)
from esm_catalog.registry import EXTENSION_URLS
from esm_catalog.stac_ext import load_schema

from .helpers import assert_valid, make_exp_metadata, make_file_metadata

# collection, item come from tests/test_esm_catalog/conftest.py

NAMELIST_URL = EXTENSION_URLS["namelist"]

_DATA = Path(__file__).parent / "data" / "namelists"


def read_nml(name: str) -> Namelist:
    """The parsed Fortran namelist at data/namelists/<name>.nml."""
    return f90nml.read(str(_DATA / f"{name}.nml"))


def component_namelists(*names: str) -> dict:
    """One component's files: {"namelist.<name>": <parsed namelist>} per name."""
    return {f"namelist.{name}": read_nml(name) for name in names}


def by_component(*names: str) -> dict:
    """All components: {"<name>": {"namelist.<name>": <parsed namelist>}} per name."""
    return {name: component_namelists(name) for name in names}


@pytest.fixture
def nml_schema():
    return load_schema("namelist")


# --- _is_queryable predicate ---


@pytest.mark.parametrize(
    "value, queryable",
    [
        (450, True),
        (1.5, True),
        ("text", True),
        (True, True),
        ([1, 2, 3], True),
        (list(range(20)), True),  # long scalar lists kept — no length cap
        ([], True),
        (None, False),
        ({"a": 1}, False),  # a nested group is not a scalar
        (complex(1, 2), False),  # not JSON-serializable
    ],
)
def test_is_queryable(value, queryable):
    assert _is_queryable(value) is queryable


# --- add_namelist_collection_extension (collection-level) ---


def test_collection_noop_without_namelists(collection):
    add_namelist_collection_extension(collection, {})
    assert "namelist:parameters" not in collection.extra_fields
    assert collection.stac_extensions == []


def test_collection_nests_parameters(collection):
    add_namelist_collection_extension(collection, by_component("echam"))
    params = collection.extra_fields["namelist:parameters"]
    assert params["echam"]["namelist_echam"]["runctl"]["delta_time"] == 450
    assert params["echam"]["namelist_echam"]["runctl"]["lcouple"] is True


def test_collection_url_registered(collection):
    add_namelist_collection_extension(collection, by_component("echam", "jsbach"))
    assert NAMELIST_URL in collection.stac_extensions


def test_collection_url_appended_once(collection):
    add_namelist_collection_extension(collection, by_component("echam"))
    add_namelist_collection_extension(collection, by_component("echam"))
    assert collection.stac_extensions.count(NAMELIST_URL) == 1


def test_collection_component_qualified_keys_avoid_filename_collision(collection):
    # Two components ship a same-named namelist file; component is the
    # outermost nesting level, so the two branches cannot collide.
    add_namelist_collection_extension(
        collection,
        {
            "echam": {"namelist.io": read_nml("echam")},
            "jsbach": {"namelist.io": read_nml("jsbach")},
        },
    )
    params = collection.extra_fields["namelist:parameters"]
    assert params["echam"]["namelist_io"]["runctl"]["delta_time"] == 450
    assert params["jsbach"]["namelist_io"]["jsbach_ctl"]["use_dynveg"] is True


def test_collection_indexes_repeated_group(collection):
    # A group repeated in a file (an f90nml Cogroup) must not collapse onto one
    # key; each occurrence gets a '_index' suffix, so both values survive as
    # distinct nested keys.
    add_namelist_collection_extension(
        collection, {"bgc": {"namelist.bgc": read_nml("repeated_group")}}
    )
    params = collection.extra_fields["namelist:parameters"]
    assert params["bgc"]["namelist_bgc"]["rep_0"]["x"] == 1
    assert params["bgc"]["namelist_bgc"]["rep_1"]["x"] == 2
    assert params["bgc"]["namelist_bgc"]["solo"]["y"] == 9


def test_collection_drops_non_json_scalar(collection):
    # f90nml yields a Fortran complex; it is not JSON-serializable and must be
    # skipped, not emitted (which would crash to_dict/json.dumps later).
    add_namelist_collection_extension(
        collection, {"echam": {"namelist.echam": read_nml("with_complex")}}
    )
    params = collection.extra_fields["namelist:parameters"]
    assert params["echam"]["namelist_echam"]["runctl"]["delta_time"] == 450
    assert "phase" not in params["echam"]["namelist_echam"]["runctl"]


def test_collection_nests_shipped_namelist(collection):
    # A real namelist shipped in esm_tools, read the way the scan layer would.
    amip = f90nml.read(esm_tools.get_namelist_filepath("amip/namelist.amip"))
    add_namelist_collection_extension(collection, {"amip": {"namelist.amip": amip}})
    params = collection.extra_fields["namelist:parameters"]
    assert params["amip"]["namelist_amip"]["namamip"]["runlengthsec"] == 86400
    assert params["amip"]["namelist_amip"]["namamip"]["startyear"] == 1850


# --- add_namelist_item_extension (item-level, all components) ---


def test_item_noop_without_namelists(item):
    add_namelist_item_extension(item, {})
    assert item.properties == {}
    assert item.stac_extensions == []


def test_item_flattens_one_component(item):
    add_namelist_item_extension(item, by_component("echam"))
    assert (
        item.properties["namelist__echam__namelist_echam__runctl__co2vmr"] == 0.000284
    )
    assert NAMELIST_URL in item.stac_extensions


def test_item_covers_all_components(item):
    add_namelist_item_extension(item, by_component("echam", "jsbach"))
    assert (
        item.properties["namelist__echam__namelist_echam__runctl__delta_time"] == 450
    )
    assert (
        item.properties["namelist__jsbach__namelist_jsbach__jsbach_ctl__use_dynveg"]
        is True
    )


# --- schema validation ---


def test_collection_validates_against_namelist_schema(nml_schema):
    exp_metadata = make_exp_metadata(namelists_by_component=by_component("echam"))
    assert_valid(make_collection(exp_metadata), nml_schema)


def test_collection_rejects_malformed_parameter_key(nml_schema):
    # Every nesting level's keys are JSON-path-safe: only [A-Za-z0-9_]. A key
    # carrying a forbidden character (here a ':') must be rejected by
    # propertyNames, at whichever level it appears.
    exp_metadata = make_exp_metadata(namelists_by_component=by_component("echam"))
    col_dict = make_collection(exp_metadata).to_dict()
    col_dict["namelist:parameters"]["echam"]["namelist_echam"]["runctl"][
        "has:a:colon"
    ] = 1  # forbidden character
    with pytest.raises(jsonschema.ValidationError):
        jsonschema.validate(instance=col_dict, schema=nml_schema)


def test_item_rejects_malformed_namelist_key_but_keeps_foreign_props(item, nml_schema):
    # namelist__ keys must be JSON-path-safe; one carrying a ':' is rejected,
    # while foreign core props (datetime, etc.) pass untouched.
    add_namelist_item_extension(item, by_component("echam"))
    # well-formed item (carrying a core datetime prop) validates
    jsonschema.validate(instance=item.to_dict(), schema=nml_schema)
    # a namelist__ key with a forbidden character must be rejected
    bad = item.to_dict()
    bad["properties"]["namelist__echam__bad:key"] = 1  # colon not allowed
    with pytest.raises(jsonschema.ValidationError):
        jsonschema.validate(instance=bad, schema=nml_schema)


# --- schema validation against the full shipped corpus ---


class ShippedNamelist(NamedTuple):
    """A Fortran namelist shipped in esm_tools, parsed for the extension tests."""

    filename: NamelistFilename
    """The namelist's filename, e.g. 'namelist.echam'."""

    namelist: Namelist
    """The parsed f90nml.Namelist."""


_NAMELIST_ROOT = Path(esm_tools.get_namelist_filepath(""))
# 'namelist.*' also matches shell/XML helpers that share the prefix; exclude by suffix.
_NON_NAMELIST_SUFFIXES = {
    ".sh",
    ".xml",
    ".md",
    ".rst",
    ".txt",
    ".json",
    ".yaml",
    ".yml",
    ".py",
    ".pyc",
}


def _shipped_namelist_files() -> list[Path]:
    """Every 'namelist.*' file shipped under esm_tools/namelists, minus obvious non-Fortran ones."""
    if not _NAMELIST_ROOT.is_dir():
        return []
    return [
        p
        for p in sorted(_NAMELIST_ROOT.rglob("namelist.*"))
        if p.is_file()
        and p.suffix not in _NON_NAMELIST_SUFFIXES
        and ".j2" not in p.name
    ]


@pytest.fixture(
    params=_shipped_namelist_files(),
    ids=lambda p: str(p.relative_to(_NAMELIST_ROOT)),
)
def shipped_namelist(request) -> ShippedNamelist:
    """Each shipped Fortran namelist as a ShippedNamelist(filename, namelist).

    Files f90nml cannot parse (jinja-templated, include-based, or otherwise not
    plain Fortran) are skipped, so the fixture yields only genuinely flattenable
    namelists.
    """
    path = request.param
    try:
        return ShippedNamelist(path.name, f90nml.read(str(path)))
    except Exception as exc:  # jinja templates, non-namelist content, parse errors
        pytest.skip(f"f90nml cannot parse {path.relative_to(_NAMELIST_ROOT)}: {exc}")


def test_every_shipped_namelist_flattens_and_validates(
    collection, shipped_namelist, nml_schema
):
    # Flatten every real namelist shipped in esm_tools and confirm the produced collection is
    # schema-valid — the extension must survive the full spread of shipped inputs.
    add_namelist_collection_extension(
        collection, {"comp": {shipped_namelist.filename: shipped_namelist.namelist}}
    )
    assert_valid(collection, nml_schema)


def test_item_validates_against_namelist_schema(nml_schema, temp_nc):
    exp_metadata = make_exp_metadata(namelists_by_component=by_component("echam"))
    assert_valid(make_item(temp_nc, make_file_metadata(), exp_metadata), nml_schema)


def test_arrow_safe_stringifies_only_mixed_kind_lists():
    # A mixed text+number list (f90nml's putrerun = 1,'months','first',0) cannot
    # be a single-typed geoparquet column, so every element is stringified.
    assert _arrow_safe([1, "months", "first", 0]) == ["1", "months", "first", "0"]
    # Homogeneous lists (all numeric / all text) and scalars pass through.
    assert _arrow_safe([1, 2, 3]) == [1, 2, 3]
    assert _arrow_safe([1.0, 2.5]) == [1.0, 2.5]
    assert _arrow_safe(["a", "b"]) == ["a", "b"]
    assert _arrow_safe(0.000284) == 0.000284
    assert _arrow_safe("scalar") == "scalar"
    # None is preserved (a nullable column), not stringified to "None".
    assert _arrow_safe([1, "x", None]) == ["1", "x", None]
