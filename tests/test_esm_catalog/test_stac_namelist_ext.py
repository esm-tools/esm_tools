"""Tests for the namelist STAC extension.

Namelist inputs are real Fortran namelists read from data/namelists/*.nml — the
same f90nml.Namelist objects the scan layer produces — assembled into the
extension's shapes by two small builders:

    component_namelists("echam", "jsbach")  -> one component's files
    by_component("echam", "jsbach")         -> all components, keyed by component
"""

from __future__ import annotations

from datetime import datetime, timezone
from pathlib import Path
from typing import NamedTuple

import f90nml
import jsonschema
import pytest

import esm_tools
from esm_catalog.collection import make_collection
from esm_catalog.item import make_item
from esm_catalog.namelist import (Namelist, NamelistFilename, _is_queryable,
                                  add_namelist_collection_extension,
                                  add_namelist_item_extension)
from esm_catalog.registry import EXTENSION_URLS
from esm_catalog.stac_ext import load_schema

# bare_collection, bare_item, make_ctx come from tests/test_esm_catalog/conftest.py

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


def test_collection_noop_without_namelists(bare_collection):
    add_namelist_collection_extension(bare_collection, {})
    assert "nml:files" not in bare_collection.extra_fields
    assert bare_collection.stac_extensions == []


def test_collection_flattens_parameters(bare_collection):
    add_namelist_collection_extension(bare_collection, component_namelists("echam"))
    params = bare_collection.extra_fields["nml:parameters"]
    assert params["namelist.echam:runctl:delta_time"] == 450
    assert params["namelist.echam:runctl:lcouple"] is True


def test_collection_lists_files_and_groups(bare_collection):
    add_namelist_collection_extension(
        bare_collection, component_namelists("echam", "jsbach")
    )
    assert bare_collection.extra_fields["nml:files"] == [
        "namelist.echam",
        "namelist.jsbach",
    ]
    assert bare_collection.extra_fields["nml:groups"] == ["jsbach_ctl", "runctl"]
    assert NAMELIST_URL in bare_collection.stac_extensions


def test_collection_url_appended_once(bare_collection):
    add_namelist_collection_extension(bare_collection, component_namelists("echam"))
    add_namelist_collection_extension(bare_collection, component_namelists("echam"))
    assert bare_collection.stac_extensions.count(NAMELIST_URL) == 1


def test_collection_indexes_repeated_group(bare_collection):
    # A group repeated in a file (an f90nml Cogroup) must not collapse onto one
    # key; each occurrence gets an [index] array suffix, so both values survive.
    add_namelist_collection_extension(
        bare_collection, {"namelist.bgc": read_nml("repeated_group")}
    )
    params = bare_collection.extra_fields["nml:parameters"]
    assert params["namelist.bgc:rep[0]:x"] == 1
    assert params["namelist.bgc:rep[1]:x"] == 2
    assert params["namelist.bgc:solo:y"] == 9


def test_collection_drops_non_json_scalar(bare_collection):
    # f90nml yields a Fortran complex; it is not JSON-serializable and must be
    # skipped, not emitted (which would crash to_dict/json.dumps later).
    add_namelist_collection_extension(
        bare_collection, {"namelist.echam": read_nml("with_complex")}
    )
    params = bare_collection.extra_fields["nml:parameters"]
    assert params["namelist.echam:runctl:delta_time"] == 450
    assert "namelist.echam:runctl:phase" not in params


def test_collection_flattens_shipped_namelist(bare_collection):
    # A real namelist shipped in esm_tools, read the way the scan layer would.
    amip = f90nml.read(esm_tools.get_namelist_filepath("amip/namelist.amip"))
    add_namelist_collection_extension(bare_collection, {"namelist.amip": amip})
    params = bare_collection.extra_fields["nml:parameters"]
    assert params["namelist.amip:namamip:runlengthsec"] == 86400
    assert params["namelist.amip:namamip:startyear"] == 1850


# --- add_namelist_item_extension (item-level, all components) ---


def test_item_noop_without_namelists(bare_item):
    add_namelist_item_extension(bare_item, {})
    assert bare_item.properties == {}
    assert bare_item.stac_extensions == []


def test_item_flattens_one_component(bare_item):
    add_namelist_item_extension(bare_item, by_component("echam"))
    assert bare_item.properties["nml:echam:namelist.echam:runctl:co2vmr"] == 0.000284
    assert NAMELIST_URL in bare_item.stac_extensions


def test_item_covers_all_components(bare_item):
    add_namelist_item_extension(bare_item, by_component("echam", "jsbach"))
    assert bare_item.properties["nml:echam:namelist.echam:runctl:delta_time"] == 450
    assert (
        bare_item.properties["nml:jsbach:namelist.jsbach:jsbach_ctl:use_dynveg"] is True
    )


# --- schema validation ---


def test_collection_validates_against_namelist_schema(make_ctx, nml_schema):
    ctx = make_ctx(namelists_by_component=by_component("echam"))
    jsonschema.validate(instance=make_collection(ctx).to_dict(), schema=nml_schema)


def test_collection_rejects_malformed_parameter_key(make_ctx, nml_schema):
    # nml:parameters keys must be exactly 'file:group:key' (three colon-separated
    # segments); the schema's propertyNames rule must reject anything else.
    ctx = make_ctx(namelists_by_component=by_component("echam"))
    col_dict = make_collection(ctx).to_dict()
    col_dict["nml:parameters"]["missing_group_and_key"] = 1  # only one segment
    with pytest.raises(jsonschema.ValidationError):
        jsonschema.validate(instance=col_dict, schema=nml_schema)


def test_item_rejects_malformed_nml_key_but_keeps_foreign_props(bare_item, nml_schema):
    # nml: keys must be nml:component:file:group:key; a short nml: key is
    # rejected, while foreign core props (datetime, etc.) pass untouched.
    add_namelist_item_extension(bare_item, by_component("echam"))
    # well-formed item (carrying a core datetime prop) validates
    jsonschema.validate(instance=bare_item.to_dict(), schema=nml_schema)
    # a truncated nml: key must be rejected
    bad = bare_item.to_dict()
    bad["properties"]["nml:echam:runctl"] = 1  # missing file+key segments
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
    bare_collection, shipped_namelist, nml_schema
):
    # Flatten every real namelist we ship and confirm the produced collection is
    # schema-valid — the extension must survive the full spread of shipped inputs.
    add_namelist_collection_extension(
        bare_collection, {shipped_namelist.filename: shipped_namelist.namelist}
    )
    jsonschema.validate(instance=bare_collection.to_dict(), schema=nml_schema)


def test_item_validates_against_namelist_schema(make_ctx, nml_schema, tmp_path):
    f = tmp_path / "temp.nc"
    f.write_bytes(b"x")
    ctx = make_ctx(namelists_by_component=by_component("echam"))
    metadata = {
        "variable": "temp",
        "format": "netcdf",
        "datetime_start": datetime(2000, 1, 1, tzinfo=timezone.utc),
        "datetime_end": datetime(2000, 1, 1, tzinfo=timezone.utc),
    }
    jsonschema.validate(
        instance=make_item(f, metadata, ctx).to_dict(), schema=nml_schema
    )
