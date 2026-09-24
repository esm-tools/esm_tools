"""Tests for the cmip6 STAC extension (real DRS facets, config-declared)."""

from __future__ import annotations

from esm_catalog.cmip6 import (
    Cmip6Config,
    add_cmip6_collection_extension,
    add_cmip6_item_extension,
)
from esm_catalog.collection import make_collection
from esm_catalog.item import make_item
from esm_catalog.registry import EXTENSION_URLS

from .helpers import assert_valid_remote, make_exp_metadata, make_file_metadata

# item, collection come from tests/test_esm_catalog/conftest.py

CMIP6_URL = EXTENSION_URLS["cmip6"]


def _config(**kwargs):
    # Real, CMIP6-CV-registered values (verified against the live upstream
    # schema, not invented) -- "AWI-CM-1-1-MR" not "AWI-CM-3", since AWI-CM3
    # itself has no registered source_id. mip_era is the literal string
    # "CMIP6" (a schema `const`) -- distinct from esgvoc's CV term id for
    # the same round, which is the lowercase "cmip6".
    defaults = {
        "activity_id": "PMIP",
        "institution_id": "AWI",
        "source_id": "AWI-CM-1-1-MR",
        "experiment_id": "piControl",
        "mip_era": "CMIP6",
    }
    return Cmip6Config(**{**defaults, **kwargs})


# --- add_cmip6_item_extension ---


def test_noop_without_config(item):
    add_cmip6_item_extension(item)
    assert "cmip6:experiment_id" not in item.properties
    assert item.stac_extensions == []


def test_noop_with_config_declaring_nothing(item):
    # A Cmip6Config with every field None is the same as no config at all --
    # never fabricate a facet just because the section exists.
    add_cmip6_item_extension(item, cmip6_config=Cmip6Config())
    assert item.properties == {}
    assert item.stac_extensions == []


def test_facets_set_on_item(item):
    add_cmip6_item_extension(item, cmip6_config=_config())
    assert item.properties["cmip6:activity_id"] == "PMIP"
    assert item.properties["cmip6:institution_id"] == "AWI"
    assert item.properties["cmip6:source_id"] == "AWI-CM-1-1-MR"
    assert item.properties["cmip6:experiment_id"] == "piControl"
    assert item.properties["cmip6:mip_era"] == "CMIP6"
    assert CMIP6_URL in item.stac_extensions


def test_partial_config_only_sets_declared_facets(item):
    add_cmip6_item_extension(item, cmip6_config=Cmip6Config(experiment_id="historical"))
    assert item.properties["cmip6:experiment_id"] == "historical"
    assert "cmip6:activity_id" not in item.properties
    assert "cmip6:institution_id" not in item.properties


def test_url_appended_once(item):
    add_cmip6_item_extension(item, cmip6_config=_config())
    add_cmip6_item_extension(item, cmip6_config=_config())
    assert item.stac_extensions.count(CMIP6_URL) == 1


# --- add_cmip6_collection_extension ---


def test_collection_noop_without_config(collection):
    add_cmip6_collection_extension(collection)
    assert not any(k.startswith("cmip6:") for k in collection.summaries.lists)
    assert collection.stac_extensions == []


def test_collection_summarizes_facets(collection):
    add_cmip6_collection_extension(collection, cmip6_config=_config())
    assert collection.summaries.get_list("cmip6:activity_id") == ["PMIP"]
    assert collection.summaries.get_list("cmip6:experiment_id") == ["piControl"]
    assert CMIP6_URL in collection.stac_extensions


# --- wiring through make_item / make_collection ---


def test_make_item_without_cmip6_config_sets_no_facets(temp_nc):
    item = make_item(temp_nc, make_file_metadata(), make_exp_metadata())
    assert not any(k.startswith("cmip6:") for k in item.properties)


def test_make_item_with_cmip6_config(temp_nc):
    exp_metadata = make_exp_metadata(cmip6_config=_config())
    item = make_item(temp_nc, make_file_metadata(), exp_metadata)
    assert item.properties["cmip6:experiment_id"] == "piControl"
    assert CMIP6_URL in item.stac_extensions


def test_make_collection_with_cmip6_config():
    exp_metadata = make_exp_metadata(cmip6_config=_config())
    collection = make_collection(exp_metadata)
    assert collection.summaries.get_list("cmip6:source_id") == ["AWI-CM-1-1-MR"]


# --- conformance against the real, published upstream schema ---


def test_item_validates_against_real_upstream_schema(item):
    """The real stac-extensions.github.io/cmip6 schema, fetched live.

    Skips if the network is unreachable rather than failing -- this is a
    conformance check against upstream, not a test of local logic.
    """
    add_cmip6_item_extension(item, cmip6_config=_config())
    assert_valid_remote(item, CMIP6_URL)
