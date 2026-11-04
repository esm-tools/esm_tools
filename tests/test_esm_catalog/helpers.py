"""Shared, importable test helpers for the esm_catalog STAC tests.

Parameterized builders and an assertion helper — plain functions so they
compose inline with per-call arguments, e.g.

    make_item(temp_nc, make_file_metadata(), make_exp_metadata())
    make_item(temp_nc, make_file_metadata(dimensions=dims), make_exp_metadata(paleo_config=cfg))

Zero-arg object builders (item, collection) live as fixtures in conftest.py
instead — they take no arguments, so injection fits them cleanly.
"""

from __future__ import annotations

import json
from datetime import datetime, timezone
from pathlib import Path
from urllib.error import URLError
from urllib.request import urlopen

import jsonschema
import pytest
from referencing import Registry, Resource
from referencing.jsonschema import DRAFT7

from esm_catalog.models import ExperimentMetadata
from esm_catalog.types import FileMetadata


def make_exp_metadata(**kwargs) -> ExperimentMetadata:
    """An ExperimentMetadata with sensible defaults; override any field via kwargs."""
    defaults = {"experiment_id": "exp-alpha", "experiment_path": Path("/exp/alpha")}
    return ExperimentMetadata(**{**defaults, **kwargs})


def make_file_metadata(**kwargs) -> FileMetadata:
    """Default scan metadata for one file; override any key via kwargs."""
    return {
        "variable": "temp",
        "component": "echam",
        "format": "netcdf",
        "datetime_start": datetime(2000, 1, 1, tzinfo=timezone.utc),
        "datetime_end": datetime(2000, 1, 1, tzinfo=timezone.utc),
        **kwargs,
    }


def assert_valid(obj, schema) -> None:
    """Assert *obj*.to_dict() validates against *schema* (a loaded JSON Schema)."""
    jsonschema.validate(instance=obj.to_dict(), schema=schema)


def _fetch_schema_resource(uri: str) -> Resource:
    """Fetch a JSON Schema over HTTP as a referencing Resource (for $ref resolution)."""
    with urlopen(uri, timeout=30) as response:
        return Resource.from_contents(json.load(response), default_specification=DRAFT7)


def assert_valid_remote(obj, schema_url: str) -> None:
    """Validate *obj*.to_dict() against the schema fetched live from *schema_url*.

    Used for extensions whose schema is hosted upstream (datacube, contacts) and
    not vendored locally. External ``$ref``s are resolved over HTTP on demand.
    Skips the test if the network is unreachable rather than failing.
    """
    try:
        schema = _fetch_schema_resource(schema_url).contents
    except URLError as exc:
        pytest.skip(f"cannot reach remote schema {schema_url}: {exc}")
    registry = Registry(retrieve=_fetch_schema_resource)
    jsonschema.Draft7Validator(schema, registry=registry).validate(obj.to_dict())
