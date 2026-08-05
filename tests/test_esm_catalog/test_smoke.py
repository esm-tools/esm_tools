"""Smoke tests: the package imports and the CLI runs."""

from __future__ import annotations

import pytest
from click.testing import CliRunner

import esm_catalog
from esm_catalog.cli import main
from esm_catalog.registry import EXTENSION_URLS, Extension
from esm_catalog.stac_ext import load_schema


def test_package_has_version():
    assert isinstance(esm_catalog.__version__, str)
    assert esm_catalog.__version__


def test_every_extension_has_a_url():
    # Extension is the single source of truth; every member must map to a URL.
    assert set(EXTENSION_URLS) == set(Extension)


@pytest.mark.parametrize("name", ["datacube", "contacts"])
def test_load_schema_rejects_remote_extensions(name):
    # datacube/contacts schemas are hosted on stac-extensions.github.io (no
    # local copy); load_schema must reject them, not hunt for a missing file.
    with pytest.raises(ValueError, match="hosted remotely"):
        load_schema(name)


def test_cli_version_runs():
    result = CliRunner().invoke(main, ["--version"])
    assert result.exit_code == 0
    assert "esm-catalog" in result.output
