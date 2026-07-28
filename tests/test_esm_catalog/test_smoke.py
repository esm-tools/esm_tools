"""Smoke tests: the package imports and the CLI runs."""

from __future__ import annotations

from click.testing import CliRunner

import esm_catalog
from esm_catalog.cli import main


def test_package_has_version():
    assert isinstance(esm_catalog.__version__, str)
    assert esm_catalog.__version__


def test_cli_version_runs():
    result = CliRunner().invoke(main, ["--version"])
    assert result.exit_code == 0
    assert "esm-catalog" in result.output
