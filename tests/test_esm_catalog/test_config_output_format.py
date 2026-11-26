"""Unit tests for Settings.output_format (default for 'esm-catalog get')."""

from esm_catalog.config import Settings


def test_output_format_defaults_to_pretty():
    assert Settings().output_format == "pretty"


def test_output_format_overridable_via_env(monkeypatch):
    monkeypatch.setenv("ESM_CATALOG_OUTPUT_FORMAT", "json")
    assert Settings().output_format == "json"
