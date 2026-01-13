#!/usr/bin/env python

"""Tests for `esm_tools` package."""

import unittest


class TestEsm_tools(unittest.TestCase):
    """Tests for `esm_tools` package."""

    def setUp(self):
        """Set up test fixtures, if any."""

    def tearDown(self):
        """Tear down test fixtures, if any."""

    def test_000_something(self):
        """Test something."""


def test_finished_config_fixture_example(finished_config):
    """Example test demonstrating usage of the finished_config fixture.

    This test runs once for each finished_config.yaml file found in
    src/esm_tests/resources/last_tested/**/config/ (98 times total).

    The finished_config fixture provides:
    - config_path: str, absolute path to the finished_config.yaml file
    - config_data: dict, loaded YAML data as a Python dictionary
    """
    config_path, config_data = finished_config

    # Basic validation that the config was loaded properly
    assert isinstance(config_path, str)
    assert isinstance(config_data, dict)
    assert len(config_data) > 0

    # Check that it's a proper ESM configuration with expected sections
    assert "general" in config_data
    assert "computer" in config_data

    # Verify the config path is valid
    import os

    assert os.path.exists(config_path)
    assert config_path.endswith("_finished_config.yaml")

    # Example: Check that the config has at least one model component
    model_components = {
        "echam",
        "fesom",
        "awiesm",
        "oifs",
        "nemo",
        "awicm",
        "foci",
        "focioifs",
        "oifsamip",
        "fesom-recom",
        "awiesm3",
        "vilma",
        "pism",
    }
    has_model_component = any(key in model_components for key in config_data.keys())
    assert has_model_component, (
        f"Config should have at least one model component: {list(config_data.keys())}"
    )
