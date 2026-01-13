#!/usr/bin/env python
"""Test and demonstrate the finished_config fixture.

This module contains tests for the finished_config fixture located in tests/conftest.py
(The fixture is defined at the root level so it's available to all tests).

The finished_config fixture automatically parametrizes tests to run once for each
finished_config.yaml file found in src/esm_tests/resources/last_tested/**/config/

Usage in tests (from anywhere in the tests/ directory):
    def test_my_feature(finished_config):
        config_path, config_data = finished_config
        # config_data is the loaded YAML as a Python dict
        # Test will run once for each finished_config.yaml file
        assert 'general' in config_data

See tests/examples/test_example_finished_config.py for more detailed examples.
"""

import os


def test_finished_config_fixture(finished_config):
    """Test that the finished_config fixture loads configs correctly."""
    config_path, config_data = finished_config

    # Verify we got a tuple with path and data
    assert isinstance(config_path, str)
    assert isinstance(config_data, dict)

    # Verify the path exists and is a YAML file
    assert config_path.endswith("_finished_config.yaml")
    assert os.path.exists(config_path)

    # Verify the config data has expected structure
    # Most finished configs should have at least one model component
    assert len(config_data) > 0

    # Each config should have some basic keys
    # (This will vary by config, but let's check for common patterns)
    top_level_keys = set(config_data.keys())
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

    # Check if any model component is present
    has_model_component = any(key in model_components for key in top_level_keys)
    assert has_model_component, (
        f"Config {config_path} doesn't seem to have any recognized model components. Keys: {top_level_keys}"
    )


def test_example_usage(finished_config):
    """Example of how tests can use finished_config data."""
    config_path, config_data = finished_config

    # Example: Check if config has certain required keys
    # This is just a demonstration - actual tests would check specific requirements

    # Most configs should have a 'general' section
    assert "general" in config_data, f"Config {config_path} missing 'general' section"

    # And a 'computer' section
    assert "computer" in config_data, f"Config {config_path} missing 'computer' section"

    # The config should be a processed configuration with resolved values
    # (This is a basic check that it looks like a finished config)
    assert isinstance(config_data["general"], dict)
