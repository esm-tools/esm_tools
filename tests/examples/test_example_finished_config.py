#!/usr/bin/env python
"""Example demonstrating how to use the finished_config fixture.

This is an example test that shows how to use the finished_config fixture
to iterate through all finished_config.yaml files in the ESM tests resources.

Run with:
    pytest tests/examples/test_example_finished_config.py -v

Note: This file is in tests/examples/ as a documentation example.
For actual test suites, put tests in appropriate test directories.
"""

import pytest


def test_example_finished_config_usage(finished_config):
    """Example test showing how to use the finished_config fixture.

    This test will run once for each finished_config.yaml file found in
    src/esm_tests/resources/last_tested/**/config/ (98 times total).

    The finished_config fixture provides a tuple of:
    - config_path: str, path to the YAML file
    - config_data: dict, loaded YAML content
    """
    config_path, config_data = finished_config

    # Example 1: Basic validation
    assert isinstance(config_data, dict)
    assert len(config_data) > 0
    assert "general" in config_data
    assert "computer" in config_data

    # Example 2: Check for model components
    model_components = {"echam", "fesom", "awiesm", "oifs", "nemo", "awicm"}
    found_components = [comp for comp in model_components if comp in config_data]
    assert (
        len(found_components) > 0
    ), f"No known model components found in {config_path}"

    # Example 3: Check config structure (this will vary by model)
    if "echam" in config_data:
        assert "compile_infos" in config_data["echam"]
        assert "available_versions" in config_data["echam"]["compile_infos"]

    # Example 4: You could add specific tests for your feature here
    # For instance, if testing a new ESM feature:
    # assert 'my_new_feature' in config_data['general']
    # assert config_data['general']['my_new_feature'] is not None

    # This test passes - it just demonstrates the fixture usage
    print(f"✓ Successfully loaded and validated config: {config_path}")


def test_example_config_inspection(finished_config):
    """Example showing how to inspect config contents."""
    config_path, config_data = finished_config

    # Get basic info about the config
    config_name = config_path.split("/")[-1].replace("_finished_config.yaml", "")
    top_level_keys = list(config_data.keys())

    # Count model components
    model_keys = [
        k
        for k in top_level_keys
        if k
        in {
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
    ]

    # Example assertion: configs should have at least one model component
    assert (
        len(model_keys) > 0
    ), f"Config {config_name} has no model components: {top_level_keys}"

    # Example assertion: general section should exist and have basic structure
    assert "general" in config_data
    assert isinstance(config_data["general"], dict)

    # You could add more specific validations here based on your needs
    print(
        f"✓ Config {config_name} has {len(model_keys)} model components: {model_keys}"
    )


def test_specific_config_awicm3_levante():
    """Example: Test against a specific configuration (AWICM3 on Levante).

    This shows how to test against a single specific config instead of all 98.
    Useful for development and debugging specific configurations.
    """
    import glob

    import yaml

    # Find the specific config file
    config_pattern = "src/esm_tests/resources/last_tested/levante/run/awicm3/awicm3-v3.4-TCO95L91-CORE2_initial/config/awicm3-v3.4-TCO95L91-CORE2_initial_finished_config.yaml"
    config_files = glob.glob(config_pattern)

    assert (
        len(config_files) == 1
    ), f"Expected 1 config file, found {len(config_files)}: {config_files}"

    config_path = config_files[0]

    # Load the config
    with open(config_path, "r") as f:
        config_data = yaml.load(f, Loader=yaml.FullLoader)

    # Test specific properties of AWICM3 on Levante
    assert "computer" in config_data
    assert config_data["computer"]["name"] == "levante"

    # Check for expected model components in AWICM3 (coupled model)
    awicm3_components = {"fesom", "oifs", "xios", "oasis3mct"}
    found_components = set(config_data.keys()) & awicm3_components
    assert (
        len(found_components) >= 3
    ), f"AWICM3 should have multiple components, found: {found_components}"

    # Check for specific model configurations
    assert "fesom" in config_data, "AWICM3 should include FESOM ocean model"
    assert "oifs" in config_data, "AWICM3 should include OpenIFS atmosphere model"
    assert "xios" in config_data, "AWICM3 should include XIOS for I/O"

    print(f"✓ Successfully validated AWICM3 config: {config_path}")


def test_config_by_pattern(finished_config):
    """Example: Test only configs matching a specific pattern.

    This shows how to conditionally run tests based on config filename patterns.
    Useful for testing specific model types, computers, or experiment types.
    """
    config_path, config_data = finished_config

    # Only run this test for AWICM3 configurations on Levante
    if "awicm3" in config_path.lower() and "levante" in config_path.lower():
        # Perform AWICM3-specific tests
        assert config_data["computer"]["name"] == "levante"

        # Check for coupled model components (AWICM3 = FESOM + OpenIFS)
        awicm3_components = {"fesom", "oifs", "xios"}
        present_components = awicm3_components & set(config_data.keys())
        assert (
            len(present_components) >= 2
        ), f"AWICM3 should have coupled components: {present_components}"

        print(f"✓ AWICM3 on Levante config validated: {config_path}")
    else:
        # Skip test for non-matching configs
        pytest.skip(f"Skipping non-AWICM3-Levante config: {config_path}")


def test_multiple_specific_configs():
    """Example: Test against multiple specific configurations manually.

    This shows how to test several specific configs without parametrization.
    """
    import yaml

    # Define specific configs to test
    test_configs = [
        "src/esm_tests/resources/last_tested/levante/run/awicm3/awicm3-v3.4-TCO95L91-CORE2_initial/config/awicm3-v3.4-TCO95L91-CORE2_initial_finished_config.yaml",
        "src/esm_tests/resources/last_tested/ollie/run/awiesm/pi/config/pi_finished_config.yaml",
        "src/esm_tests/resources/last_tested/albedo/run/fesom/fesom2.1-initial-monthly/config/fesom2.1-initial-monthly_finished_config.yaml",
    ]

    for config_path in test_configs:
        with open(config_path, "r") as f:
            config_data = yaml.load(f, Loader=yaml.FullLoader)

        # Basic validation for each config
        assert "general" in config_data
        assert "computer" in config_data

        # Config-specific checks
        config_name = config_path.split("/")[-1].replace("_finished_config.yaml", "")

        if "awicm3" in config_name:
            assert "awicm3" in config_data
        elif "awiesm" in config_name:
            assert "awiesm" in config_data
        elif "fesom" in config_name:
            assert "fesom" in config_data

        print(f"✓ Validated specific config: {config_name}")


# Pytest parametrization for specific configs
SPECIFIC_CONFIGS = [
    "src/esm_tests/resources/last_tested/levante/run/awicm3/awicm3-v3.4-TCO95L91-CORE2_initial/config/awicm3-v3.4-TCO95L91-CORE2_initial_finished_config.yaml",
    "src/esm_tests/resources/last_tested/ollie/run/awiesm/pi/config/pi_finished_config.yaml",
]


@pytest.mark.parametrize("config_path", SPECIFIC_CONFIGS)
def test_parametrized_specific_configs(config_path):
    """Example: Use pytest parametrization for specific configs.

    This allows running the same test against multiple specific configs.
    """
    import yaml

    with open(config_path, "r") as f:
        config_data = yaml.load(f, Loader=yaml.FullLoader)

    # Common validations for all specified configs
    assert "general" in config_data
    assert "computer" in config_data
    assert len(config_data) > 0

    # Extract config name for reporting
    config_name = config_path.split("/")[-1].replace("_finished_config.yaml", "")

    print(f"✓ Parametrized test passed for: {config_name}")
