"""Tests for component handling functionality in esm_parser"""

import pytest
from esm_parser import get_components


def test_get_components():
    """Test get_components function"""
    config = {
        "general": {
            "valid_model_names": ["model1", "model2"],
            "system_components": ["general", "dask"]
        }
    }

    # Test with system components
    components = get_components(config, include_system=True)
    assert set(components) == {"model1", "model2", "general", "dask"}

    # Test without system components
    components = get_components(config, include_system=False)
    assert set(components) == {"model1", "model2"}

    # Test with missing system_components key
    config = {
        "general": {
            "valid_model_names": ["model1", "model2"]
        }
    }
    components = get_components(config, include_system=True)
    assert set(components) == {"model1", "model2", "general"}
