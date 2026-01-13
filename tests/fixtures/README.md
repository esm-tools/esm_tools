# ESM Tools Test Fixtures

This directory contains specialized test fixtures for ESM Tools.

## Available Fixtures

### `finished_config` Fixture

**Location**: Defined in `tests/conftest.py` (available globally)

**Purpose**: Provides access to all `finished_config.yaml` files from ESM test resources.

**Usage**:
```python
def test_my_feature(finished_config):
    config_path, config_data = finished_config
    # Test runs once for each of ~98 finished_config.yaml files
    assert 'general' in config_data
```

**Returns**: Tuple of `(config_path, config_data)` where:
- `config_path`: Absolute path to the YAML file
- `config_data`: Loaded YAML content as a Python dictionary

## Examples

See `tests/examples/test_example_finished_config.py` for detailed usage examples.

## Organization

- `tests/conftest.py`: Global fixtures available to all tests
- `tests/fixtures/`: Specialized fixture tests and organization
- `tests/examples/`: Example code demonstrating fixture usage