"""
Integration tests for yaml section validation.

These tests exercise the full pipeline:
  yaml_file_to_dict  →  general.sections populated
  validate_config_sections  →  error raised if rogue top-level keys found
"""

import pytest

from esm_parser.esm_parser import validate_config_sections
from esm_parser.yaml_to_dict import yaml_file_to_dict


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def write_yaml(tmp_path, name, content):
    """Write *content* to a .yaml file inside *tmp_path* and return its path (no extension)."""
    path = tmp_path / name
    path.write_text(content)
    return str(path.with_suffix(""))  # yaml_file_to_dict appends the extension itself


def make_valid_keys(model_names=(), setup_names=(), system_components=None):
    """Build the general dict that validate_config_sections reads."""
    return {
        "valid_model_names": list(model_names),
        "valid_setup_names": list(setup_names),
        "system_components": list(system_components or ["general", "dask"]),
    }


# ---------------------------------------------------------------------------
# yaml_file_to_dict: sections tracking
# ---------------------------------------------------------------------------

def test_yaml_file_to_dict_records_root_keys_in_sections(tmp_path):
    """
    Every root-level key (except 'general') is recorded in general.sections
    so that validate_config_sections can later name the source file.
    """
    yaml_path = write_yaml(tmp_path, "mymodel.yaml", "fesom:\n  version: 2.0\n")

    result = yaml_file_to_dict(yaml_path)

    assert "fesom" in result["general"]["sections"]
    assert result["general"]["sections"]["fesom"].endswith("mymodel.yaml")


def test_yaml_file_to_dict_multiple_root_keys_all_recorded(tmp_path):
    """All root-level keys from one file are recorded, not just the first one."""
    yaml_path = write_yaml(tmp_path, "multi.yaml", "fesom:\n  a: 1\ncomputer:\n  b: 2\n")

    result = yaml_file_to_dict(yaml_path)

    sections = result["general"]["sections"]
    assert "fesom" in sections
    assert "computer" in sections


# ---------------------------------------------------------------------------
# validate_config_sections: valid config passes silently
# ---------------------------------------------------------------------------

def test_valid_config_passes(tmp_path):
    """A config where all top-level keys are known sections raises no error."""
    yaml_path = write_yaml(tmp_path, "fesom.yaml", "fesom:\n  version: 2.0\n")
    loaded = yaml_file_to_dict(yaml_path)

    loaded["general"].update(make_valid_keys(model_names=["fesom"]))

    # Must not raise
    validate_config_sections(loaded)


# ---------------------------------------------------------------------------
# validate_config_sections: rogue key triggers a clear error
# ---------------------------------------------------------------------------

def test_rogue_key_triggers_error(tmp_path):
    """A top-level key that is not a known section causes SystemExit."""
    yaml_path = write_yaml(tmp_path, "bad.yaml", "rogue_key:\n  something: true\n")
    loaded = yaml_file_to_dict(yaml_path)

    loaded["general"].update(make_valid_keys(model_names=["fesom"]))

    with pytest.raises(SystemExit):
        validate_config_sections(loaded)


def test_error_message_names_source_file(tmp_path):
    """The error output identifies both the bad key and the file it came from."""
    from utils import Capturing

    yaml_path = write_yaml(tmp_path, "bad_further_reading.yaml", "rogue_key:\n  x: 1\n")
    loaded = yaml_file_to_dict(yaml_path)
    loaded["general"].update(make_valid_keys(model_names=["fesom"]))

    with Capturing() as output:
        with pytest.raises(SystemExit):
            validate_config_sections(loaded)

    combined = "\n".join(output)
    assert "rogue_key" in combined
    assert "bad_further_reading.yaml" in combined
