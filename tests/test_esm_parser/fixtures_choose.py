"""
Fixtures for testing choose functionality in ESM-Tools
"""

import pytest
import yaml

from esm_parser import DictWithProvenance, Provenance


@pytest.fixture
def simple_choose_config():
    """Basic choose block configuration"""

    simple_choose_yaml = """
    general:
        version: 3.1.1
        choose_version:
            3.2.1:
                major_version: 3.2
            3.1.1:
                major_version: 3.1
    """
    return yaml.safe_load(simple_choose_yaml)


@pytest.fixture
def choose_overwrites_default():
    """Choose block that overwrites default values"""

    choose_overwrites_yaml = """
    general:
        version: 3.1
        scenario: pi
        choose_scenario:
            pi:
                version: 3.2
    """

    choose_overwrites = yaml.safe_load(choose_overwrites_yaml)
    choose_overwrites = DictWithProvenance(choose_overwrites, {})

    choose_overwrites["general"]["version"].provenance = Provenance(
        {"category": "runscript"}
    )
    choose_overwrites["general"]["choose_scenario"]["pi"]["version"].provenance = (
        Provenance({"category": "runscript"})
    )

    return choose_overwrites

@pytest.fixture
def conflict_choose_config():
    """Conflict between nested choose blocks"""

    conflict_choose_yaml = """
    general:
        version: 3.2.1
        scenario: pi
        choose_version:
            3.2.1:
                major_version: 3.2
        choose_scenario:
            pi:
                major_version: 3.3
    """
    return yaml.safe_load(conflict_choose_yaml)


@pytest.fixture
def no_conflict_in_nested_choose_config():
    """Conflict between nested choose blocks"""
    import yaml

    conflict_choose_yaml = """
    general:
        version: 3.2.1
        scenario: pi
        choose_version:
            3.2.1:
                major_version: 3.2
                choose_scenario:
                    pi:
                        major_version: 3.3
    """
    return yaml.safe_load(conflict_choose_yaml)
