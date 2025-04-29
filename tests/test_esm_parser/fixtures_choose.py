"""
Fixtures for testing choose functionality in ESM-Tools
"""
import pytest


@pytest.fixture
def simple_choose_config():
    """Basic choose block configuration"""
    import yaml

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
def conflict_choose_config():
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
