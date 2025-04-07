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


# @pytest.fixture
# def nested_choose_config():
#     """Nested configuration choose blocks"""
#     return {
#         "nested_choose": {
#             "choose_config": {
#                 "development": {
#                     "debug": True,
#                     "log_level": "debug",
#                     "verbose": True
#                 },
#                 "production": {
#                     "debug": False,
#                     "log_level": "info",
#                     "verbose": False
#                 }
#             }
#         }
#     }


# @pytest.fixture
# def computer_choose_config():
#     """Computer-specific choose blocks"""
#     return {
#         "computer_choose": {
#             "choose_computer.name": {
#                 "levante": {
#                     "partition": "compute",
#                     "cores_per_node": 128
#                 },
#                 "juwels": {
#                     "partition": "batch",
#                     "cores_per_node": 48
#                 }
#             }
#         }
#     }


# @pytest.fixture
# def run_choose_config():
#     """Run configuration with boolean conditions"""
#     return {
#         "run_config": {
#             "choose_general.run_number": {
#                 "1": {
#                     "lresume": False,
#                     "initial_date": "2000-01-01"
#                 },
#                 "2": {
#                     "lresume": True,
#                     "parent_date": "2000-01-01",
#                     "parent_exp_id": "test_1"
#                 }
#             }
#         }
#     }


# @pytest.fixture
# def complex_choose_config():
#     """Complex nested choose blocks"""
#     return {
#         "complex_choose": {
#             "choose_version": {
#                 "3.1": {
#                     "choose_resolution": {
#                         "low": {
#                             "grid": "TCO319L137",
#                             "timestep": 900
#                         },
#                         "high": {
#                             "grid": "TCO639L137",
#                             "timestep": 450
#                         }
#                     },
#                     "choose_experiment": {
#                         "DART": {
#                             "assimilation": True,
#                             "analysis_step": 24
#                         },
#                         "SO3": {
#                             "assimilation": False
#                         }
#                     }
#                 },
#                 "3.2": {
#                     "choose_resolution": {
#                         "low": {
#                             "grid": "TCO319L137",
#                             "timestep": 720
#                         },
#                         "high": {
#                             "grid": "TCO639L137",
#                             "timestep": 360
#                         }
#                     }
#                 }
#             }
#         }
#     }
