import glob
import os
import sys

import pytest
import yaml

sys.path.append(os.path.join(os.path.dirname(__file__), "helpers"))


@pytest.fixture(
    params=glob.glob(
        "src/esm_tests/resources/last_tested/**/config/*_finished_config.yaml",
        recursive=True,
    )
)
def finished_config(request):
    """Fixture that provides finished_config.yaml files from esm_tests resources.

    Each test using this fixture will run once for each finished_config.yaml file
    found in src/esm_tests/resources/last_tested/**/config/

    The fixture yields a tuple of (config_path, config_data) where:
    - config_path: absolute path to the finished_config.yaml file
    - config_data: loaded YAML data as a Python dict
    """
    config_path = request.param
    with open(config_path, "r") as f:
        config_data = yaml.load(f, Loader=yaml.FullLoader)

    # Use just the filename (without path) as the test ID for better readability
    config_name = os.path.basename(config_path).replace("_finished_config.yaml", "")
    request.node.name = f"{request.node.originalname}[{config_name}]"

    return config_path, config_data
