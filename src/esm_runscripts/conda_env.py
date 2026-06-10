import os
import yaml

from pathlib import Path

def get_conda_info():
    conda_env = os.getenv("CONDA_PREFIX")
    conda_exe = os.getenv("CONDA_EXE")
    conda_root = str(Path(conda_exe).parent.parent) if conda_exe else None

    return conda_env, conda_root

def set_launched_with_conda(config):
    conda_env, _ = get_conda_info()
    config["computer"]["launched_with_conda"] = True if conda_env else False

    return config

def get_conda_info_from_file(config):
    conda_info_file = f'{config["general"]["thisrun_config_dir"]}/conda_info.yaml'
    if not os.path.isfile(conda_info_file):
        return None, None

    with open(conda_info_file, "r") as f:
        conda_info = yaml.safe_load(f)

    return conda_info.get("conda_env"), conda_info.get("conda_root")

def write_conda_info_file(config):
    conda_env, conda_root = get_conda_info()

    if not conda_env:
        return config

    conda_info = {"conda_env": conda_env, "conda_root": conda_root}

    conda_info_file = f'{config["general"]["thisrun_config_dir"]}/conda_info.yaml'
    with open(conda_info_file, "w") as f:
        yaml.dump(conda_info, f)
