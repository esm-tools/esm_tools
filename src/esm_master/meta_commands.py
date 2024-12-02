import ruamel.yaml as yaml

from .general_stuff import CONFIG_YAML


def get_meta_commands():
    with open(CONFIG_YAML) as f:
        config = yaml.safe_load(f)
    meta_commands = {}
    for key, value in config.items():
        if key.endswith("_meta_command"):
            meta_commands[key.replace("_meta_command", "")] = value
    return meta_commands
