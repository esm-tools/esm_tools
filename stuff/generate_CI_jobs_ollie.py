#!/usr/bin/env python3
from esm_parser.yaml_to_dict import yaml_file_to_dict
import yaml

import os
import sys


def main():
    workflow = yaml_file_to_dict("../.github/workflows/CI-Ollie.yml.example")
    workflow["on"] = workflow[True]
    del workflow[True]

    install_as_user = workflow['jobs']['install_as_user']
    install_model = workflow['jobs']['install_models']
    del workflow['jobs']['install_as_user']
    del install_model['needs']
    steps = install_as_user['steps']
    for idx, step_config in enumerate(steps):
        if step_config.get("name") == "Set up environment":
            break
    generate_env_step = {"name": "Generate virtualenv", "run": "python -m venv env\nsource env/bin/activate\nwhich python\nwhich pip\necho $PATH"}
    steps.insert(idx+1, generate_env_step)
    install_model['steps'] = steps + install_model['steps']
    for setup_yaml in os.listdir("../configs/esm_master/setups/"):
        setup_name = setup_yaml.replace(".yaml", "")
        setup = yaml_file_to_dict("../configs/esm_master/setups/" + setup_name)
        for version in setup.get("available_versions", []):
            workflow["name"] = "CI-Ollie-install-" + setup_name + "-" + version
            for event_type in ["push", "pull_request"]:
                all_components = []
                for coupling in setup["choose_version"][version]["couplings"]:
                    components = coupling.split("+")
                    components = [item.split("-")[0] for item in components]
                    all_components += components
                components = list(set(all_components))
                workflow["on"][event_type]["branches"] = [
                    "release",
                    "develop",
                    "*prep-release*",
                ]
                workflow["on"][event_type]["paths"] = (
                    ["configs/esm_master/setups/" + setup_name + ".yaml"]
                    + [
                        "configs/esm_master/components/" + component + ".yaml"
                        for component in components
                    ]
                    + [
                        "configs/esm_master/couplings/" + coupling + ".yaml"
                        for coupling in setup["choose_version"][version]["couplings"]
                    ]
                )
            workflow["jobs"]["install_models"]["strategy"]["matrix"]["model"] = [
                setup_name + "-" + version
            ]
            with open(
                "../.github/workflows/"
                + "CI-Ollie-install-"
                + setup_name
                + "-"
                + version
                + ".yml",
                "w",
            ) as yml:
                yaml.dump(workflow, yml)


if __name__ == "__main__":
    main()
