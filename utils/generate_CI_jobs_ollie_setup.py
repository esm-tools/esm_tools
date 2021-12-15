#!/usr/bin/env python
import jinja2
import yaml
import os
import sys

def main():
    loader = jinja2.FileSystemLoader("../")
    env = jinja2.Environment(loader=loader)
    template = env.get_template(".github/workflows/esm_tools_MODEL_TEMPLATE_hpc_awi_ollie.yml")
    print(template.render(MODEL="fesom", MODEL_NAME="fesom", MODEL_VERSION="2.0"))

    for setup_yaml in os.listdir("../configs/esm_master/setups/"):
        setup_name = setup_yaml.replace(".yaml", "")
        setup = yaml_file_to_dict("../configs/esm_master/setups/" + setup_name)
        try:
            versions = setup["available_versions"]
        except KeyError:
            print("Setup {setup_name} has no avaiable versions defined!")
            sys.exit(1)

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


if __name__ == '__main__':
    main()
