#!/usr/bin/env python
import os
import sys

import dpath.util
import jinja2

from esm_parser import yaml_file_to_dict


def main():
    loader = jinja2.FileSystemLoader("../")
    env = jinja2.Environment(loader=loader)
    template = env.get_template(
        ".github/workflows/esm_tools_COMPONENT_TEMPLATE_hpc_awi_ollie.yml"
    )
    print(
        "This script will generate CI Yamls for >> COMPONENTS << in the following form:"
    )
    print(template.render(MODEL_NAME="fesom", MODEL_VERSION="2.0"))

    component_configs = []
    for component_name in os.listdir("../configs/components/"):
        for component_config in os.listdir(f"../configs/components/{component_name}"):
            if component_config.endswith(".yaml"):
                config = yaml_file_to_dict(
                    f"../configs/components/{component_name}/{component_config}"
                )
                if (
                    "model" in config
                ):  # Filter out stuff like datasets from e.g. further reading
                    component_configs.append(config)
    for config in component_configs:
        available_versions = [values for values in dpath.util.values(config, "**/available_versions") if values]
        for ver_list in available_versions:
            if ver_list:
                for ver in ver_list:
                    mod_name = config['model']
                    ci_script = template.render(MODEL_NAME=mod_name, MODEL_VERSION=ver)
                    with open(f"../.github/workflows/esm_tools_{mod_name}-{ver}_hpc_awi_ollie.yml", "w") as ci_file:
                        ci_file.write(ci_script)

if __name__ == "__main__":
    main()
