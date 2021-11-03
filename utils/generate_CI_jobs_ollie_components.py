#!/usr/bin/env python
import os
import sys

from esm_parser import yaml_file_to_dict
import jinja2


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
        print(config["model"])
        import dpath.util

        print(dpath.util.values(config, "**/available_versions"))
        # print(config["model"]["available_versions"])


if __name__ == "__main__":
    main()
