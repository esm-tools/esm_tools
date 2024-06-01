import os

import esm_parser

from jinja2 import Template, StrictUndefined, UndefinedError
from loguru import logger


def render_template(config, source, target):
    with open(source, "r") as f:
        template = Template(f.read(), undefined=StrictUndefined)

    try:
        content = template.render(config)
    except UndefinedError as e:
        missing_variable = e.message.split("'")[3]
        esm_parser.user_error(
            "Jinja",
            f"Error rendering template from ``{source}`` to ``{target}``. Variable "
            f"``{missing_variable}`` is not defined in any configuration file.",
        )

    if os.path.isfile(target):
        logger.debug(f"File already exists!: {target}")

    with open(target, "w") as f:
        f.write(content)


if __name__ == "__main__":
    config = {
        "xios": {
            "ni_glo": 400,
            "nj_glo": 600,
        },
    }

    render_template(
        config,
        "/Users/mandresm/Codes/esm_tools/namelists/oifs/43r3/xios/domain_def.j2",
        "out_file.xml",
    )
