import os

from jinja2 import StrictUndefined, Template, UndefinedError
from loguru import logger

import esm_parser


def render_template(config, source, target):
    """
    Renders a ``target`` file using a ``source`` template. ``config`` is used to
    substitute the place holders in the template. Like in any other ninja template
    it's possible to use the shorten version (``{{xios.ni_glo}}``) or following the
    python dictionary syntax (``{{xios['ni_glo']}}``).

    Parameters
    ----------
    config : dict
        Dictionary with the configuration data to be used in the template
    source : str
        Relative or absolute path to the template
    target : str
        Relative or absolute path to the rendered file

    Raises
    ------
    user_error : esm_parser.user_error
        If the parameter to be substituting does not exists in ``config``
    """
    # If target finishes with ``.j2``, remove it
    if target.endswith(".j2"):
        target = target[:-3]

    # Read the template
    with open(source, "r") as f:
        template = Template(f.read(), undefined=StrictUndefined)

    # Try to render the file using the data in ``config``
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

    # Write the rendered file
    with open(target, "w") as f:
        f.write(content)
