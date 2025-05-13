import os

from jinja2 import (StrictUndefined, Template, TemplateSyntaxError,
                    UndefinedError)
from loguru import logger

from esm_tools import user_error


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
        try:
            template = Template(f.read(), undefined=StrictUndefined)
        except TemplateSyntaxError as e:
            user_error(
                "Jinja",
                f"Templating Error while loading template from ``{source}``. "
                f"Syntax error in the template: {e.message}",
            )

    # Try to render the file using the data in ``config``
    try:
        content = template.render(config)
    except UndefinedError as e:
        missing_variable = e.message.split("'")[3]
        user_error(
            "Jinja",
            f"Error rendering template from ``{source}`` to ``{target}``. Variable "
            f"``{missing_variable}`` is not defined in any configuration file.",
        )
    except TemplateSyntaxError as e:
        user_error(
            "Jinja",
            f"Templating Error while rendering template from ``{source}`` to ``{target}``. "
            f"Syntax error in the template: {e.message}",
        )
    except Exception as e:
        # Any other error
        user_error(
            "Jinja",
            f"Error rendering template from ``{source}`` to ``{target}``. "
            f"Error: {e}",
        )

    if os.path.isfile(target):
        logger.debug(f"File already exists!: {target}")

    # Write the rendered file
    with open(target, "w") as f:
        f.write(content)
