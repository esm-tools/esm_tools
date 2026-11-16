"""Resolving and running a job type's recipe steps.

Lazy, on purpose: step names are resolved here, at dispatch time, not
baked into the :class:`~esm_runscripts.pplan.plan.MultiStep` model at
``Workflow``-build time. Baking them in there would mean ``pplan.plan``
calling into this module to resolve them -- the wrong dependency
direction (``execution`` already imports ``pplan``, not the reverse) --
and would make constructing a ``Workflow`` do real I/O (reading
``esm_runscripts.yaml``) for a model that's supposed to be cheap, pure
data.

Reuses ``helpers.get_recipe_step_names`` -- already factored out of
``helpers.evaluate`` for exactly this reuse (see its own docstring) --
plus the same ``esm_plugin_manager.read_plugin_information`` ->
``check_plugin_availability`` -> ``work_through_recipe`` sequence
``evaluate`` itself uses for the legacy engine's recipes.
"""

from __future__ import annotations

from typing import List

import esm_plugin_manager
import esm_tools

from .. import helpers
from ..pplan.plan import Step


def resolve_steps(config: dict, job_type: str) -> List[Step]:
    """The ordered list of :class:`Step`\\ s ``job_type`` resolves to,
    honoring any ``general.<job_type>_recipe``/``<setup_name>.
    <job_type>_recipe`` override -- read-only, no plugin resolution or
    execution."""
    names = helpers.get_recipe_step_names(config, job_type, f"{job_type}_recipe")
    return [Step(name=name) for name in names]


def run_steps(config: dict, job_type: str) -> dict:
    """Resolve ``job_type``'s steps and actually run them -- the same
    ``read_plugin_information`` -> ``check_plugin_availability`` ->
    ``work_through_recipe`` sequence ``helpers.evaluate`` uses for the
    legacy engine's recipes."""
    steps = resolve_steps(config, job_type)
    if not steps:
        return config

    recipe = {"recipe": [step.name for step in steps], "job_type": job_type}
    plugins_bare = (
        esm_tools.get_config_filepath() + "/esm_software/esm_runscripts/esm_plugins.yaml"
    )
    plugins = esm_plugin_manager.read_plugin_information(plugins_bare, recipe)
    esm_plugin_manager.check_plugin_availability(plugins)
    return esm_plugin_manager.work_through_recipe(recipe, plugins, config)
