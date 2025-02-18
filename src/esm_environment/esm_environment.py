"""
ESM-Tools Batch Script Template Module
======================================

A template-based script generation system for HPC environments using Jinja2.
Supports generation of both simple shell scripts and batch job scripts
(SLURM, PBS) with appropriate headers and environment setup.

This module modernizes the environment generation approach in ESM-Tools by providing
a template-driven solution for creating batch environment setup scripts. It replaces the
previous string-based generation with a more maintainable and flexible template system.

Examples
--------
>>> from batch_script_template import BatchScriptTemplate
>>> config = {
...     "sh_interpreter": "/bin/bash",
...     "module_actions": ["load python", "load netcdf"],
...     "export_vars": {"PATH": "/usr/local/bin:$PATH"}
... }
>>> batch = BatchScriptTemplate(config)
>>> script = batch.render()

>>> # Batch job script
>>> config.update({
...     "batch_system": "slurm",
...     "job": {
...         "name": "test_job",
...         "tasks": 4,
...         "time": "01:00:00",
...     }
... })
>>> batch = BatchScriptTemplate(config)
>>> script = batch.render()

See Also
--------
jinja2.Environment : The underlying template engine used
Path : Python's pathlib.Path used for template directory management
dpath : Deep dictionary utilities for nested updates
"""

# NOTE(PG): I'm not sure exactly, but pkg_resources is outdated
#           Instead, it is recommended to use importlib!
import importlib.resources
# import importlib.resources as pkg_resources
from pathlib import Path
from typing import Any, Dict, Optional

import dpath
from jinja2 import Environment, FileSystemLoader


def clean_env_var_name(name: str) -> str:
    """
    Remove numbered suffixes from environment variable names.

    Removes suffixes of the form [(number)] from environment variable names.
    This is useful when dealing with duplicate environment variables that
    have been numbered for uniqueness but need to be exported without the suffix.

    Parameters
    ----------
    name : str
        The environment variable name to clean

    Returns
    -------
    str
        The cleaned environment variable name

    Examples
    --------
    >>> clean_env_var_name("PATH[(1)]")
    'PATH'
    >>> clean_env_var_name("PYTHONPATH[(42)]")
    'PYTHONPATH'
    >>> clean_env_var_name("NORMAL_VAR")
    'NORMAL_VAR'
    >>> clean_env_var_name("MY_LIST[(list)]")
    'MY_LIST'

    Notes
    -----
    This function specifically handles:
    - Environment variables with numeric suffixes: VAR[(number)]
    - Environment variables with list suffix: VAR[(list)]
    - Regular environment variables (returns unchanged)

    The template that uses this function will skip empty or ``None`` values::

        >>> config = {
        ...     "export_vars": {
        ...         "PATH[(1)]": "/usr/bin",
        ...         "EMPTY_VAR": "",
        ...         "NONE_VAR": None,
        ...         "ZERO_VAR": 0,
        ...         "LIST_VAR[(list)]": "",
        ...         "VALID_VAR": "value"
        ...     }
        ... }

    Will generate::

        export PATH=/usr/bin
        export ZERO_VAR=0
        export VALID_VAR=value
    """
    if "[(" in name and ")]" in name:
        return name.split("[(")[0]
    return name


class BatchScriptTemplate:
    """
    A template engine for generating shell and batch job scripts.

    This class provides functionality to render both simple shell scripts and
    batch job scripts using Jinja2 templates. When configured for batch jobs,
    it handles batch system headers (SLURM/PBS), and in all cases manages
    module loading commands, environment variable exports, and variable unset commands.

    Parameters
    ----------
    config : Dict[str, Any]
        Configuration dictionary containing script setup parameters.
        Expected keys include:
            - sh_interpreter : str
                Shell interpreter to use (default: /bin/bash)
            - batch_system : str, optional
                Either 'slurm' or 'pbs'. If not provided, generates a simple
                shell script without batch headers.
            - module_actions : list
                List of module commands to execute
            - export_vars : dict
                Dictionary of environment variables to export
            - unset_vars : list
                List of variables to unset
    template_dir : Path, optional
        Directory containing custom Jinja2 templates for script generation.
        If not provided, uses the default templates from the package.

    Attributes
    ----------
    env : jinja2.Environment
        The Jinja2 environment instance used for template rendering.
    config : Dict[str, Any]
        The configuration dictionary used for rendering templates.
    batch_system : Optional[str]
        The batch system type ('slurm', 'pbs', or None for simple scripts)

    Notes
    -----
    The template directory should contain the following structure:
        templates/
        ├── base.sh.j2           # Base shell script template
        ├── headers/
        │   ├── slurm.sh.j2     # SLURM header template
        │   └── pbs.sh.j2       # PBS header template
        ├── module_actions.sh.j2 # Module loading template
        ├── exports.sh.j2       # Environment exports template
        └── unset.sh.j2         # Unset variables template

    Examples
    --------
    Simple shell script:
    >>> config = {
    ...     "module_actions": ["load intel"],
    ...     "export_vars": {"OMP_NUM_THREADS": "4"}
    ... }
    >>> batch = BatchScriptTemplate(config)
    >>> print(batch.render())
    #!/bin/bash -l

    module load intel
    export OMP_NUM_THREADS=4

    SLURM job script:
    >>> config.update({
    ...     "batch_system": "slurm",
    ...     "job": {
    ...         "name": "test_job",
    ...         "tasks": 4,
    ...         "time": "01:00:00",
    ...         "partition": "compute"
    ...     }
    ... })
    >>> batch = BatchScriptTemplate(config)
    >>> print(batch.render())
    #!/bin/bash -l
    #SBATCH --job-name=test_job
    #SBATCH --ntasks=4
    #SBATCH --time=01:00:00
    #SBATCH --partition=compute

    module load intel
    export OMP_NUM_THREADS=4

    See Also
    --------
    jinja2.Environment : The template engine used by this class
    dpath : Library used for deep dictionary updates
    """

    def __init__(
        self, config: Dict[str, Any], template_dir: Optional[Path] = None
    ) -> None:
        """
        Initialize the BatchScriptTemplate with configuration and optional templates.

        Parameters
        ----------
        config : Dict[str, Any]
            Configuration dictionary for script setup.
        template_dir : Path, optional
            Path to custom template directory. If None, uses package defaults.
        """
        self.config = config
        self.batch_system = config.get("batch_system", "").lower() or None

        if self.batch_system and self.batch_system not in ["slurm", "pbs"]:
            raise ValueError(
                "batch_system, if specified, must be either 'slurm' or 'pbs'"
            )

        # FIXME(PG): This is a nice idea, but it doesn't fit here...
        # if self.batch_system and "job" not in config:
        #     raise ValueError(
        #         "job configuration is required when batch_system is specified"
        #     )

        if template_dir is None:
            # Use the package's default templates
            # NOTE(PG): Old implementation with pkg_resources is outdated!
            # with pkg_resources.path("esm_tools.templates", "") as template_path:
            #     template_dir = template_path / "esm_environment"
            with importlib.resources.as_file(
                importlib.resources.files("esm_environment")
            ) as template_path:
                template_dir = template_path / "templates"

        self.env = Environment(
            loader=FileSystemLoader(template_dir), trim_blocks=True, lstrip_blocks=True
        )
        # Add custom jinja2 filters:
        self.env.filters["clean_env_var"] = clean_env_var_name

        # Post inits: clean up and finialize the configuration:
        self._post_init_add_esm_var()

    @classmethod
    def from_complete_config(
        cls, config: dict, template_dir: Optional[Path] = None
    ) -> "BatchScriptTemplate":
        """Initializes a BatchScriptTemplate from a full simulation config

        This extracts only the computer key of a complete simulation config
        and uses it to create the BatchScriptTemplate. Mirrors old behaviour.

        Raises
        ------
        KeyError :
            If you don't have a computer in your complete config, this
            constructor won't work!
        """
        if "computer" in config:
            return cls(config["computer"], template_dir=template_dir)
        raise KeyError(
            "Must give a complete simulation config containing a ``computer`` key!"
        )

    def _post_init_add_esm_var(self):
        """
        Adds the ENVIRONMENT_SET_BY_ESMTOOLS=TRUE to the config, for later
        dumping to the shell script.

        Mutates
        -------
        self.config :
            A key "export_vars" is either initialized as a dictionary, or extended
            by ENVIRONMENT_SET_BY_ESMTOOLS=True.
        """
        if "export_vars" in self.config:
            self.config["export_vars"]["ENVIRONMENT_SET_BY_ESMTOOLS"] = "TRUE"
        else:
            self.config["export_vars"] = {"ENVIRONMENT_SET_BY_ESMTOOLS": "TRUE"}

    def render(
        self, include_set_e: bool = True, tail_commands: Optional[list] = None
    ) -> str:
        """
        Render a complete script, optionally including batch system headers.

        Parameters
        ----------
        include_set_e : bool, optional
            Whether to include 'set -e' in the script (default: True)
        tail_commands : list, optional
            Commands to add at the end of the rendered template

        Returns
        -------
        str
            The rendered script as a string.

        Examples
        --------
        >>> config = {
        ...     "sh_interpreter": "/bin/bash",
        ...     "module_actions": ["load intel/2020"],
        ...     "export_vars": {"MPI_ROOT": "/opt/mpi"}
        ... }
        >>> script_template = BatchScriptTemplate(config)
        >>> script = script_template.render()

        Notes
        -----
        The rendered script will include sections for:
            1. Shell interpreter specification
            2. Batch system headers (if batch_system is specified)
            3. Module loading commands
            4. Environment variable exports
            5. Variable unset commands

        Raises
        ------
        jinja2.TemplateNotFound
            If the required template files are not found in template_dir
        jinja2.TemplateError
            If there are syntax errors in the templates
        """
        if tail_commands is None:
            tail_commands = []
        script_parts = []

        # Add batch system header if specified
        if self.batch_system:
            header_template = self.env.get_template(
                f"headers/{self.batch_system}.sh.j2"
            )
            header = header_template.render(job=self.config.get("job", {}))
            script_parts.append(header)

        # Add environment setup
        env_template = self.env.get_template("base.sh.j2")
        environment = env_template.render(
            sh_interpreter=self.config.get("sh_interpreter", "/bin/bash"),
            include_set_e=include_set_e,
            module_actions=self.config.get("module_actions", []),
            export_vars=self.config.get("export_vars", {}),
            unset_vars=self.config.get("unset_vars", []),
            general_actions=self.config.get("general_actions", []),
            spack_actions=self.config.get("spack_actions", []),
        )

        # Since batch system was added at the very beginning, we need to make sure
        # that the shell interpreter line is still at the top:
        environment = environment.split("\n")
        shell_interpreter = environment.pop(0)  # Remove shell interpreter line
        for env_statement in environment:
            script_parts.append(env_statement)

        # Add shell interpreter line at the beginning
        script_parts.insert(0, shell_interpreter)

        for tail_command in tail_commands:
            script_parts.append(tail_command)

        breakpoint()
        return "\n".join(script_parts)

    def update_config(self, new_config: Dict[str, Any], separator: str = "/") -> None:
        """
        This method uses dpath to perform deep dictionary updates, allowing for nested
        key updates without overwriting entire subdictionaries.

        Parameters
        ----------
        new_config : Dict[str, Any]
            New configuration values to update or add. Can contain nested paths.
        separator : str, optional
            Separator to use for nested paths in dpath (default: '/')

        Examples
        --------
        Simple update:
        >>> batch = BatchScriptTemplate({"sh_interpreter": "/bin/bash"})
        >>> batch.update_config({"module_actions": ["load intel"]})

        Nested update:
        >>> # Initial configuration:
        >>> initial_config = {
        ...     "export_vars": {
        ...         "PATH": "/usr/bin",
        ...         "nested": {"key": "old_value"}
        ...     }
        ... }
        >>> batch = BatchScriptTemplate(initial_config)
        >>> print("Initial config:", batch.config)
        Initial config: {
            'export_vars': {
                'PATH': '/usr/bin',
                'nested': {'key': 'old_value'}
            }
        }

        >>> # Perform nested updates:
        >>> batch.update_config({
        ...     "export_vars/nested/key": "new_value",
        ...     "export_vars/PATH": "/new/path"
        ... })
        >>> print("Updated config:", batch.config)
        Updated config: {
            'export_vars': {
                'PATH': '/new/path',
                'nested': {'key': 'new_value'}
            }
        }

        Notes
        -----
        The update is performed in-place on the instance's config dictionary.
        Nested paths in the new_config can be specified using the separator.

        See Also
        --------
        dpath : Library used for the deep dictionary updates
        """
        dpath.util.merge(self.config, new_config, separator=separator)
        # Update batch_system if it was changed
        if "batch_system" in new_config:
            self.batch_system = self.config.get("batch_system", "").lower() or None
            if self.batch_system and self.batch_system not in ["slurm", "pbs"]:
                raise ValueError(
                    "batch_system, if specified, must be either 'slurm' or 'pbs'"
                )

    def write_dummy_script(self, include_set_e: bool = True) -> None:
        """
        Writes a dummy script containing only the header information, module
        commands, and export variables. The actual compile/configure commands
        are added later.

        Parameters
        ----------
        include_set_e : bool
            Default to True, whether or not to include a ``set -e`` at the
            beginning of the script. This causes the shell to stop as soon as
            an error is encountered.
        """
        with open("dummy_script.sh", "w") as script_file:
            script_file.write(self.render(include_set_e=include_set_e))
