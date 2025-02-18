"""
ESM-Tools Batch Script Template Module
====================================

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

import importlib.resources as pkg_resources
from pathlib import Path
from typing import Any, Dict, Optional

import dpath
from jinja2 import Environment, FileSystemLoader


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
            - job : dict, optional
                Job-specific settings (name, tasks, time, etc.).
                Required if batch_system is specified.
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

        if self.batch_system and "job" not in config:
            raise ValueError(
                "job configuration is required when batch_system is specified"
            )

        if template_dir is None:
            # Use the package's default templates
            with pkg_resources.path("esm_tools.templates", "") as template_path:
                template_dir = template_path / "esm_environment"

        self.env = Environment(
            loader=FileSystemLoader(template_dir), trim_blocks=True, lstrip_blocks=True
        )

    def render(self, include_set_e: bool = True) -> str:
        """
        Render a complete script, optionally including batch system headers.

        Parameters
        ----------
        include_set_e : bool, optional
            Whether to include 'set -e' in the script (default: True)

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
        script_parts.append(environment)

        # Add shell interpreter line at the beginning
        script_parts.insert(0, shell_interpreter)

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
