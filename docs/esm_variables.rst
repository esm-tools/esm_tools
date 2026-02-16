ESM-Tools Variables
===================

The `esm_parser` is used to read the multiple types of `YAML` files contained in `ESM-Tools`
(i.e. model and coupling configuration files, machine configurations, runscripts, etc.). Each of
these `YAML` files can contain two type of `YAML` elements/variables:

  * **Tool-specific elements**: `YAML-scalars`, `lists` or `dictionaries` that include instructions and
    information used by `ESM-Tools`. These elements are predefined inside the `esm_parser` or other
    packages inside `ESM-Tools` and are used to control the `ESM-Tools` functionality.

  * **Setup/model elements**: `YAML-scalars`, `lists` of `dictionaries` that contain
    information defined in the model/setup config files (i.e. ``awicm.yaml``, ``fesom.yaml``, etc.).
    This information is model/setup-specific and causes no effect unless it is combined with the
    **tool-specific elements**. For example, in ``fesom.yaml`` for `FESOM-1.0` the variable
    ``asforcing`` exists, however this means nothing to `ESM-Tools` by its own. In this case, this
    variable is used in ``namelist_changes`` (a tool-specific element) to state the type of forcing
    to be used and this is what actually makes a difference to the simulation. The advantage of
    having this variable already defined and called in ``namelist_changes``, in the ``fesom.yaml``
    is that the front-end user can simply change the forcing type by changing the value of
    ``asforcing`` (no need for the front-end user to use ``namelist_changes``).

The following subsection lists and describes the **Tool-specific elements** used to operate `ESM-Tools`.

.. Note::
   Most of the **Tool-specific elements** can be defined in any file (i.e. `configuration file`,
   `runscript`, ...) and, if present in two files used by ESM-Tools at a time, the value is chosen
   depending on the ESM-Tools file priority/read order (:ref:`yaml_hierarchy:YAML File Hierarchy`).
   Ideally, you would like to declare as many elements as possible inside the `configuration files`,
   to be used by default, and change them in the `runscripts` when necessary. However, it is ultimately
   up to the user where to setup the Tool-specific elements.

Tool-Specific Elements/Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following keys should/can be provided inside configuration files for models
(``<PATH>/esm_tools/configs/components/<name>/<name>.yaml``), coupled setups
(``<PATH>/esm_tools/configs/setups/<name>/<name>.yaml``) and runscripts. You can find
runscript templates in ``esm_tools/runscripts/templates/``).

Compile time variables
----------------------

.. csv-table::
   :header: Key, Section, Description
   :widths: 10, 10, 80

   execution_mode,     general,             "Takes the value ``compile`` during compile time. Can be used in ``choose_`` blocks with ``choose_general.execution_mode``."
   model,              general,             "Name of the model/setup as listed in the config files (``esm_tools/configs/components`` for models and ``esm_tools/configs/setups`` for setups)."
   setup_name,         general,             Name of the coupled setup.
   version,            general,             "Version of the model/setup (one of the available options in the ``available_versions`` list)."
   available_versions, <component>,         List of supported versions of the component or coupled setup.
   git-repository,     <component>,         Address of the model's git repository.
   branch,             <component>,         Branch from where to clone.
   destination,        <component>,         "Name of the folder where the model is downloaded and compiled, in a coupled setup."
   comp_command,       <component>,         Command used to compile the component.
   install_bins,       <component>,         "Path inside the component folder, where the component is compiled by default. This path is necessary because, after compilation, ESM-Tools needs to copy the binary from this path to the ``<component/setup_path>/bin`` folder."
   source_code_permissions,     <component>,    "Sets the file permisions for the source code using `chmod <source_code_permissions> -R <source_code_folder>."

Runi time variables
-------------------
.. csv-table::
   :header: Key, Section, Description
   :widths: 10, 10, 80

   account,             general,                User account of the HPC system to be used to run the experiment.
   base_dir,            general,                Path to the directory that will contain the experiment folder (where the experiment will be run and data will be stored).
   compute_time,        general,                "Estimated computing time for a run, used for submitting a job with the job scheduler."
   create_folders,      <component>,            "List of absolute paths of the folders to be created. See :ref:`yaml:Create empty folders`."
   executable,          <component>,            "Name of the component executable file, as it shows in the ``<component/setup_path>/bin`` after compilation."
   execution_command,   <component>,            "Command for executing the component, including ``${executable}`` and the necessary flags."
   execution_mode,      general,                "Takes the value ``run`` during run time. Can be used in ``choose_`` blocks with ``choose_general.execution_mode``."
   expid,               general,                "ID of the experiment. This variable can also be defined when calling ``esm_runscripts`` with the ``-e`` flag."
   ":ref:`yaml:File Dictionaries`",     <component>,    "`YAML` dictionaries used to handle input, output, forcing, logging, binary and restart files (see :ref:`yaml:File Dictionaries`)."
   force_overwrite_in_file_movements, general   "A boolean to indicate whether the file movements should overwrite existing files or not. If ``False`` (default), the file movements will not overwrite existing files. Only set to ``True`` if you know why you would want to do that (e.g to overwrite files in a failed tidy task)."
   heterogeneous_parallelization,   computer,    "A boolean that controls whether the simulation needs to be run with or without heterogeneous parallelization. When ``false`` OpenMP is not used for any component, independently of the value of ``omp_num_threads`` defined in the components. When ``true``, ``open_num_threads`` needs to be specified for each component using OpenMP. ``heterogeneous_parallelization`` variable **needs to be defined inside the** ``computer`` section of the runscript. See :ref:`cookbook:Heterogeneous Parallelization Run (MPI/OpenMP)` for examples."
   ini_restart_dir,     <component>,            "Path of the restarted experiment in case the current experiment runs in a different directory. For this variable to have an effect ``lresume`` needs to be ``true`` (e.g. the experiment is a restart)."
   ini_restart_exp_id,  <component>,            "ID of the restarted experiment in case the current experiment has a different ``expid``. For this variable to have an effect ``lresume`` needs to be ``true`` (e.g. the experiment is a restart)."
   install_missing_plugins,     general,        "A boolean to indicate whether ``esm_runscripts`` needs to install missing plugins (``True``, default) or not (``False``). Implemented to solve a problem with the ``esm_tests`` CI in GitHub where we might not have access to some repositories."
   lresume,             <component>,            Boolean to indicate whether the run is an initial run or a restart.
   mail_type,           general/computer,       "Value for the SBATCH flag ``--mail-type`` (see https://slurm.schedmd.com/sbatch.html#OPT_mail-type)"
   mail_user,           general/computer,       "Value for the SBATCH flag ``--mail-user`` (see https://slurm.schedmd.com/sbatch.html#OPT_mail-user)"
   model_dir,           general/<component>,    "Absolute path of the model directory (where it was installed by `esm_master`)."
   namelists,           <component>,            "List of namelist files required for the model."
   namelist_changes,    <component>,            "Functionality to handle changes in the namelists from the yaml files (see :ref:`yaml:Changing Namelists`)."
   nproc,               <component>,            Number of processors to use for the model.
   nproca/nprocb,       <component>,            "Number of processors for different MPI tasks/ranks. Incompatible with ``nproc``."
   nnodes_envvar,       computer,               "Name of the environment variable holding the number of allocated nodes (e.g. ``SLURM_JOB_NUM_NODES``)."
   omp_num_threads,     <component>,            "A variable to control the number of OpenMP threads used by a component during an heterogeneous parallelization run. This variable **has to be defined inside the section of the components** for which OpenMP needs to be used. This variable will be ignored if ``computer.heterogeneous_parallelization`` is not set to ``true``."
   parallel_file_movements,     general,        "Controls how file movements are parallelized. ``""dask""`` (default) distributes I/O across all compute nodes via a Dask cluster, ``""threads""`` uses local threads on a single node, ``False`` runs sequentially. See :ref:`esm_runscripts:Parallel File Movements`."
   pool_dir,            general,                "Path to the pool directory to read in mesh data, forcing files, inputs, etc."
   post_processing,     <component>,            Boolean to indicate whether to run postprocessing or not.
   post_run_commands,   "any section",          "Shell commands appended to the job script after the model execution and before resubmission. Collected from all top-level config sections (components, computer, etc.). Can be a ``string`` or a ``list`` of ``strings``."
   pre_recipe.exclude_job_types,  general,      "List of job types that skip ``pre_recipe.steps`` (default: ``[""prepare"", ""prepexp"", ""observe""]``)."
   pre_recipe.steps,    general,                "List of recipe step names injected before the main recipe (e.g. ``[""initialize_dask_cluster""]``). Steps listed here run for all job types except those in ``pre_recipe.exclude_job_types``."
   save_batch_env_patterns,  computer,          "List of grep patterns used to capture and restore batch system environment variables across job script stages (e.g. ``[""SLURM""]`` or ``[""PBS""]``)."
   setup_dir,           general,                "Absolute path of the setup directory (where it was installed by `esm_master`)."
   system_components,   general,                "List of non-model config sections included in file-list iteration loops (default: ``[""general"", ""dask""]``)."
   time_step,           <component>,            Time step of the component in seconds.

Dask variables
--------------

Variables in the ``dask`` section control the Dask cluster used for parallel file
movements. See :ref:`esm_runscripts:Parallel File Movements` for usage details.

.. csv-table::
   :header: Key, Section, Description
   :widths: 10, 10, 80

   actions,              dask,           "List of actions that trigger Dask cluster initialization (default: ``[""parallel_file_movements""]``)."
   client_timeout,       dask,           "Timeout in seconds when probing the Dask scheduler status (default: ``0.05``)."
   init_scheduler_cmd,   dask,           "Shell command to start the Dask scheduler. Defined per batch system (e.g. in ``slurm.yaml``)."
   init_workers_cmd,     dask,           "Shell command to start the Dask workers. Defined per batch system (e.g. in ``slurm.yaml``)."
   parallel_file_movements,  general,    "Controls how file movements are parallelized. ``""dask""`` (default) distributes I/O across all compute nodes via a Dask cluster, ``""threads""`` uses local threads on a single node, ``False`` runs sequentially. See :ref:`esm_runscripts:Parallel File Movements`."
   poll_interval,        dask,           "Polling interval in seconds for Dask cluster readiness checks (default: ``0.5``)."
   scheduler_json,       dask,           "Full path to the Dask scheduler JSON file used for client connections (default: ``${general.thisrun_work_dir}/dask_scheduler.json``)."
   workers_timeout,      dask,           "Maximum time in seconds to wait for Dask workers to become available (default: ``5``)."

Calendar variables
------------------
.. csv-table::
   :header: Key, Description
   :widths: 15, 85

   initial_date,        "Date of the beginning of the **simulation** in the format YYYY-MM-DD. If the simulation is a restart, ``initial_date`` marks the beginning of the restart."
   final_date,          "Date of the end of the **simulation** in the format YYYY-MM-DD."
   start_date,          "Date of the beginning of the **current run**."
   end_date,            "Date of the end of the **current run**."
   current_date,        Current date of the run.
   next_date,           "Next run initial date."
   "nyear, nmonth, nday, nhour, nminute",       "Number of time unit per run. They can be combined (i.e. ``nyear: 1`` and ``nmonth: 2`` implies that each run will be 1 year and 2 months long)."
   parent_date,         Ending date of the previous run.

Coupling variables
------------------
.. csv-table::
   :header: Key, Description
   :widths: 15, 85

   grids,               "List of grids and their parameters (i.e. ``name``, ``nx``, ``ny``, etc.)."
   coupling_fields,     List of coupling field dictionaries containing coupling field variables.
   nx,                  "When using `oasis3mct`, used inside ``grids`` to define the first dimension of the grid."
   ny,                  "When using `oasis3mct`, used inside ``grids`` to define the second dimension of the grid."
   coupling_methods,    "List of coupling methods and their parameters (i.e. ``time_transformation``, ``remapping``, etc.)."
   time_transformation,     "Time transformation used by `oasis3mct`, defined inside ``coupling_methods``."
   remapping,           "Remappings and their parameters, used by `oasis3mct`, defined inside ``coupling_methods``."

Environment variables
--------------------
.. csv-table::
   :header: Key, Section, Description
   :widths: 15, 15, 70

   general_actions,      computer,            "List of general shell actions to be included in the compilation and run scripts. These are added directly to the script without any prefix."
   module_actions,       computer,            "List of module actions to be included in the compilation and run scripts. Each entry will be prefixed with ``module`` in the generated script."
   spack_actions,        computer,            "List of Spack actions to be included in the compilation and run scripts. Each entry will be prefixed with ``spack`` in the generated script."
   export_vars,          computer,            "Dictionary of environment variables to be exported in the script. Each key-value pair will generate an ``export KEY=VALUE`` line."
   unset_vars,           computer,            "List of environment variables to be unset in the script. Each entry will generate an ``unset VARIABLE`` line."
   include_env_from_component_files,  computer/<component>,  "Boolean that controls whether environment variables from component files should be included. Can be set globally in the computer section or per-component. Default: ``True``."
   merge_component_envs, computer,           "Dictionary with ``compile`` and ``run`` keys that controls whether environments from all components should be merged. For ``compile`` the default is false (each component maintains its own environment), for ``run`` the default is true (environments are merged)."

.. note::
   For more detailed information on all environment configuration options, including attribute-based selection,
   coupled setup environment control, and advanced environment management features, please refer to the
   :ref:`esm_environment:ESM Environment` documentation.

Other variables
---------------
.. csv-table::
   :header: Key, Description
   :widths: 15, 85

   metadata,            "List to incude descriptive information about the model (i.e. ``Authors``, ``Institute``, ``Publications``, etc.) used to produce the content of :ref:`Supported_Models:Supported Models`. This information should be organized in nested `keys` followed by the corresponding description. Nested `keys` do not receive a special treatment meaning that you can include here any kind of information about the model. Only the `Publications` `key` is treated in a particular way: it can consist of a single element or a `list`, in which each element contains a link to the publication inside ``<>`` (i.e. ``- Title, Authors, Journal, Year. <https://doi.org/...>``)."

