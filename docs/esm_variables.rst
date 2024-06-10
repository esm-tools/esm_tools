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

Installation variables
----------------------

.. csv-table::
   :header: Key, Description
   :widths: 15, 85

   model,                   "Name of the model/setup as listed in the config files (``esm_tools/configs/components`` for models and ``esm_tools/configs/setups`` for setups)."
   setup_name,              Name of the coupled setup.
   version,                 "Version of the model/setup (one of the available options in the ``available_versions`` list)."
   available_versions,      List of supported versions of the component or coupled setup.
   git-repository,          Address of the model's git repository.
   branch,                  Branch from where to clone.
   destination,             "Name of the folder where the model is downloaded and compiled, in a coupled setup."
   comp_command,            Command used to compile the component.
   install_bins,            "Path inside the component folder, where the component is compiled by default. This path is necessary because, after compilation, ESM-Tools needs to copy the binary from this path to the ``<component/setup_path>/bin`` folder."
   source_code_permissions, "Sets the file permisions for the source code using `chmod <source_code_permissions> -R <source_code_folder>."

Runtime variables
-----------------
.. csv-table::
   :header: Key, Section, Description
   :widths: 10, 10, 80

   account,             general,                User account of the HPC system to be used to run the experiment.
   base_dir,            general,                Path to the directory that will contain the experiment folder (where the experiment will be run and data will be stored).
   compute_time,        general,                "Estimated computing time for a run, used for submitting a job with the job scheduler."
   executable,          <component>,            "Name of the component executable file, as it shows in the ``<component/setup_path>/bin`` after compilation."
   execution_command,   <component>,            "Command for executing the component, including ``${executable}`` and the necessary flags."
   expid,               general,                "ID of the experiment. This variable can also be defined when calling ``esm_runscripts`` with the ``-e`` flag."
   ":ref:`yaml:File Dictionaries`",     <component>,    "`YAML` dictionaries used to handle input, output, forcing, logging, binary and restart files (see :ref:`yaml:File Dictionaries`)."
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
   omp_num_threads,     <component>,            "A variable to control the number of OpenMP threads used by a component during an heterogeneous parallelization run. This variable **has to be defined inside the section of the components** for which OpenMP needs to be used. This variable will be ignored if ``computer.heterogeneous_parallelization`` is not set to ``true``."
   parallel_file_movements,     general,        "A variable indicating whether the file movements should be done in parallel or not. If ``threads`` (, the file movements will be done in parallel in a single node (. If ``False``, the file movements will be done sequentially."
   pool_dir,            general,                "Path to the pool directory to read in mesh data, forcing files, inputs, etc."
   post_processing,     <component>,            Boolean to indicate whether to run postprocessing or not.
   setup_dir,           general,                "Absolute path of the setup directory (where it was installed by `esm_master`)."
   time_step,           <component>,            Time step of the component in seconds.

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


Other variables
---------------
.. csv-table::
   :header: Key, Description
   :widths: 15, 85

   metadata,            "List to incude descriptive information about the model (i.e. ``Authors``, ``Institute``, ``Publications``, etc.) used to produce the content of :ref:`Supported_Models:Supported Models`. This information should be organized in nested `keys` followed by the corresponding description. Nested `keys` do not receive a special treatment meaning that you can include here any kind of information about the model. Only the `Publications` `key` is treated in a particular way: it can consist of a single element or a `list`, in which each element contains a link to the publication inside ``<>`` (i.e. ``- Title, Authors, Journal, Year. <https://doi.org/...>``)."

