===========================================
ESM Runscripts - Using the Workflow Manager
===========================================

Introduction
------------

Starting with Release 6.0, esm_runscripts allows to define additional phases for e.g. data processing, coupling.
Such subjobs can be arranged into clusters, and the order of execution can be set in a flexible and short way from the runscript. This is applicable for both pre- and postprocessing, but especially useful for iterative coupling jobs, like e.g. coupling pism to vilma (see below). In this section we explain the basic concept, describe the keywords that have to be set in the runscript in order to make use of this feature, and give some examples on how to integrate pre- and postprocessing jobs and how to set up phases for iterative coupling.

Default phases of a general model simulation run
--------------------------------------------------------

ESM-Tools uses the workflow manager itself to organize the default :term:`workflow` :term:`phases<phase>` of a simulation :term:`run`. Since Release 6.0 the default workflow phases are the following::

        newrun --> prepcompute --> compute --> observe_compute --> tidy (+ resubmit next run)

.. Other than before adding the workflow manager, 

These standard phases are all separated and independant phases, each submitted (or started) by the previous phase in one of three ways (see below). Here is what each of the standard phases does:

.. The splitting of the old compute job into newrun, prepcompute and compute on one side, and tidy_and_resubmit into observe and tidy, was necessary to enable the user to insert coupling subjobs for iterative coupling at the correct places. Here is what each of the standard subjobs does:

====================================================== ============================================================= ========================
Phase                                                  Function                                                      Started by
====================================================== ============================================================= ========================
  newrun                                               Initializes a new experiment, only very basic stuff, like
                                                       creating (empty) folders needed by any of the following 
                                                       subjobs/phases. 
                                                       NEEDS TO BE THE FIRST SUBJOB/PHASE OF ANY 
                                                       :term:`EXPERIMENT<experiment>`.
  prepcompute                                          Prepares the compute job. All the (Python) functionality that
                                                       needs to be run, up to the job submission. Includes copying
                                                       files, editing namelists, write batch scripts, etc.
  compute                                              Actual model integration, nothing else. No Python codes       sbatch
                                                       involved.
  observe_compute                                      Python job running at the same time as compute, checking if   sbatch (compute), started by its own esm_runscripts call
                                                       the compute job is still running, looking for some known 
                                                       errors for monitoring / job termination.
  tidy                                                 Sorts the produced outputs, restarts and log files into       observe_compute
                                                       the correct folders, checks for missing and unknown files,
                                                       builds coupler restart files if not present
====================================================== ============================================================= ========================

It is important to understand that none of this has to be edited by the users. The above described workflow phases form the default set of phases. Changing anyone of these phases may lead esm-tools to fail. 

Inspect workflow phases
-----------------------

To inspect the workflow and workflow phases that are defined by e.g. a choosen setup or in an already run simulation/experiment, you can run esm_runscript with the -i (--inspect) option. This can be done for two different cases:

- To inspect the workflow previous to running a certain experiment. For exampl, if you want to add a new workflow phase, and need to know which phases are already defined in a choosen setup or model configuration::

        esm_runscripts runscript.yaml -i workflow

- To inspect a workflow from an experiment that has beed carried out already or created during a check-run (-c)::

        esm_runscripts runscript.yaml -e <expid> -i workflow

It will display the workflow configuration showing the order of workflow phases and their attributes and possible dependencies. This output should help to find out the correct keyworkds to be set when integrating a new workflow phase.

**Example output**::

        sldkfj

.. _def_workflow_phases:

Defining additional workflow phases
-----------------------------------

If it is necessary to complement the default workflow with simulation specific processing steps, this sequence of default workflow phases can be extended by adapting the runscipt or any component specific configuration files. The workflow manager will evaluate these additional phases and integrate them into the default sequence of the workflow. In order to integrate the additional phases correctly, theses phases have to be defined by providing the following information. In the following, we will explain how an additional phase can be defined by describing the necessary keywords and what kind of restrictions need to be taken into account.

In order to integrate a user defined phase into the default workflow, the following information need to be provided:
 * Name of the script to be run
 * Name of the python script used for setting up the environment
 * Path to the folder in which both of the above scripts can be found
 * Information on how often the phase should be called
 * Information about between which other phases the new user defined phase is to be inserted into the workflow
 * In case it isn't clear: Which phase should resubmit the next run.

In general, a workflow can be defined in the runscript or in any component configuration file. But there are some restrictions to the definition that needs to be taken into account:
 * The name of each phase needs to be unique. Otherwise, an exception error will be raised.
 * The names of the default phases are not allowed to be used for any new phases. This will also cause an exception error during runtime.
 * Settings in the runscript will overwrite settings in other config files. (See also :ref:`yaml_hierarchy:Hierarchy of YAML configuration files`.)

Keywords to define a new workflow phase
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
To define a new phase, the following keywords and mappings (key/value pairs) are available. (Keywords that are indicated with ``< >`` need to be adapted by the user.)

====================================================== ============ =========================== ==========================================================
Keyword                                                Mandatory    Default value               Function
====================================================== ============ =========================== ==========================================================
  **workflow**                                         yes          --                          Chapter headline in a runscript or configuration section, 
                                                                                                indicating that an alterations to the standard workflow 
                                                                                                will be defined here.

  next_run_triggered_by: <value>                       no           last phase in               Key/value entry in ``workflow`` section. Name of the phase
                                                                    (default) workflow          that should start the next run.
                                                                    (e.g. tidy)                        

  **phases**                                           yes          --                          Section within the ``workflow`` chapter that containes new 
                                                                                                additional workflow phases.

  **<new_phase_name>**                                 yes          --                          Section within the ``new_phases`` section for each new phase.
                                                                                                The name of the new phase needs to be unique. See also further
                                                                                                explenation here :ref:`def_workflow_phases`

  run_after: <value> or run_before: <value>            no           last phase in               Key/value entry in each ``<new_phase_name>`` section. 
                                                                    (default) workflow          This mapping defines the (default or user) phase of the 
                                                                    (e.g. tidy)                 workflow after or before the new phase should be executed.
                                                                                                Only one of the two should be specified. 

  submit_to_batch_system: <value>                      no           false                       Key/value entry in each ``<new_phase_name>`` section. 
                                                                                                This mapping defines if the (default or user) phase is 
                                                                                                submitted to batch system or not.

  run_on_queue: <value>                                no           None                        Key/value entry in each ``<new_phase_name>`` section.
                                                                                                This mapping defines to which queue (name) the job of the new phase
                                                                                                should be submitted to.

  batch_or_shell: <value>                              no           shell                       Key/value entry in each ``<new_phase_name>`` section.
                                                                                                This Mapping defines if the (default or user) phase is submitted
                                                                                                as batch job or as shell script.
                                                                              
  cluster: <value>                                     no           Phase name                  Key/value entry in each ``<new_phase_name>`` section. Phases
                                                                                                that have the same entry in ``cluster`` will be run 
                                                                                                from the same batch script.

  order_in_cluster: <value>                            no           sequential                  Key/value entry in each ``<new_phase_name>`` section. This mapping
                                                                                                defines how phases in the same ``<cluster>`` should be run.
                                                                                                Concurrent or serial.

  **script: <value>**                                  yes          None                        Key/value entry in each ``<new_phase_name>`` section. 
                                                                                                This mapping defines the name of the script that is going 
                                                                                                to be executed during the new workflow phase.

  **script_dir: <value>**                              yes          None                        Key/value entry in each ``<new_phase_name>`` section. 
                                                                                                This mapping defines the path to the script set by the variable
                                                                                                ``<script>``.

  call_function: <value>                               no           None                        Key/value entry in each ``<new_phase_name>`` section. 
                                                                                                This mapping defines the function within the script defined in
                                                                                                variable ``<script>`` should be executed.

  env_preparation: <value>                             no           None                        Key/value entry in each ``<new_phase_name>`` section. This
                                                                                                mapping defines e.g. a Python script/function that prepares 
                                                                                                a dictionary with environment variables.

  nproc: <value>                                       no           1                           Key/value entry in each ``<new_phase_name>`` section.
                                                                                                This mapping defines the number of CPUs a phase should run with
                                                                                                (if run via sbatch).

  run_only: <value>                                    no           None                        Key/value entry in each ``<new_phase_name>`` section.
                                                                                                This mapping defines when the phase should be run. E.g. run only
                                                                                                at the beginning of a :term:`chunk` (set of runs).

  skip_chunk_number: <value>                           no           None                        Key/value entry in each ``<new_phase_name>`` section. This
                                                                                                mapping defines how many chunks should be skipped before the 
                                                                                                phase will be execited.
====================================================== ============ =========================== ==========================================================

Syntax example
^^^^^^^^^^^^^^
The following code snippet shows the general syntax for defining a new workflow phase.
::

    workflow:
        next_run_triggered_by: <value>
        
        phases:
            <new_phase_name>:
                run_after: <value>
                submit_to_batch_system: <value>
                run_on_queue: <value>
                batch_or_shell: <value>
                cluster: <value>
                order_in_cluster: <value>
                script: <value>
                call_function: <value>
                env_preparation: <value>
                nproc: <value>
                run_only: <value>
                skip_chunk_number: <value>

Examples for the definition of new workflow phases
--------------------------------------------------

Example 1: Adding an additional postprocessing subjob
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the case of a simple postprocessing task (here for model Echam), that sould be run as the last task of each run, independantly from restarting the experiment, the corresponding minimal code snippet in a runscript could look like this ::

    echam:
        [...other information...]

        workflow:
            phases:
                my_postprocessing:
                    script_dir: <value>
                    script: <values>


Example 2: Adding an additional preprocessing subjob
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A preprocessing job basically is configured the same way as a postprocessing job, but the run_before keyword is needed now, to define when the new phase should be run::

    echam:
        [...other information...]

        workflow:
            phases:
                my_preprocessing:
                    run_before: prepcompute
                    script_dir: <value>
                    script: <values>

Example 3: Adding a new phase as the last task in a run
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To integrate a new phase that should be run as the last task in every run but before the next run starts, use the following example::

    echam:
        [...other information...]

        workflow:
            next_run_triggered: my_new_last_phase

            phases:
                my_new_last_phase:
                    script_dir: <value>
                    script: <values>

Example 4: Adding an iterative coupling job
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Writing a runscript for iterative coupling using the workflow manager requires some more changes. The principal idea is
that each coupling step consists of two data processing jobs, one pre- and one postprocessing job. This is done this way
as to make the coupling modular, and enable the modeller to easily replace one of the coupled components by a different
implementation. This is of course up to the user to decide, but we generally advise to do so, and the iterative couplings
distributed with `ESM-Tools` are organized this way. ::

    echam:
        [...other information...]

         workflow:
            next_run_triggered_by: couple_out
            subjobs:
                couple_in:
                    nproc: 1
                    run_before: prepcompute
                    script: coupling_ice2echam.functions
                    script_dir: ${general.script_dir}/echam
                    call_function: ice2echam
                    env_preparation: env_echam.py
                    run_only: first_run_in_chunk
                    skip_chunk_number: 1

                couple_out:
                    nproc: 1
                    run_after: tidy
                    script: coupling_echam2ice.functions
                    script_dir: ${general.script_dir}/echam
                    call_function: echam2ice
                    env_preparation: env_echam.py
                    run_only: last_run_in_chunk

    fesom:
        [...other information...]

        workflow:
            next_run_triggered_by: couple_out
            subjobs:
                couple_in:
                    nproc: 1
                    run_before: prepcompute
                    script: coupling_ice2fesom.functions
                    script_dir: ${general.script_dir}/fesom
                    call_function: ice2fesom
                    env_preparation: env_fesom.py
                    run_only: first_run_in_chunk
                    skip_chunk_number: 1

                couple_out:
                    nproc: 1
                    run_after: tidy
                    script: coupling_fesom2ice.functions
                    script_dir: ${general.script_dir}/fesom
                    call_function: fesom2ice
                    env_preparation: env_fesom.py
                    run_only: last_run_in_chunk
