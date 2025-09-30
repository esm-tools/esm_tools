===========================================
ESM Runscripts - Using the Workflow Manager
===========================================

Introduction
------------

Starting with Release 6.0, ``esm_runscripts`` allows to define additional :term:`jobs<job>` for e.g. data processing, coupling.
Such jobs can be arranged into job-clusters, and the order of execution can be set in a flexible and short way from the runscript.
This is applicable for both pre- and postprocessing, but especially useful for iterative coupling jobs, like e.g. coupling PISM to
VILMA (see below). In this section we explain the basic concept, describe the keywords that have to be set in the runscript in order
to make use of this feature, and give some examples on how to integrate pre- and postprocessing jobs and how to set up jobs for iterative
coupling.

Default jobs of a general model simulation run
----------------------------------------------

The task of ``esm_runscript`` is split into different subjobs which are::

        newrun --> prepcompute --> compute --> observe_compute --> tidy (+ resubmit next run)

These standard jobs are all separated and independent, each submitted (or started) by the previous job in one of three
ways (see below). Here is what each of the standard jobs do:

.. |warn| replace:: ⚠️ **Warning:** It needs to be the first job of any :term:`experiment<experiment>`.

====================================================== ============================================================= ========================
Job                                                    Description                                                   Started by
====================================================== ============================================================= ========================
  newrun                                               Initializes a new experiment, only very basic stuff, like
                                                       creating (empty) folders needed by any of the following 
                                                       subjobs/jobs. 
                                                       |warn|
  prepcompute                                          Prepares the compute job. All the (Python) functionality that newrun
                                                       needs to be run, up to the job submission. Includes copying
                                                       files, editing namelists, write batch scripts, etc.
  compute                                              Actual model integration, nothing else. No Python codes       prepcompute via ``sbatch`` or other batch system command
                                                       involved.
  observe_compute                                      Python job running at the same time as compute, checking if   ``sbatch``, started by its own ``esm_runscripts`` call in the ``.run`` script, after the  ``compute`` job has been submitted with ``srun`` or other batch launcher.
                                                       the compute job is still running, looking for some known 
                                                       errors for monitoring / job termination.
  tidy                                                 Sorts the produced outputs, restarts and log files into       observe_compute
                                                       the correct folders, checks for missing and unknown files,
                                                       builds coupler restart files if not present
====================================================== ============================================================= ========================

.. note::
  None of this has to be edited by the users. The above described workflow jobs form the default set of jobs needed to run any simulation.
  Changing anyone of these jobs may lead `ESM-Tools` to fail. However, additional jobs can be added to this workflow, as described below,
  to extend the default workflow.

Inspect workflow jobs
---------------------

To inspect the workflow and workflow jobs that are defined by e.g. a choosen setup or in an already run simulation/experiment, you can
run esm_runscript with the ``-i`` (``--inspect``) option. This can be done for two different cases:

- To inspect the workflow previous to running a certain experiment. For example, if you want to add a new workflow job, and need to know
  which jobs are already defined in a choosen setup or model configuration::

        esm_runscripts runscript.yaml -i workflow

- To inspect a workflow from an experiment that has beed carried out already or created during a check-run (-c)::

        esm_runscripts runscript.yaml -e <expid> -i workflow

It will display the workflow configuration showing the order of workflow jobss and their attributes and possible dependencies. This output
should help to find out the correct keyworkds to be set when integrating a new workflow job.

**Example output**::

        Workflow sequence (cluster [jobs])
        ----------------------------------
        prepcompute ['prepcompute'] ->  compute ['compute'] ->  tidy ['tidy'] ->  prepcompute ['prepcompute'] and my_own_new_cluster ['my_new_last_job', 'my_second_new_job']

.. _def_workflow_jobs:

Defining additional workflow jobs
---------------------------------

If it is necessary to complement the default workflow with simulation specific processing steps, this sequence of default workflow jobs can be
extended by adapting the runscipt or any component specific configuration files. The workflow manager will evaluate these additional jobss and
integrate them into the default sequence of the workflow. In order to integrate the additional jobs correctly, the following information about
this job needs to be given in the one of the yaml files:

 * Name of the script to be run
 * Name of the python script used for setting up the environment
 * Path to the directory in which both of the above scripts can be found
 * Information on how often the job should be called
 * Information where in the workflow the new job needs to be inserted
 * In case it isn't clear, which job should resubmit the next run.

In general, a workflow can be defined in the runscript or in any component configuration file. But there are some restrictions to the
definition that needs to be taken into account:
 * The name of each job needs to be unique. Otherwise, an exception error will be raised.
 * The names of the default jobs are not allowed to be used for any new jobs. This will also cause an exception error during runtime.
 * Settings in the runscript will overwrite settings in other config files. (See also
   :ref:`yaml_hierarchy:Hierarchy of YAML configuration files`.)

Keywords to define a new workflow job
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
To provide the information about a new job the following keywords and mappings (key/value pairs) are available (keywords that are indicated
with ``< >`` need to be adapted by the user):

====================================================== ============ =========================== ==========================================================
Keyword                                                Mandatory    (Default) values            Function
====================================================== ============ =========================== ==========================================================
  ``workflow``                                         yes          --                          Chapter headline in a runscript or configuration section, 
                                                                                                indicating that alterations to the standard workflow 
                                                                                                will be defined here.

  ``subjobs``                                          yes          user defined string         Section within the ``workflow`` chapter that containes new 
                                                                                                additional workflow jobs.

  ``<new_job_name>``                                   yes          user defined string         Section within the ``subjobs`` section for each new job.
                                                                                                The name of the new job needs to be unique. See also further
                                                                                                explenation in :ref:`def_workflow_jobs`

  ``run_after: <value>`` or ``run_before: <value>``    no           default: last job in        Key/value entry in each ``job`` section. 
                                                                    (default) workflow          This mapping defines the (default or user) job of the 
                                                                    (e.g. tidy)                 workflow after or before the new job should be executed.
                                                                                                Only one of the two should be specified. 

  ``submit_to_batch_system: <value>``                  no           **false**, true             Key/value entry in each ``job`` section. 
                                                                                                This mapping defines if the (default or user) job is 
                                                                                                submitted to batch system or not.

  ``run_on_queue: <value>``                            no           None                        Key/value entry in each ``job`` section.
                                                                                                This mapping defines to which queue (name) the job 
                                                                                                should be submitted to.

  ``batch_or_shell: <value>``                          no           **shell**, batch            Key/value entry in each ``job`` section.
                                                                                                This mapping defines if the (default or user) job is submitted
                                                                                                as batch job or as shell script. 
                                                                                                This attribute will be overwritten depending on ``submit_to_batch_system``

  ``cluster: <value>``                                 no           Job name                    Key/value entry in each ``job`` section. Jobs
                                                                                                that have the same entry in ``cluster`` will be run 
                                                                                                from the same batch script.

  ``order_in_cluster: <value>``                        no           **sequential**, concurrent  Key/value entry in each ``job`` section. This mapping
                                                                                                defines how jobs in the same ``<cluster>`` should be run.
                                                                                                Concurrent or serial.

  ``script: <value>``                                  yes          None                        Key/value entry in each ``job`` section. 
                                                                                                This mapping defines the name of the script that is going 
                                                                                                to be executed during the new workflow job.

  ``script_dir: <value>``                              yes          None                        Key/value entry in each ``job`` section. 
                                                                                                This mapping defines the path to the script set by the variable
                                                                                                ``<script>``.

  ``call_function: <value>``                           no           None                        Key/value entry in each ``job`` section. 
                                                                                                This mapping defines the function within the script defined in
                                                                                                variable ``<script>`` should be executed.

  ``env_preparation: <value>``                         no           None                        Key/value entry in each ``job`` section. This
                                                                                                mapping defines e.g. a Python script/function that prepares 
                                                                                                a dictionary with environment variables.

  ``nproc: <int>``                                     no           1                           Key/value entry in each ``job`` section.
                                                                                                This mapping defines the number of CPUs a job should run with
                                                                                                (if run via sbatch).

  ``run_only: <value>``                                no           None                        Key/value entry in each ``job`` section.
                                                                                                This mapping defines when the job should be run. E.g. run only
                                                                                                at the beginning of a :term:`chunk` (set of runs).

  ``skip_chunk_number: <int>``                         no           None                        Key/value entry in each ``job`` section. This
                                                                                                mapping defines how many chunks should be skipped before the 
                                                                                                job will be execited.

  ``trigger_next_run: <value>``                        no           **false**, true             If job should trigger next run
====================================================== ============ =========================== ==========================================================

Syntax example
^^^^^^^^^^^^^^
The following code snippet shows the general syntax for defining a new workflow job:

.. code-block:: yaml

    workflow:
        subjobs:
            <job_name>:
                run_after: <value>
                submit_to_batch_system: <value>
                run_on_queue: <value>
                cluster: <value>
                order_in_cluster: <value>
                script: <value>
                call_function: <value>
                env_preparation: <value>
                nproc: <value>
                run_only: <value>
                skip_chunk_number: <value>
                trigger_next_run: <value>

Workflow defaults
-----------------

A minimal example of defining a new workflow job is given in Example 1. This will integrate a new job with the following default assumptions:

- The new job will be run after the last job of the default workflow.
- The script given for this job is run as a subprocess (not a batch run).
- The next run of the overall experiment will be (still) triggered by the last job of the default workflow and not the new job.


Examples for the definition of new workflow jobs
--------------------------------------------------

Example 1: Adding an additional postprocessing subjob
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the case of a simple postprocessing task (here for model Echam), that sould be run as the last task of each run, independantly from restarting the experiment,
the corresponding minimal code snippet in a runscript could look like this:

.. code-block:: yaml

    echam:
        [...other information...]

        workflow:
            subjobs:
                my_postprocessing:
                    script_dir: <value>
                    script: <values>


Example 2: Adding an additional preprocessing subjob
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A preprocessing job basically is configured the same way as a postprocessing job, but the run_before keyword is needed now, to define when the new job should
be run:

.. code-block:: yaml

    echam:
        [...other information...]

        workflow:
            subjobs:
                my_preprocessing:
                    run_before: prepcompute
                    script_dir: <value>
                    script: <values>

Example 3: Adding a new job as the last task in a run
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To integrate a new job that should be run as the last task in every run but before the next run starts, use the following example:

.. code-block:: yaml

    echam:
        [...other information...]

        workflow:
            subjobs:
                my_new_last_job:
                    script_dir: <value>
                    script: <values>
                    trigger_next_run: True

Example 4: Adding multiple user jobs that can be run concurrently in a workflow cluster
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is possible to define multiple new jobs that should start at the same but can be run independently from each other. This can be done by assigning these jobs
to the same workflow cluster and run them concurrently over the batch system:

.. code-block:: yaml

    echam:
        [...other information...]

        workflow:
            subjobs:
                my_new_last_job:
                    script_dir: <value>
                    script: <values>
                    submit_to_batch_system: True
                    run_on_queue: <value>
                    cluster: my_own_new_cluster

                my_second_new_job:
                    script_dir: <value>
                    script: <values>
                    submit_to_batch_system: True
                    run_on_queue: <value>
                    cluster: my_own_new_cluster


Example 5: Adding an iterative coupling job
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Writing a runscript for iterative coupling using the workflow manager requires some more changes. The principal idea is
that each coupling step consists of two data processing jobs, one pre- and one postprocessing job. This is done this way
as to make the coupling modular, and enable the modeller to easily replace one of the coupled components by a different
implementation. This is of course up to the user to decide, but we generally advise to do so, and the iterative couplings
distributed with `ESM-Tools` are organized this way. :

.. code-block:: yaml

    echam:
        [...other information...]

         workflow:
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
                    trigger_next_run: True

    fesom:
        [...other information...]

        workflow:
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
                    trigger_next_run: True
