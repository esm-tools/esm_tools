===========================================
ESM Runscripts - Using the Workflow Manager
===========================================

Introduction
------------

Starting with Release 6.0, esm_runscripts allows the user to define additional subjobs/phases for e.g. data processing. Such subjobs can be arranged into clusters, and the order of execution of these and the standard runjob parts can be set in a flexible and short way from the runscript. This is applicable for both pre- and postprocessing, but especially useful for iterative coupling jobs, like e.g. coupling pism to vilma (see below). In this section we explain the basic concept, and the keywords that have to be set in the runscript to make use of this feature.

Default subjobs/phases of a general model simulation run
--------------------------------------------------------

.. Even before the addition of the workflow manager, the run jobs of esm_runscript were split into different subjobs, even though that was mostly hidden from the user's view. Before
.. Release 6.0, these subjobs were:

.. ::

        compute --> tidy_and_resubmit (incl. wait_and_observe + resubmit next run)

.. Technically, ``wait_and_observe`` was part of the tidy_and_resubmit job, as was the resubmission, including above only for the purpose of demonstrating the difference to the 
Since Release 6.0 the default workflow phases are the following::

        newrun --> prepcompute --> compute --> observe_compute --> tidy (+ resubmit next run)

.. Other than before adding the workflow manager, 
These standard subjobs/phases are all separated and independant subjobs, each submitted (or started) by the previous subjob in one of three ways (see below). The splitting of the old compute job into newrun, prepcompute and compute on one side, and tidy_and_resubmit into observe and tidy, was necessary to enable the user to insert coupling subjobs for iterative coupling at the correct places. Here is what each of the standard subjobs does:


====================================================== ==========================================================
Subjob                                                 Function
====================================================== ==========================================================
  newrun                                               Initializes a new experiment, only very basic stuff, like
                                                       creating (empty) folders needed by any of the following 
                                                       subjobs. NEEDS TO BE THE FIRST SUBJOB OF ANY EXPERIMENT.
  prepcompute                                          Prepares the compute job. All the (Python) functionality that
                                                       needs to be run, up to the job submission. Includes copying
                                                       files, editing namelists, write batch scripts, etc.
  compute                                              Actual model integration, nothing else. No Python codes
                                                       involved.
  observe_compute                                      Python job running at the same time as compute, checking if
                                                       the compute job is still running, looking for some known 
                                                       errors for monitoring / job termination.
  tidy                                                 Sorts the produced outputs, restarts and log files into 
                                                       the correct folders, checks for missing and unknown files,
                                                       builds coupler restart files if not present
====================================================== ==========================================================

It is important to understand that none of this has to be edited by the users, this is the default setup. 


Defining additional workflow subjobs/phases
-------------------------------------------

The workflow manager is intended to include shell scripted data processing jobs into the
esm_runscripts workflow, so several things have to be defined:

 * Name of the script to be run
 * Name of the python script used for setting up the environment
 * Name of the folder in which both of the above scripts can be found
 * Information on how often the subjob should be called
 * Information on between which other subjobs the new subjob should be inserted into the workflow
 * In case it isn't clear: Which subjob should resubmit the next run.

The keywords used to define that are:

====================================================== ==========================================================
Keyword                                                Function
====================================================== ==========================================================
  workflow                                             Chapter headline in a model's section, indicating that
                                                       alterations to the standard workflow will be defined here
  subjob_clusters                                      Section in the workflow chapter, containing the information
                                                       on additional subjob_clusters. A subjob_cluster is a
                                                       collection of subjobs run from the same batch script. Each
                                                       subjob needs to belong to one cluster, if none is defined, 
                                                       each subjob will automatically get assigned to its own
                                                       cluster. Each entry in ``subjob_clusters`` is a dict,
                                                       with the outermost key being the (arbitrary) name of the
                                                       cluster.
  subjobs                                              Section in the workflow chapter, containing the information
                                                       on additional subjobs. 
  run_after / run_before                               Entry in specifications of a subjob_cluster, to define
                                                       before or after which other cluster of the workflow this cluster
                                                       is supposed to run. Only one of the two should be specified.
                                                       Can also be used in the specifications of subjobs if these
                                                       subjobs get a corresponding cluster auto-assigned.
  script:                                              Name of the script that is going to be executed during the new
                                                       workflow phase.
  script_dir:                                          Path to the script defined by the variable ``script``.
  call_function:
  env_preparation:                                     E.g. a Python script/function that prepares a dictionary with
                                                       environment variables.
  next_run_triggered_by:                               The phase/subjob that should start the next run.
====================================================== ==========================================================




Example 1: Adding an additional postprocessing subjob
-----------------------------------------------------


 In the case of a simple echam postprocessing job, the corresponding section in the runscript could look like this::


    echam:
        [...other information...]

        workflow:
            next_run_triggered_by: tidy
                    
            subjobs:
                my_new_subjob:
                    nproc: 1
                    run_after: tidy
                    script_dir:
                    script:
                    call_function:
                    env_preparation:



Example 2: Adding an additional preprocessing subjob
-----------------------------------------------------

A preprocessing job basically is configured the same way as a postprocessing job, but the run_after entry is repl




Example 3: Adding an iterative coupling job
-------------------------------------------

Writing a runscript for iterative coupling using the workflow manager requires some more changes. The principal idea is
that each coupling step consists of two data processing jobs, one pre- and one postprocessing job. This is done this way
as to make the coupling modular, and enable the modeller to easily replace one of the coupled components by a different
implementation. This is of course up to the user to decide, but we generally advise to do so, and the iterative couplings
distributed with ESM-Tools are organized this way.
::
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
