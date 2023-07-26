==============
ESM Runscripts
==============

Usage
-----

::

    esm_runscripts [-h] [-d] [-v] [-e EXPID] [-c] [-P] [-j LAST_JOBTYPE]
                      [-t TASK] [-p PID] [-x EXCLUDE] [-o ONLY]
                      [-r RESUME_FROM] [-U]
                      runscript

Arguments
---------

====================================================== ==========================================================
Optional arguments                                     Description
====================================================== ==========================================================
  -h, --help                                           Show this help message and exit.
  -d, --debug                                          Print lots of debugging statements.
  -v, --verbose                                        Be verbose.
  -e ``EXPID``, --expid ``EXPID``                      The experiment ID to use. Default ``test``.
  -c, --check                                          Run in check mode (don't submit job to supercomputer).
  -P, --profile                                        Write profiling information (esm-tools).
  -j ``LAST_JOBTYPE``, --last_jobtype ``LAST_JOBTYPE`` Write the jobtype this run was called from (esm-tools internal).
  -t ``TASK``, --task ``TASK``                         The task to run. Choose from: ``compute``, ``post``, ``couple``, ``tidy_and_resubmit``.
  -p ``PID``, --pid ``PID``                            The PID of the task to observe.
  -x ``EXCLUDE``, --exclude ``EXCLUDE``                E[x]clude this step.
  -o ``ONLY``, --only ``ONLY``                         [o]nly do this step.
  -r ``RESUME_FROM``, --resume-from ``RESUME_FROM``    [r]esume from the specified run/step (i.e. to resume a second run you'll need to use ``-r 2``).
  -U, --update                                         [U]pdate the runscript in the experiment folder and associated files
  --update-filetypes                                   Updates the requested files from external sources in a currently ongoing simulation. We strongly advise against using this option unless you really know what you are doing.
  -i, --inspect                                        This option can be used to [i]nspect the results of a previous
                                                       run, for example one prepared with ``-c``. This argument needs an
                                                       additional keyword. Choose among: ``overview`` (gives you the
                                                       same litte message you see at the beginning of each run, ``lastlog``
                                                       (displays the last log file), ``explog`` (the overall experiment
                                                       logfile), ``datefile`` (the overall experiment logfile), ``config`` 
                                                       (the Python dict that contains all information), ``size`` (the size
                                                       of the experiment folder), a filename or a directory name output 
                                                       the content of the file /directory if found in the last 
                                                       ``run_`` folder.)
====================================================== ==========================================================


Running a Model/Setup
---------------------

`ESM-Runscripts` is the `ESM-Tools` package that allows the user to run the experiments.
`ESM-Runscripts` reads the runscript (either a `bash` or `yaml` file), applies the
required changes to the namelists and configuration files, submits the runs of the
experiment to the compute nodes, and handles and organizes restart, output and log files.
The command to run a runscript is::

 $ esm_runscripts <runscript.yaml/.run> -e <experiment_ID>

The ``runscript.yaml/.run`` should contain all the information regarding the experiment
paths, and particular configurations of the experiment (see the :ref:`yaml:Runscripts` section
for more information about the syntax of `yaml` runscripts). The ``experiment_ID`` is used
to identify the experiment in the scheduler and to name the experiment's directory (see
:ref:`esm_runscripts:Experiment Directory Structure`). Omitting the argument
``-e <experiment_ID>`` will create an experiment with the default experimant ID ``test``.

`ESM-Runscript` allows to run an experiment check by adding the ``-c`` flag to the previous
command. This check performs all the system operations related to the experiment that would
take place on a normal run (creates the experiment directory and subdirectories, copies the
binaries and the necessary restart/forcing files, edits the namelists, ...) but stops before
submitting the run to the compute nodes. We strongly recommend **running first a check before
submitting an experiment to the compute nodes**, as the check outputs contains already valuable
information to understand whether the experiment will work correctly or not (we strongly
encourage users to pay particular attention to the `Namelists` and the `Missing files` sections
of the check's output).

Job Phases
----------

.. graphviz:: graph/job_phases.dot
    :name: job_phases
    :caption: ESM-Tools job phases
    :alt: ESM-Tools job phases
    :align: center


The following table summarizes the job phases of `ESM-Runscripts` and gives a brief description.
...

Running only part of a job
--------------------------

It's possible to run only part of a job. This is particularly interesting for
development work; when you might only want to test a specific phase without
having to run a whole simulation.

As an example; let's say you only want to run the ``tidy`` phase of a
particular job; which will move things from the particular run folder to the
overall experiment tree. In this example; the experiment will be called ``test001``::

        esm_runscripts ${PATH_TO_USER_CONFIG} -t tidy_and_resubmit

Experiment Directory Structure
------------------------------

All the files related to a given experiment are saved in the `Experiment Directory`. This includes
among others model binaries, libraries, namelists, configuration files, outputs, restarts, etc.
The idea behind this approach is that all the necessary files for running an experiment are
contained in this folder (the user can always control through the runscript or configuration files
whether the large forcing and mesh files also go into this folder), so that the
experiment can be reproduced again, for example, even if there were changes into one of the
model's binaries or in the original runscript.

The path of the `Experiment Directory` is composed by the ``general.base_dir`` path specified in the
runscript (see :ref:`yaml:Runscripts` syntax) followed by the given ``experiment_ID`` during the
``esm_runscripts`` call::

    <general.base_dir>/<experiment_ID>

The **main experiment folder** (``General exp dir``) contains the subfolders indicated in the graph
and table below. Each of these subfolders contains a folder for each component in the experiment
(i.e. for an AWI-CM experiment the ``outdata`` folder will contain the subfolders ``echam``,
``fesom``, ``hdmodel``, ``jsbach``, ``oasis3mct``).

The structure of the **run folder** ``run_YYYYMMDD-YYYYMMDD`` (``Run dir`` in the graph) replicates
that of the general experiment folder. `Run` directories are created before each new run and they are
useful to debug and restart experiments that have crashed.

.. graphviz:: graph/exp_dir_struct.dot
    :name: exp_dir_structure
    :caption: Experiment directory structure
    :alt: Experiment directory structure
    :align: center

======================= ======================= ========================================================
Subfolder               Files                   Description
======================= ======================= ========================================================
analysis                user's files            Results of user's "by-hand" analysis can be placed here.

bin                     component binaries      Model binaries needed for the experiment.

config                  * <experiment_ID>_      Configuration files for the experiment including
                          finished_config.yaml  namelists and other files specified in the component's
                        * namelists             configuration files
                        * other configuration   (``<PATH>/esm_tools/configs/<component>/<component>.yaml``,
                          files                 see :ref:`yaml:File Dictionaries`).
                                                The file ``<experiment_ID>_finished_config.yaml`` is
                                                located at the base of the ``config`` folder and contains
                                                the whole ESM-Tools variable space for the experiment,
                                                resulting from combining the variables of the
                                                runscript, setup and component configuration files, and
                                                the machine environment file.

couple                  coupling related files  Necessary files for model couplings.

forcing                 forcing files           Forcing files for the experiment. Only copied here when
                                                specified by the user in the runscript
                                                or in the configuration files
                                                (:ref:`yaml:File Dictionaries`).

input                   input files             Input files for the experiment. Only copied here when
                                                specified by the user in the runscript
                                                or in the configuration files
                                                (:ref:`yaml:File Dictionaries`).

log                     * <experiment_ID>_      Experiment log files. The component specific log files
                          <setup_name>.log      are placed in their respective subfolder. The general
                        * component log files   log file ``<experiment_ID>_<setup_name>.log`` reports
                                                on the `ESM-Runscripts` :ref:`esm_runscripts:Job Phases`
                                                and is located at the base of the ``log`` folder. Log
                                                file names and copying instructions should be included
                                                in the configuration files of components
                                                (:ref:`yaml:File Dictionaries`).

mon                     user's files            Monitoring scripts created by the user can be placed here.

outdata                 outdata files           Outdata files are placed here. Outdata file names and
                                                copying instructions should be included in the
                                                configuration files of components
                                                (:ref:`yaml:File Dictionaries`).

restart                 restart files           Restart files are placed here. Restart file names and
                                                copying instructions should be included in the
                                                configuration files of components
                                                (:ref:`yaml:File Dictionaries`).

run_YYYYMMDD-YYYYMMDD   run files               Run folder containing all the files for a given run.
                                                Folders contained here have the same names as the ones
                                                contained in the general experiment folder (``analysis``,
                                                ``bin``, ``config``, etc). Once the run is finished
                                                the run files are copied to the general experiment folder.

scripts                 * ``esm_tools`` folder  Contains all the scripts needed for the experiment. A
                          containing:           subfolder ``esm_tools`` includes all the config files
                            * all namelists     and namelists of ``ESM-Tools`` (a copy of the ``configs``
                            * all functions     and ``namelists`` folders in the ``esm_tools``
                        * <experiment_ID>_      installation folder). It also contains the ``.run`` files
                          compute_YYYYMMDD-     to be submitted to `slurm`.
                          YYYYMMDD.run>         The file
                        * <experiment_ID>_      ``<experiment_ID>_compute_YYYYMMDD_YYYYMMDD_<JobID>.log``
                          compute_YYYYMMDD-     is the log file for the experiment run. The
                          YYYYMMDD_<JobID>.log  ``<experiment_ID>_<setup_name>.date`` indicates the
                        * <experiment_ID>_      finishing date of the last run.
                          <setup_name>.date
                        * original runscript
                        * file.log
                        * hostfile_srun

unknown                                         Folder where all the unknown files from
                                                ``run_YYYYMMDD_YYYYMMDD/work`` are copied.

viz                     user's files            Aimed for user's visualization scripts.

work                    * component files       The ``work`` folder inside the ``run_YYYYMMDD_YYYYMMDD``
                        * output files before   folder is the main directory where the components are
                          copied to the         executed. Output and restart files are generated here
                          ``output`` folder     before being copied to their respective folders.
                        * restart files before
                          copied to the
                          ``restart`` folder
======================= ======================= ========================================================

If one file was to be copied in a directory containing a file with the same name,
both files get renamed by the addition of their start date and end dates at the
end of their names (i.e. ``fesom.clock_YYYYMMDD-YYYYMMDD``).



.. Note::
   Having a `general` and several `run` subfolders means that files are duplicated and, when
   models consist of several runs, the `general` directory can end up looking very untidy.
   `Run` folders were created with the idea that they will be deleted once all files
   have been transferred to their respective folders in the `general` experiment directory.
   The default is not to delete this folders as they can be useful for debugging or
   restarting a crashed simulation, but the user can choose to delete them
   (see :ref:`esm_runscripts:Cleanup of \`\`run_\`\` directories`).

Cleanup of ``run_`` directories
-------------------------------

.. TODO: fix this and remove the text below    .. automethod:: esm_runscripts.tidy.clean_run_dir

This plugin allows you to clean up the ``run_${DATE}`` folders.
To do that you can use the following variables under the
``general`` section of your runscript (documentation follows order
of code as it is executed):

* ``clean_runs``: **This is the most important variable for most
  users**. It can take the following values:
    * ``True``: removes the ``run_`` directory after each run
      (**overrides every other** ``clean_`` **option**).

    * ``False``: does not remove any ``run_`` directory (default)
      if no ``clean_`` variable is defined.

    * ``<int>``: giving an integer as a value results in deleting
      the ``run_`` folders except for the last <int> runs
      (recommended option as it allows for debugging of crashed
      simulations).

  .. Note::
     ``clean_runs: (bool)`` is incompatible with
     ``clean_this_rundir`` and ``clean_runs: (int)`` is incompatible
     with ``clean_old_rundirs_except`` (an error will be raised
     after the end of the first simulation). The functionality of
     ``clean_runs`` variable **alone will suffice most of the
     standard user requirements**. If finer tunning for the removal
     of ``run_`` directories is required you can used the following
     variables instead of ``clean_runs``.

* ``clean_this_rundir``: (bool) Removes the entire run directory
  (equivalent to ``clean_runs: (bool)``). ``clean_this_rundir: True``
  **overrides every other** ``clean_`` **option**.

* ``clean_old_rundirs_except``: (int) Removes the entire run
  directory except for the last <x> runs (equivalent to
  ``clean_runs: (int)``).

* ``clean_old_rundirs_keep_every``: (int) Removes the entire
  run directory except every <x>th run. Compatible with
  ``clean_old_rundirs_except`` or ``clean_runs: (int)``.

* ``clean_<filetype>_dir``: (bool) Erases the run directory
  for a specific filetype. Compatible with all the other options.

* ``clean_size``: (int or float) Erases all files with size
  greater than ``clean_size``, must be specified in bytes! Compatible
  with all the other options.

**Example**

To delete all the ``run_`` directories in your experiment include this
into your runscript:

.. code-block:: yaml

   general:
           clean_runs: True

To keep the last 2 ``run_`` directories:

.. code-block:: yaml

   general:
           clean_runs: 2

To keep the last 2 runs and every 5 runs:

.. code-block:: yaml

   general:
           clean_old_rundirs_except: 2
           clean_old_rundirs_keep_every: 5

Debugging an Experiment
-----------------------

To debug an experiment we recommend checking the following files that you will find, either
in the `general` experiment directory or in the `run` subdirectory:

  * The `ESM-Tools` variable space file ``config/<experiment_ID>_finished_config.yaml``.
  * The run log file ``run_YYYYMMDD-YYYYMMDD/<experiment_ID>_compute_YYYYMMDD-YYYYMMDD_<JobID>.log```.
  
For interactive debugging, you may also add the following to the ``general`` section of your configuration file. 
This will enable the `pdb Python debugger <https://docs.python.org/3/library/pdb.html#debugger-commands>`_, and allow you to step through the recipe.

.. code-block:: yaml

    general: 
        debug_recipe: True
        
Setting the file movement method for filetypes in the runscript
---------------------------------------------------------------

By default, `esm_runscripts` copies all files initially into the first ``run_``-folder, and from there to ``work``. After the run, outputs, logs, restarts etc. are copied
from ``work`` to ``run_``, and then moved from there to the overall experiment folder. We chose that as the default setting as it is the safest option, leaving the user
with everything belonging to the experiment in one folder. It is also the most disk space consuming, and it makes sense to link some files into the experiment rather
than copy them.

As an example, to configure `esm_runscripts` for an echam-experiment to link the forcing and inputs, one can add the following to the runscript yaml file:

.. code-block:: yaml

        echam:
                file_movements:
                        forcing:
                                all_directions: "link"
                        input:
                                init_to_exp: "link"
                                exp_to_run: "link"
                                run_to_work: "link"
                                work_to_run: "link"

Both ways to set the entries are doing the same thing. It is possible, as in the ``input`` case, to set the file movement method independently for each of the
directions; the setting ``all_directions`` is just a shortcut if the method is identical for all of them.

Running an experiment with a virtual environment
-----------------------------------------------

Running jobs can optionally be encapsulated into a virtual environment.

To use a virtual environment run ``esm_runscripts`` with the flag
``--contained-run`` or set ``use_venv`` within the ``general`` section of your
runscript to ``True``:

.. code-block:: yaml

   general:
       use_venv: True

This shields the run from changes made to the remainder of the ESM-Tool installation,
and it's strongly recommended for production runs.

Before the first run, a local copy will be installed in the experiment tree,
and **that** installation will be used. For example, for a user ``miguel``
with a run with `expid` ``test`` ESM-Tools will be installed here::

     /scratch/miguel/test/.venv_esmtools/lib/python3.10/site-packages/esm_tools

instead of::

    /albedo/home/miguel/.local/lib/site-packages/esm_tools

If you choose to use a virtual environment, a folder named ``.venv_esmtools``
will be created at the root of your experiment. This contains all the Python
libraries used by ESM-Tools. The first installation induces some overhead (~2-3 minutes).

The virtual environment installs by default the ``release`` branch, pulling it directly
from our GitHub repository. You can choose to override this default by specifying another
branch, adding to your runscript:

.. code-block:: yaml

  general:
      install_esm_tools_branch: '<your_branch_name>'

.. warning::
   The branch **needs to exist on GitHub** as it is cloned form there, and **not from your
   local folder**. If you made any changes in your local branch make sure they are pushed before
   running esm_runscripts with a virtual environment, so that your changes are included in the
   virtual environment installation.

You may also select to install esm_tools in `editable mode`, in which case
they will be installed in a folder ``src/esm_tools/`` in the root of
your experiment. Any changes made to the code in that folder **will** influence how
ESM-Tools behave. To create a virtual environment with ESM-Tools installed in
`editable` mode use:

.. code-block:: yaml

   general:
       install_<esm_package>_editable: true/false

.. note::
   When using a virtual environment, config files and namelists will come of the
   folder .venv_esmtools listed above and **not** from your user install directory.
   You should make **all** changes to the namelists and config files via your user
   runscript. This is recommended in all cases!!!
