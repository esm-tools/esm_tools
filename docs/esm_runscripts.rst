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
  -r ``RESUME_FROM``, --resume-from ``RESUME_FROM``    [r]esume from this step.
  -U, --update                                         [U]pdate the runscript in the experiment folder and associated files.
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

        $ esm_runscripts ${PATH_TO_USER_CONFIG} -t tidy_and_resubmit

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

The main experiment folder contains the subfolders indicated in the graph and table below. Each of these
subfolders contains a folder for each component in the experiment (i.e. for an AWI-CM experiment the
``outdata`` folder will contain the subfolders ``echam``, ``fesom``, ``hdmodel``, ``jsbach``,
``oasis3mct``).

.. graphviz::
    :name: exp_dir_structure
    :caption: Experiment directory structure
    :alt: Experiment directory structure
    :align: center

     digraph "file_hierachy" {
         size="10.0";
         graph [fontname="Verdana", fontsize="12"];
         node [fontname="Verdana", fontsize="12"];
         edge [fontname="Sans", fontsize="12"];
         #newrank=true;
         rankdir="TB";
         compound=true;
         #splines=ortho;

         #rank = same; ane; anr;
         #rank = same; bie; bir;

         subgraph cluster0
         {
             label="General exp dir";
             node [style=filled];
             fontname="bold";
             color=black;

             ane [label="analisys", shape="folder", fillcolor="gray", style=filled];
             bie [label="bin", shape="folder", fillcolor="gray", style=filled];
             cfe [label="config", shape="folder", fillcolor="gray", style=filled];
             cpe [label="couple", shape="folder", fillcolor="gray", style=filled];
             foe [label="forcing", shape="folder", fillcolor="gray", style=filled];
             ine [label="input", shape="folder", fillcolor="gray", style=filled];
             loe [label="log", shape="folder", fillcolor="gray", style=filled];
             moe [label="mon", shape="folder", fillcolor="gray", style=filled];
             oue [label="outdata", shape="folder", fillcolor="gray", style=filled];
             ree [label="restart", shape="folder", fillcolor="gray", style=filled];
             rue [label="run_<DATE>", shape="folder", fillcolor="gray", style=filled];
             sce [label="scripts", shape="folder", fillcolor="gray", style=filled];
             une [label="unknown", shape="folder", fillcolor="gray", style=filled];
             vie [label="viz", shape="folder", fillcolor="gray", style=filled];
             woe [label="work", shape="folder", fillcolor="gray", style=filled];

             ane -> bie -> cfe -> cpe -> foe -> ine -> loe -> moe -> oue -> ree -> rue -> sce -> une -> vie -> woe[style=invis];
         }

         subgraph cluster1
         {
             label="Run dir";
             node [style=filled];
             fontname="bold";
             style=filled;
             color=black;
             fillcolor=gray;

             anr [label="analisys", shape="folder", fillcolor="white", style=filled];
             bir [label="bin", shape="folder", fillcolor="white", style=filled];
             cfr [label="config", shape="folder", fillcolor="white", style=filled];
             cpr [label="couple", shape="folder", fillcolor="white", style=filled];
             for [label="forcing", shape="folder", fillcolor="white", style=filled];
             inr [label="input", shape="folder", fillcolor="white", style=filled];
             lor [label="log", shape="folder", fillcolor="white", style=filled];
             mor [label="mon", shape="folder", fillcolor="white", style=filled];
             our [label="outdata", shape="folder", fillcolor="white", style=filled];
             rer [label="restart", shape="folder", fillcolor="white", style=filled];
             scr [label="scripts", shape="folder", fillcolor="white", style=filled];
             unr [label="unknown", shape="folder", fillcolor="white", style=filled];
             vir [label="viz", shape="folder", fillcolor="white", style=filled];
             wor [label="work", shape="folder", fillcolor="white", style=filled];

             anr -> bir -> cfr -> cpr -> for -> inr -> lor -> mor -> our -> rer -> scr -> unr -> vir -> wor [style=invis];
         }
         rue -> unr [lhead=cluster1, dir=none, style=dashed];
         #loe -> lor[style=invis];
         #moe -> mor[style=invis];
         #woe -> wor[style=invis];
         #ane -> anr[style=invis];
         #une -> unr[style=invis];
         #vie -> vir[style=invis];

         "namelists" [shape="note"]
         "binaries" [shape="note"]

         subgraph cluster2
         {
              color=white;

              "restart files" [shape="note"]
              "output files" [shape="note"]

              "output files" -> "restart files" [style=invis]
         }

         subgraph cluster3
         {
             color=white

             "ESM-Tools" [shape="folder"]
             "Model installation folder" [shape="folder"]
         }
         "ESM-Tools" -> namelists [dir=none]
         namelists -> wor
         "Model installation folder" -> binaries [dir=none]
         binaries -> wor

         #ree -> "restart files" -> wor [style=invis]
         #ree -> "output files" -> wor [style=invis]

         #loe -> "output files" -> our [style=invis]

         "restart files" -> wor
         "restart files" -> ree
         "output files" -> wor [dir=none]
         "output files" -> oue
     }

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
                                                files the run files are copied to the general
                                                experiment folder.

scripts                 * ``esm_tools`` folder  Contains all the scripts needed for the experiment. A
                          containing:           subfolder ``esm_tools`` includes all the config files
                            * all namelists     and namelists of ``ESM-Tools`` (a copy of the ``configs``
                            * all functions     and ``namelists`` folders in the ``esm_tools``
                        * <experiment_ID>_      installation folder). It also contains the ``.sad`` files
                          compute_YYYYMMDD-     to be submitted to `slurm`.
                          YYYYMMDD.sad>         The file
                        * <experiment_ID>_      ``<experiment_ID>_compute_YYYYMMDD_YYYYMMDD_<JobID>.log``
                          compute_YYYYMMDD-     is the log file for the experiment run. The
                          YYYYMMDD_<JobID>.log  ``<experiment_ID>_<setup_name>.date`` indicates the
                        * <experiment_ID>_      finishing date of the last run.
                          <setup_name>.date
                        * original runscript
                        * file.log
                        * hostfile_srun

unknown                                         Folder where all the unknown files should be copied.

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

The structure of the ``run_YYYYMMDD-YYYYMMDD`` directory replicates that of the general
experiment folder. `Run` directories are created before each new run and they are
useful to debug and restart experiments that have crashed.

.. Note::
   Having a `general` and several `run` subfolders means that files are duplicated and, when
   models consist of several runs, the `general` directory can end up looking very untidy.
   `Run` folders were created with the idea that they will be deleted once all files
   have been transferred to their respective folders in the `general` experiment directory.
   Currently, that is not the case due to potential issues when there is a need for
   restarting a crashed simulation: a faulty tidy-up (i.e. due to a run crash, some run
   files are updated but others not) will lead to a faulty restart. In the the near
   future the user will have control on whether keeping the `run` folders or deleting them,
   through a variable in the runscript.

.. check that the above is changed by the merge of develop in release 5.0, so that it includes
   the delete file functionality. 

Debugging an Experiment
-----------------------

To debug an experiment we recommend checking the following files that you will find, either
in the `general` experiment directory or in the `run` subdirectory:

  * The `ESM-Tools` variable space file ``config/<experiment_ID>_finished_config.yaml``.
  * The run log file ``run_YYYYMMDD-YYYYMMDD/<experiment_ID>_compute_YYYYMMDD-YYYYMMDD_<JobID>.log```.
