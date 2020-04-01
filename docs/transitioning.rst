.. highlight:: shell

=====================================
Transitioning from the Shell Version
=====================================

ESM-Master
----------

The Makefile based esm_master of the shell version has been replaced by a (python-based) executable called ``esm_master`` that should be in your PATH after installing the new tools. The command can be called from any place now, models will be installed in the current work folder. The old commands are replaced by new, but very similar calls::

   OLD WAY:                            NEW WAY:
   make                   -->          esm_master                    (to get the list of available 
                                                                      targets)
   make get-fesom-1.4     -->          esm_master get-fesom-1.4      (download)
   make conf-...          -->          esm_master conf-...           (configure)
   make comp-...          -->          esm_master comp-...           (compile)
   make clean-...         -->          esm_master clean-...          (clean)

Apart from that, the new esm_master offers certain new functionality:: 

   esm_master fesom          (lists all available targets containing the string "fesom")
   esm_master install-...    (shortcut for: get- , then conf- , then comp-)
   esm_master recomp-...     (shortcut for: conf-, then clean-, then comp-)
   esm_master log-...        (overview over last commits of the model, e.g. git log)
   esm_master status-...     (changes in the model repository since last commit, e.g. git status)

If the user wants to define own shortcut commands, that can be done by editing ``esm_tools/configs/esm_master/esm_master.yaml``. New wrappers for the version control software can be e.g. added in ``esm_tools/configs/vcs/git.yaml``. Adding commands in these configuration files is sufficient that they show up in the list of targets.

The details about models, setups, etc. are now to be found in ``esm_tools/configs/esm_master/setups2models.yaml``. This file is a strucutred list instead of a barely readable, and rapidly growing, makefile. If you want to change details of your model, or add new components, this is where it should be put. Please refer to the chapter ESM-Master down below for further details.


ESM-Environment
---------------

A visible tool, like esm-environment used to be, doesn't exist anymore. The information about the environment needed for compiling / running a model is contained:

* in the machine yaml file (e.g. ``esm_tools/configs/machines/ollie.yaml``): This contains a default environment that we know works for a number of models / setups, but maybe not in an optimal way,
* in the model yaml file (e.g. ``esm_tools/configs/fesom/fesom-2.0.yaml``): The model files are allowed to contain deviations from the default environment defined in the machine file, indicated by the keywords ``environment_changes``, ``compiletime_environment_changes`` or ``runtime_environment_changes``.

Please note that even though there still is a python package called ``esm_environment``, this is just the collection of python routines used to assemble the environment. It does not contain anything to be configured by the user.


ESM-Runscripts
--------------

One main thing that has changed for the runtime tool is the way it is evoked::

   OLD WAY:                                NEW WAY:
   ./runscriptname -e experiment_id        esm_runscripts runscriptname -e experiment_id

Instead of calling your runscript directly, it is now interpreted and executed by the wrapper ``esm_runscripts``, the second executable to be added to your PATH when installing the Tools. Internally, ``esm_runscripts`` reads in the script file line by line and converts it into a python dictionary. It is therefore also possible to write the "runscripts" in the form of a yaml file itself, which can be imported by python much easier. The user is invited to try the yaml-style runscripts, some example can be found in ``esm_tools/runscripts``.

Some of the variables which had to be set in the script when using the shell version are now deprecated, these include:

* FUNCTION_PATH
* FPATH
* machine

Also the last two lines of the normel runscript for the shell version of the tools, ``load_all_functions`` and ``general_do_it_all``, don't do anything anymore, and can be safely removed. They don't hurt though.

(...to be continued...)


Functions --> Configs + Python Packages
---------------------------------------

The shell functions, which used to be in ``esm-runscripts/functions/all``, are gone. That was basically the whole point of re-coding the tools, to get rid of this mixture of model configuration, wild shell hacks, and in general lots of annoying problems. What used to be in the functions is now seperated into python code (which is actually doing things, but doesn't have any model-, setup- or machine specific information), and yaml configurations (which are basically structured lists of all the information we have, including mesh resolutions, scenario simulation forcings,... Anything really that you could possibly know about running a simulation belongs into the yaml configs that you can now find in ``esm_runscripts/configs``, while ESM-Tools functionality is coded in the python packages.




Namelists
---------

No changes. Namelists can be found in ``esm_tools/namelists``.




