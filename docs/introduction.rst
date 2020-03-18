.. highlight:: shell

============
Introduction
============

This is the user manual for the esm tools. To contribute to this document, please contact the authors for feedback.
    
The esm-tools are a collection of scripts to download, compile, configure different simulation models for the Earth system, such as atmosphere, ocean, geo-biochemistry, hydrology, sea-ice and ice-sheet models, as well as coupled Earth System Models (ESMs). They include functionality to write unified runscripts to carry out model simulations for different model setups (standalone and ESMs) on different HPC systems.

The ESM-Tools are divided into a number of python packages / git repositories, both to ensure stability of the code as well as reusability:

esm_tools.git
=============
The only repository to clone by hand by the user, ``esm_tools.git`` contains the subfolders

        **configs**: A collection of yaml configuration files, containing all the information needed by the python packages to work properly. This includes machine specific files (e.g. ``machines/mistral.yaml``) , model specific files (e.g ``fesom/fesom-2.0.yaml``), configurations for coupled setups (e.g. ``foci/foci.yaml``), but also files with the information on how a certain software works (``batch_systems/slurm.yaml``), and finally, how the esm_tools themselves are supposed to work (e.g. ``esm_master/esm_master.yaml``).


esm_master.git
==============
This repository contains the python files that give the ``esm_master`` executable in the subfolder ``esm_master``.


esm_runscripts.git
==================
The python package of the ``esm_runscripts`` executable. The main routines can be found in ``esm_runscripts/esm_sim_objects.py``.


esm_parser.git
==============
In order to provide the additional functionality to the ``yaml+`` configuration files (like choose blocks, simple math operations, variable expansions etc.). ``esm_parser`` is an extension of the pyyaml package, it needs the ``esm_calendar`` package to run, but can otherwise easily be used to add ``yaml+`` configurations to any python software.


esm_calendar.git
================







