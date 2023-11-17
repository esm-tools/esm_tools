.. highlight:: shell

============
Introduction
============

Welcome to the user documentation of the `ESM-Tools` software. 
    
`ESM-Tools` is a collection of command-line tools to download and build different simulation models for the Earth system, such as atmosphere, ocean, biogeochemistry, hydrology, sea-ice and ice-sheet models, as well as coupled Earth System Models (ESMs). It also includes the functionality to write unified runscripts to carry out model simulations for different model setups (standalone and ESMs) on different High Performance Computing (HPC) systems.

`ESM-Tools` consists of the following command line tools:

+----------------+----------------------------------------------+--------------------------------------------+
| Tool           | Description                                  | Documentation                              |
+================+==============================================+============================================+
| esm_tools      | Basic command to return information about    | :ref:`esm_tools:ESM Tools`                 |
|                | `ESM-Tools` and create config files.         |                                            |
+----------------+----------------------------------------------+--------------------------------------------+
| esm_master     | Command to download and build a model or ESM | :ref:`esm_master:ESM Master`               |
+----------------+----------------------------------------------+--------------------------------------------+
| esm_runscripts | Command to execute a simulation              | :ref:`esm_runscripts:ESM Runscripts`       |
+----------------+----------------------------------------------+--------------------------------------------+

A short roadmap on how to run a model or ESM with `ESM-Tools` is given in the section :ref:`ten_steps:Ten Steps to a Running Model`.

`ESM-Tools` strongly seperates back-end code and the configuration of model or machine dependent information. A description about the syntax of these configuation files is given in the section :ref:`yaml:YAML File Syntax`.

A list of supported model components are given in the section :ref:`Supported_Models:Supported Models`.

You are very welcome to contribute to this document. Please find more information on how to contribute `here <https://github.com/esm-tools/esm_tools/blob/release/CONTRIBUTING.rst>`_ and contact the authors for feedback.
