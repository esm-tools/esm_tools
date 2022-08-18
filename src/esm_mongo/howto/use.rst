Information items to describe a simulation via esm_database
===========================================================

.. use = for sections, ~ for subsections and - for subsubsections

**Feature available since version:** 6.13.3

There are various types of information that are needed to describe a simulation in a way so
that it is documented for later re-use and for the purpose of fulfilling the guidelines of
good scientific practice. Beyond the model data, that is obviously necessary to create derived
research and to understand simulation results, there is much more information necessary to make
a simulation re-usable and understandable. Such types of information include a reference to the
producer of the simulation, details of the computing environment, model, simulation setup, and
relevant simulation files including auxiliary files where applicable.

Here is an incomplete list that will be updated on demand.
- producer
- computing environment
  - the HPC where the simulation has been conducted
  - version of esm-tools employed and settings in the (dynamic) environment
- model
  - coupled model name / names of model components
  - version of models and, if applicable, of submodels
  - revision of the model / model versions
  - if applicable, documentation of changes to the code w.r.t. the revision
  - model source code
  - binaries (to enable, in the ideal case, a binary identical reproduction on the same machine)
  - model parameters, 

