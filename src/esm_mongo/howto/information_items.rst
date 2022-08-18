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

Here is an incomplete list of information that is useful and/or necessary to understand how a
simulation was created and on which conditions the modelling results depend. This list will be updated on demand.
- producer

  - real name of producer
  - user name on HPC
  - contact email
  - affiliation (e.g. section Paleoclimate Dynamics, section Climate Dynamics)

- computing environment

  - HPC where the simulation has been conducted
  - version of esm-tools employed
  - settings within the (dynamic) environment at the time when the simulation ran

- model

  - coupled model name / names of model components
  - major version of models and, if applicable, of submodels (e.g. echam 6.3.04)
  - revision of the model / model versions identifying changes relative to the major version
  - if applicable, documentation of changes to the code w.r.t. the revision (e.g. not commited changes to the model source code)
  - model source code (or a diff to a checked in reference version)
  - binaries (to enable, in the ideal case, a binary identical continuation of / reproduction of (parts of) the simulation on the same machine)
  - model parameters (those set outside experiment YAML and chosen scenario - potentially set at compile time)

- simulation

  - simulation name
  - simulation description - this can be extensive and should provide enough information to understand why a simulation has been made and what its characteristics are:

    - purpose of the simulation
    - a short outline explaining most relevant details of the simulation setup

      - description of / references to greenhous gases, orbital forcing, etc.
      - description of / references to details of the geographic settings, including the underlying data sets
      - reference to time period (PI, Miocene, Pliocene)
      - time period in years (e.g. 1850 or -3000000 to provide more context within a time period)

  - any relevant deviations from standard model parameters (e.g. modified mixing scheme)
  - "flavor" of the simulation (equilibrium spinup, transient simulation)
  - if transient, references to the forcing / files relevant for the forcing of

    - greenhouse gases
    - orbit
    - freshwater

  - suggested restart year if the simulation should be continued
  - simulation run time (e.g. model year 2000-2500)
  - suggested analysis period (e.g. model year 2400-2500)
  - spatial and vertical resolution of the model components, e.g. T63L47/CORE2L40
  - mesh path
  - employed scenario
  - references to auxiliary documentation (e.g. publications where data has been used or the simulation is documented)
  - simulation YAML
  - path to simulation files (input, outdata, restart, initialization restarts, log, etc.)
  - unique identifier of the simulation (automatically created by esm_database)

- context of the simulation

  - affiliation to a project (PMIP5, CMIP6, PalMod2, ...)
  - reference simulation (identifier of a simulation / of simulations suggested for computation of anomalies - e.g. a PI simulation that has compatible settings)
  - suggested time period of reference simulation for computing anomalies (to avoid reaching different conclusions from one and the same set of simulations due to internal variability)
  - parent simulation (the identifier of the simulation from which branch-off occured)

- quality status (running, finished, in analysis, in review, published, retracted)

Some of these fields have to be explicitly filled, some information items will be provided automatically via parsing of the simulation's setup files.  

