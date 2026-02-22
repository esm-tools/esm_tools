AWI-ESM1/2 simulations with modified topography
===============================================

Description
~~~~~~~~~~~
How to setup up a runscript to run an AWI-ESM1 / AWI-ESM2 simulation with modified topography.

.. note:: ECHAM6 becomes unstable if orography is suddenly significantly changed. Via the methodology provided below you can get your simulation into a stable state. The idea is that the perturbation of topography is not applied instantaneously but rather iteratively over one model year, thereby keeping the model in a stable numeric state.

Preparation steps
~~~~~~~~~~~~~~~~~
In order to set up a simulation with AWI-ESM1 or AWI-ESM2 (ECHAM6 as atmosphere model component) and modified topography, please do the following preparation steps that perform a bootstrapping of the atmosphere model for modified orography:

* Adapt all your model boundary conditions to the paleogeography that the simulation shall reflect, you may apply anomaly or absolut approaches.
* Adapt the configuration for the simulation to the settings as outlined in the example below.
* To avoid ECHAM6 becoming unstable if orography is suddenly significantly changed, 

  * put ``jansurf`` file with the full paleoboundary condition, with all fields adapted, into the YAML setting ``target_oro`` under ``echam`` -> ``add_input_sources``;
  * also prepare a version of the ``jansurf`` file that contains all paleo-boundary conditions, but has ``GEOSP`` and ``OROXXX`` data sets unchanged from modern; this adapted paleoboundary condition file is to be used in YAML setting ``jansurf`` under ``echam`` -> ``add_input_sources``.
* If the change of orography shall be performed,

  * set the namelist switch ``lupdate_orog`` under ``add_namelist_changes`` -> ``namelist.echam`` -> ``submodelctl`` to ``True``;
    
    This switch will trigger ECHAM6 to read both ``target_oro`` and ``jansurf``, using the latter as initial condition and then progressively adapting orography towards the condition in the former to reach a stable atmosphere state that is based on your modified orography.
  * set the namelist switch ``lupdate_orography`` under ``echam`` to ``True``; 

    This triggers esm-tools to copy the additional boundary condition files to the work directory.
  * run the model for one year;
  * you may check that orography is adapted by searching for the following (and similar) strings in the log file of the awi-esm_compute of the ongoing run::

          0:  PG: END of routine >>>read_target_orog_fields<<<
          0:  PG: START of routine >>>calculate_adjustment_step_size<<<
          0:  number_years =            1
          0:  PG: There are this many timesteps:        70080
          0:  PG: END of routine >>>calculate_adjustment_step_size<<<
          0:  PG: Start of routine >>>update_orography<<<
          0:  PG: Start of stepwise_update_orog_variable
          0:  PG: END of stepwise_update_orog_variable
          0:  PG: Start of stepwise_update_orog_variable
          0:  PG: END of stepwise_update_orog_variable
          0:  PG: Start of stepwise_update_orog_variable
          0:  PG: END of stepwise_update_orog_variable
          0:  PG: Start of stepwise_update_orog_variable
          0:  PG: END of stepwise_update_orog_variable
          0:  PG: Start of stepwise_update_orog_variable
          0:  PG: END of stepwise_update_orog_variable
          0:  PG: Start of stepwise_update_orog_variable
          0:  PG: END of stepwise_update_orog_variable
          0:  PG: Start of stepwise_update_orog_variable
          0:  PG: END of stepwise_update_orog_variable
          0:  PG: Start of stepwise_update_orog_variable
          0:  PG: END of stepwise_update_orog_variable
          0:  PG: The current difference from target in geosp is: 45556.2912600568
          0:  PG: END of routine >>>update_orography<<<

  * after this one year simulation with adaptation of orography to the target condition has finished, stop the simulation; 
    
    Now you have produced a restart file that contains your target orography and a stable atmosphere state. This restart file may be used to initialize other simulations with your target orography or to simply continue your simulation. Regarding the latter:
  * if the orography change has been sucessfully performed, then switch the setting ``lupdate_orog`` to ``False`` again, resubmit the simulation, and let the model equilibrate to the full paleo-boundary condition; 

    You should not find any more log messages of the kind described above in your log files anymore.

Example
~~~~~~~

The following example YAML, that refers to a simulation that has been adapted to Pliocene geography, shows code snippets of additional changes to the configuration in order to activate a modified topography; these additional settings are shown in the context of other conventional YAML settings.

.. code-block:: yaml

    echam:
    
        input_sources:
            jansurf: "<path_to>/boundary_conditions/Pliocene_3Ma/echam6/T63CORE2_jan_surf.Pliocene_3Ma.nc" # boundary condition adapted to paleogeography
            vgratclim: "<path_to>/boundary_conditions/Pliocene_3Ma/echam6/T63CORE2_VGRATCLIM.Pliocene_3Ma.nc" # boundary condition adapted to paleogeography
            vltclim: "<path_to>/boundary_conditions/Pliocene_3Ma/echam6/T63CORE2_VLTCLIM.Pliocene_3Ma.nc" # boundary condition adapted to paleogeography
    # Code for bootstrapping:
        lupdate_orography: True #make esm-tools copy additional boundary conditions to the work directory
        add_input_sources: #define both your target boundary condition and an initialization version of it, where the latter contains modern orography
            jansurf: "<path_to>/boundary_conditions/Pliocene_3Ma/echam6/T63CORE2_jan_surf.Pliocene_3Ma_modern_GEOSP.nc" # boundary condition adapted to paleogeography EXCEPT FOR GEOSP AND OROXXX VARIABLES, these are as per standard modern setup
            target_oro: "<path_to>/boundary_conditions/Pliocene_3Ma/echam6/T63CORE2_jan_surf.Pliocene_3Ma.nc" # boundary condition adapted to paleogeography, ALL FIELDS ADAPTED TO THE DESIRED PALEOGEOGRAPHY THAT ECHAM6 SHOULD CONSIDER
    
        add_namelist_changes:
            namelist.echam:
                submodelctl:
                    lupdate_orog: True #activates adaptation of orography from the initialisation state to the target state
    
    hdmodel:
        add_input_sources:
            hdpara: "<path_to>/boundary_conditions/Pliocene_3Ma/hd/hdpara.Pliocene_3Ma.nc" #boundary condition adapted to paleogeography
    
    jsbach:
        input_sources:
            jsbach_1850: "<path_to>/boundary_conditions/Pliocene_3Ma/jsbach/jsbach_T63CORE2_11tiles_5layers_natural-veg.Pliocene_3Ma_semimoist.nc" #boundary condition adapted to paleogeography
    
    fesom:
        nx: 134288 #adapted to paleomesh
        mesh_dir: "<path_to>/boundary_conditions/Pliocene_3Ma/fesom2/midpli2/" #paleomesh

