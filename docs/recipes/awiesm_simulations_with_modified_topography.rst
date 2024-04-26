AWI-ESM simulations with modified topography
============================================

Description
~~~~~~~~~~~
How to setup up a runscript to run an AWI-ESM simulation with modified topography.

Preparation steps
~~~~~~~~~~~~~~~~~
In order to set up a simulation with AWI-ESM and modified topography, please do the following preparation steps:

- Adapt the configuration for the simulation to the settings as outlined in the example below.
- Adapt all your model boundary conditions to the paleogeography that the simulation shall reflect.
- ECHAM6 becomes unstable if orography is suddenly significantly changed. Therefore, 
    - put the full paleoboundary condition, with all fields adapted, into the YAML setting ``target_oro``
    - also prepare a version of the ``jansurf`` file that contains all paleo-boundary conditions, but has ``GEOSP`` and ``OROXXX`` data sets unchanged from modern; this adapted paleoboundary condition file is to be used in option ``jansurf``
- if the change of orography shall be performed
    - set the varibale ``lupdate_orog`` to ``True`` and run the model for one year
    - you may check that orography is adapted by searching for the following (and similar) strings in the log file of the awi-esm_compute of the ongoing run::

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

    - after the year with adaptation of orography to the target condition has finished, stop the simulation;
    - Note: The set of the variable ``lupdate_orography`` to ``True`` may not be necessary. 

      ..  but Paul noted that this switch is not necessary; I kept it for the moment in this description as I did not test the impact of removing the switch
- if the orography change has been sucessfully performed, and the switch setting ``lupdate_orog`` is changed to ``False`` again, resubmit the simulation and let the model equilibrate to the full paleo-boundary condition; you should not find log messages of the kind described above in your log files anymore

Example
~~~~~~~

The following example shows code snippets of additional changes to the configuration in order to activate a modified topography.

.. code-block:: yaml

    echam:
    
        input_sources:
            jansurf: "<path_to>/boundary_conditions/Pliocene_3Ma/echam6/T63CORE2_jan_surf.Pliocene_3Ma.nc" # boundary condition adapted to paleogeography
            vgratclim: "<path_to>/boundary_conditions/Pliocene_3Ma/echam6/T63CORE2_VGRATCLIM.Pliocene_3Ma.nc" # boundary condition adapted to paleogeography
            vltclim: "<path_to>/boundary_conditions/Pliocene_3Ma/echam6/T63CORE2_VLTCLIM.Pliocene_3Ma.nc" # boundary condition adapted to paleogeography
    # Code for bootstrapping:
        lupdate_orography: True #not needed, not of any effect according to Paul Gierz.
        add_input_sources:
            jansurf: "<path_to>/boundary_conditions/Pliocene_3Ma/echam6/T63CORE2_jan_surf.Pliocene_3Ma_modern_GEOSP.nc" # boundary condition adapted to paleogeography EXCEPT FOR GEOSP AND OROXXX VARIABLES, these are as per standard modern setup
            target_oro: "<path_to>/boundary_conditions/Pliocene_3Ma/echam6/T63CORE2_jan_surf.Pliocene_3Ma.nc" # boundary condition adapted to paleogeography, ALL FIELDS ADAPTED TO THE DESIRED PALEOGEOGRAPHY THAT ECHAM6 SHOULD CONSIDER
    
        add_namelist_changes:
            namelist.echam:
                submodelctl:

    lupdate_orog: True
    
    hdmodel:
        add_input_sources:
            hdpara: "<path_to>/boundary_conditions/Pliocene_3Ma/hd/hdpara.Pliocene_3Ma.nc" #boundary condition adapted to paleogeography
    
    jsbach:
        input_sources:
            jsbach_1850: "<path_to>/boundary_conditions/Pliocene_3Ma/jsbach/jsbach_T63CORE2_11tiles_5layers_natural-veg.Pliocene_3Ma_semimoist.nc" #boundary condition adapted to paleogeography
    
    fesom:
        nx: 134288 #adapted to paleomesh
        mesh_dir: "<path_to>/boundary_conditions/Pliocene_3Ma/fesom2/midpli2/" #paleomesh

