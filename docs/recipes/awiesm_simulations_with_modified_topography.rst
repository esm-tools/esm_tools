AWI-ESM simulations with modified topography
============================================

Description
~~~~~~~~~~~
How to setup up a runscript to run an AWI-ESM simulation with modified topography

Preparation steps
~~~~~~~~~~~~~~~~~

- adapt the configuration for the simulation to the settings as outlined in the example below
    - adapt all your model boundary conditions to the paleogeography that the simulation shall reflect
    - ECHAM6 becomes unstable if orography is suddenly significantly changed; therefore:
        - put the full paleoboundary condition, with all fields adapted, into the YAML setting "target_oro"
        - also prepare a version of the jansurf file that contains all paleo-boundary conditions, but has GEOSP and OROXXX data sets unchanged from modern; this adapted paleoboundary condition file is to be used in option "jansurf"
    - if the change of orography shall be performed
        - set switch lupdate_orog to True and run the model for one year
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
        - Note: I also had lupdate_orography set, but Paul noted that this switch is not necessary; I kept it for the moment in this description as I did not test the impact of removing the switch
    - if the orography change has been sucessfully performed, and the switch setting lupdate_orog is changed to False, resubmit the simulation and let the model equilibrate to the full paleo-boundary condition; you should not find log messages of the kind described above in your log files anymore
    - Paul, a note: after switching off the orography adaptation, the ESM-tools still provide the jansurf-Version of the echam boundary condition to the work folder, not the one with the target orography. I assume this is not a problem, as the orography has made it into the restart file and is used from there. Or do I overlook a potential problem with this? I.e., do you know that any field is read from jansurf instead of from the restart file if a restart is performed? I checked the variables that I could, and slf, slm, glac, are identical in the output of echam6 as it should be the case based on the modified orography. wsmx and albedo are not the same though, but it may be that these are adapted throughout the simulation from the initial state; not an expert here. Anyway, the fact that unit.24 is not consistent with the orography in the simulation is kind of confusing. 

Example
~~~~~~~

.. code-block:: yaml

    echam:
    
        input_sources:
                jansurf: "XYZ/boundary_conditions/Pliocene_3Ma/echam6/T63CORE2_jan_surf.Pliocene_3Ma.nc" #boundary condition adapted to paleogeography
                vgratclim: "XYZ/boundary_conditions/Pliocene_3Ma/echam6/T63CORE2_VGRATCLIM.Pliocene_3Ma.nc" #boundary condition adapted to paleogeography
                vltclim: "XYZ/boundary_conditions/Pliocene_3Ma/echam6/T63CORE2_VLTCLIM.Pliocene_3Ma.nc" #boundary condition adapted to paleogeography
        # Code for bootstrapping:
    lupdate_orography: True #not needed, not of any effect according to Paul Gierz.
        add_input_sources:
            jansurf: "/albedo/home/stepanek/esm_tools_6/esm_tools/boundary_conditions/Pliocene_3Ma/echam6/T63CORE2_jan_surf.Pliocene_3Ma_modern_GEOSP.nc" #boundary condition adapted to paleogeography EXCEPT FOR GEOSP AND OROXXX VARIABLES, these are as per standard modern setup
            target_oro: "/albedo/home/stepanek/esm_tools_6/esm_tools/boundary_conditions/Pliocene_3Ma/echam6/T63CORE2_jan_surf.Pliocene_3Ma.nc" #boundary condition adapted to paleogeography, ALL FIELDS ADAPTED TO THE DESIRED PALEOGEOGRAPHY THAT ECHAM6 SHOULD CONSIDER
    
        add_namelist_changes:
                namelist.echam:
                    submodelctl:
    lupdate_orog: True
    
    hdmodel:
        add_input_sources:
                hdpara: "/albedo/home/stepanek/esm_tools_6/esm_tools/boundary_conditions/Pliocene_3Ma/hd/hdpara.Pliocene_3Ma.nc" #boundary condition adapted to paleogeography
    
    jsbach:
        input_sources:
                jsbach_1850: "/albedo/home/stepanek/esm_tools_6/esm_tools/boundary_conditions/Pliocene_3Ma/jsbach/jsbach_T63CORE2_11tiles_5layers_natural-veg.Pliocene_3Ma_semimoist.nc" #boundary condition adapted to paleogeography
    
    fesom:
        nx: 134288 #adapted to paleomesh
        mesh_dir: "XYZ/boundary_conditions/Pliocene_3Ma/fesom2/midpli2/" #paleomesh

