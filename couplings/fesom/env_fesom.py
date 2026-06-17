def prepare_environment(config):
    environment_dict = {
            "ICE_TO_FESOM": int(config["fesom"].get("use_icebergs", False).__bool__()),
            "CHANGE_OCEAN": int(config["fesom"].get("change_ocean", False).__bool__()),
            "FESOM_TO_ICE": int(config["general"]["first_run_in_chunk"]),
            "MESH_DIR_fesom": config["fesom"]["mesh_dir"],
            # Max-mesh the dynamic submesh is carved from. Defaults to the
            # running mesh (mesh_dir) but can be overridden per experiment via
            # `fesom: { max_mesh: /path/to/larger_mesh/ }` in the runscript.
            "MAX_MESH": config["fesom"].get("max_mesh", config["fesom"]["mesh_dir"]),
            # Node grid-description of the max-mesh, expected inside MAX_MESH.
            "MESH_GRIDDES_fesom": config["fesom"].get("griddes_nodes", "core2_griddes_nodes.nc"),
            "MESH_ROTATED_fesom": config["fesom"]["mesh_rotated"],
            # Number of FESOM MPI tasks; the submesh must be partitioned to match.
            "NPROC_fesom": config["fesom"]["nproc"],
            "DATA_DIR_fesom": config["fesom"]["experiment_outdata_dir"],
            "RESTART_DIR_fesom": config["fesom"]["experiment_restart_in_dir"],
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            "number_of_years_for_forcing": config["model1"]["chunk_size"],
            "CHUNK_SIZE_pism_standalone": config["model2"]["chunk_size"],
            "CHUNK_START_DATE_fesom": config["general"]["chunk_start_date"],
            "CHUNK_END_DATE_fesom": config["general"]["chunk_end_date"],
            "FUNCTION_PATH": config["fesom"]["workflow"]["subjobs"]["couple_in"]["script_dir"],
            "PYFESOM_PATH": "/pf/a/a270124/pyfesom2/",
            "EXP_ID": config["general"]["command_line_config"]["expid"],
            "iter_coup_regrid_method_ice2oce": "INTERPOLATE",
            #"BASIN_FILE": config["fesom"].get("basin_file"),
            "MACHINE": config["computer"]["name"],
            "ICEBERG_DIR": config["fesom"].get("iceberg_dir", ""),

            #"FESOM_GRID_input": config["fesom"]["grid_input"],
            #"solidearth_ice_thickness_file":(
            #    config["general"]["experiment_couple_dir"] +
            #    "/ice_thickness.nc"
            #    ),
            #"ADD_UNCHANGED_ICE": config["vilma"].get("add_unchanged_ice", False),
            #"EISLASTFILE_vilma":  (
            #    config["vilma"]["experiment_input_dir"] +
            #    "/" +
            #    config["vilma"]["eislastfile"]
            #    ),
            #"RUN_NUMBER_vilma": config["general"]["run_number"],
            #"RUN_DATE_STAMP": config["general"]["run_datestamp"],
            #"LAST_RUN_DATE_STAMP": config["general"]["last_run_datestamp"],
            #"INITIAL_YEAR_vilma": config["general"]["initial_date"].syear,
            #"NYEAR_vilma_standalone": config["general"]["nyear"],
            #"FINAL_YEAR_vilma": config["general"]["final_date"].syear,
            #"EISLASTCONF_vilma":(
            #    config["vilma"]["experiment_config_dir"] +
            #    "/inp/" +
            #    config["vilma"]["eislastconf"]
            #    )

            }
    
    #if environment_dict["ADD_UNCHANGED_ICE"] == False:
    #    environment_dict["ADD_UNCHANGED_ICE"] = 0
    #elif environment_dict["ADD_UNCHANGED_ICE"] == True:
    #    environment_dict["ADD_UNCHANGED_ICE"] = 1


    print(environment_dict)
    return environment_dict




