def prepare_environment(config):
    environment_dict = {
            "ICE_TO_FESOM": int(config["fesom"].get("use_icebergs", False)),
            "FESOM_TO_ICE": int(config["general"]["first_run_in_chunk"]),
            "MESH_DIR_fesom": config["fesom"]["mesh_dir"],
            "MESH_ROTATED_fesom": config["fesom"]["mesh_rotated"],
            "DATA_DIR_fesom": config["fesom"]["experiment_outdata_dir"],
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            "number_of_years_for_forcing": config["model1"]["chunk_size"],
            "CHUNK_SIZE_pism_standalone": config["model2"]["chunk_size"],
            "CHUNK_START_DATE_fesom": config["general"]["chunk_start_date"],
            "CHUNK_END_DATE_fesom": config["general"]["chunk_end_date"],
            "FUNCTION_PATH": config["fesom"]["workflow"]["subjobs"]["couple_in"]["script_dir"],
            "PYFESOM_PATH": "/pf/a/a270124/pyfesom2/",
            "EXP_ID": config["general"]["command_line_config"]["expid"],
            "iter_coup_regrid_method_ice2oce": "INTERPOLATE",
            "fesom_use_icebergs": config["fesom"].get("use_icebergs", ""), 
            "BASIN_FILE": config["fesom"].get("basin_file"),
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




