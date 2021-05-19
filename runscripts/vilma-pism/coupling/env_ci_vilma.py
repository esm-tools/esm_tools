def prepare_environment(config):



    environment_dict = {
            "TEST_IN_ENV": "testvar_in_env_exported_couple_in_vilma",
            "ICE_TO_VILMA": 1,
            "VILMA_GRID_input": config["vilma"]["grid_input"],
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            "solidearth_ice_thickness_file":(
                config["general"]["experiment_couple_dir"] +
                "/ice_thickness.nc"
                ),
            #"ice_thickness_name": from names file
            #"ice_mask_name": from names file
            #"ice_topography_name": from names file
            #"RUN_NUMBER_ice": from names file
            #"CURRENT_YEAR_ice" from names file
            #"END_YEAR_ice" from names file
            #"VALUE_LAND_ice": from names file
            #"VALUE_GROUNDED_ice": from names file
            #"VALUE_FLOATING_ice": from names file
            #"VALUE_OCEAN_ice": from names file
            "ADD_UNCHANGED_ICE": config["vilma"].get("add_unchanged_ice", False),
            "EISLASTFILE_vilma":  config["vilma"]["eislastfile"],
            "RUN_NUMBER_vilma": config["general"]["run_number"],
            "DATA_DIR_vilma": config["vilma"]["experiment_restart_out_dir"],
            "INITIAL_YEAR_vilma": config["general"]["initial_date"].syear,
            "NYEAR_vilma_standalone": config["general"]["nyear"],
            "FINAL_YEAR_vilma": config["general"]["final_date"].syear,
            "EISLASTCONF_vilma":(
                config["vilma"]["experiment_config_dir"] +
                "/inp/" +
                config["vilma"]["eislastconf"]
                )

            }
    
    if environment_dict["ADD_UNCHANGED_ICE"] == False:
        environment_dict["ADD_UNCHANGED_ICE"] = 0
    elif environment_dict["ADD_UNCHANGED_ICE"] == True:
        environment_dict["ADD_UNCHANGED_ICE"] = 1


    print(environment_dict)
    return environment_dict




