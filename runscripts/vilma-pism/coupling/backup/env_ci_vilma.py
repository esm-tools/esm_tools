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
            "ADD_UNCHANGED_ICE": config["vilma"].get("add_unchanged_ice", False),
            "EISLASTFILE_vilma":  (
                config["vilma"]["experiment_input_dir"] +
                "/" +
                config["vilma"]["eislastfile"]
                ),
            "RUN_NUMBER_vilma": config["general"]["run_number"],
            "RUN_DATE_STAMP": config["general"]["run_datestamp"],
            "LAST_RUN_DATE_STAMP": config["general"]["last_run_datestamp"],
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




