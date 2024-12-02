def prepare_environment(config):
    default_input_grid = config["general"]["experiment_couple_dir"] +"/ice.griddes"
    #print (config)
    environment_dict = {
            "PISM_TO_SOLID_EARTH": 1,
            "SOLID_EARTH_TO_PISM": 1,
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            "RESTART_DIR_pism": config["pism"]["experiment_restart_in_dir"],
            "ice_bedrock_change_file": (
                config["general"]["experiment_couple_dir"] +
                "/bedrock_change.nc"
                ),
            "POOL_DIR_pism": config["pism"]["pool_dir"],
            "DOMAIN_pism": config["pism"]["domain"],
            "EXE_pism": config["pism"]["executable"],
            "RES_pism": config["pism"]["resolution"],
            "RUN_NUMBER_pism" : config["general"]["run_number"],
            "pism_solidearth_initialize_method": config["pism"]["solidearth_initialize_method"],
            "pism_solidearth_initialize_dummyrun_file": config["pism"].get("solidearth_initialize_dummyrun_file", ""),
            "INPUT_GRID_pism": config["pism"].get("input_grid", default_input_grid),
            "INPUT_FILE_pism": config["pism"].get("cli_input_file_pism"),
            "YR0_INI_pism": config["general"]["initial_date"].syear,
            "CURRENT_YEAR_pism": config["general"]["current_date"].syear,
            "END_YEAR_pism": config["general"]["end_date"].syear,
            "NYEAR_pism_standalone": config["general"]["nyear"],
            "latest_ex_file_pism": config["pism"]["outdata_targets"]["ex_file"],
            "RUN_DATE_STAMP": config["general"]["run_datestamp"],
            "LAST_RUN_DATE_STAMP": config["general"]["last_run_datestamp"],
            }

    if config["general"]["run_number"] > 1:
        environment_dict["latest_restart_file_pism"] = config["pism"]["restart_in_targets"]["restart"]
    print (environment_dict)
    return environment_dict






