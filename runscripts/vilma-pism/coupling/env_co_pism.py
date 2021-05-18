def prepare_environment(config):
    default_input_grid = config["general"]["experiment_couple_dir"] +"/ice.griddes"
    environment_dict = {
            "TEST_IN_ENV": "testvar_in_env_exported_couple_out_pism",
            "PISM_TO_SOLID_EARTH": 1,
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
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
            }
    print (environment_dict)
    return environment_dict






