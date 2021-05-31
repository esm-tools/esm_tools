def prepare_environment(config):
    environment_dict = {
            "TEST_IN_ENV": "testvar_in_env_exported_couple_in_pism",
            "SOLID_EARTH_TO_PISM": 1,
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            #"SOLIDEARTH_grid": from names file,
            #"bedrock_change_name": from names file
            #"RUN_NUMBER_solidearth": from names file,
            "ice_bedrock_change_file": (
                config["general"]["experiment_couple_dir"] +
                "/bedrock_change.nc"
                ),
            "RESTART_DIR_pism": config["pism"]["experiment_restart_in_dir"],
            "latest_restart_file_pism": config["pism"]["restart_in_in_work"]["restart"],
            "RUN_DATE_STAMP": config["general"]["run_datestamp"],
            }

    print(environment_dict)
    return environment_dict






