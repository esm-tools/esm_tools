def prepare_environment(config):
    environment_dict = {
            "TEST_IN_ENV": "testvar_in_env_exported_couple_out_vilma",
            "VILMA_TO_ICE": 1,
            "VILMA_GRID_input": config["vilma"]["grid_input"],
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            "RUN_NUMBER_vilma": config["general"]["run_number"],
            "DATA_DIR_vilma": config["vilma"]["experiment_outdata_dir"]+"/out/",
            "RUN_DATE_STAMP": config["general"]["run_datestamp"] ,           
            }
    print(environment_dict)
    return environment_dict






