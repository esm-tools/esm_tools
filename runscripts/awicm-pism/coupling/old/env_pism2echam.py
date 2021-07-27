def prepare_environment(config):
    default_input_grid = config["general"]["experiment_couple_dir"] +"/ice.griddes"
    environment_dict = {
            "PISM_TO_ATMOSPHERE": 1,
            "ATMOSPHERE_TO_PISM": 1,
            "iterative_coupling_atmosphere_pism_regrid_method": "DYN_DOWNSCALE", 
            "iterative_coupling_atmosphere_pism_ablation_method": "DEBM",
            "CHUNK_START_DATE_pism": config["general"]["chunk_start_date"],
            "CHUNK_END_DATE_pism": config["general"]["chunk_end_date"],
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            "DOWNSCALE_TEMP": 1, 
            "DOWNSCALING_LAPSE_RATE": -0.007,
            "DOWNSCALE_PRECIP": 1, 
            "VERSION_pism": config["pism"]["version"],
            "POOL_DIR_pism": config["pism"]["pool_dir"],
            "DOMAIN_pism": config["pism"]["domain"],
            "EXE_pism": config["pism"]["executable"],
            "RES_pism": config["pism"]["resolution"],
            
            "YR0_pism": config["general"]["initial_date"].syear,
            "CURRENT_YEAR_pism": config["general"]["current_date"].syear,
            "END_YEAR_pism": config["general"]["end_date"].syear,
            "EXP_ID": config["general"]["command_line_config"]["expid"],
            #"fesom_use_icebergs": config["fesom"]["use_icebergs"], 
            "OUTPUT_DIR_pism": config["pism"]["experiment_outdata_dir"],
            "SPINUP_FILE_pism": config["pism"]["spinup_file"],
            "FUNCTION_PATH": "/home/ollie/lackerma/esm_tools/runscripts/awicm-pism/coupling",
            "CHUNK_SIZE_pism_standalone": config["model2"]["chunk_size"],

            #"RESTART_DIR_pism": config["pism"]["experiment_restart_in_dir"],
            #"ice_bedrock_change_file": (
            #    config["general"]["experiment_couple_dir"] +
            #    "/bedrock_change.nc"
            #    ),
            #"RUN_NUMBER_pism" : config["general"]["run_number"],
            #"pism_solidearth_initialize_method": config["pism"]["solidearth_initialize_method"],
            #"pism_solidearth_initialize_dummyrun_file": config["pism"].get("solidearth_initialize_dummyrun_file", ""),
            #"INPUT_GRID_pism": config["pism"].get("input_grid", default_input_grid),
            "INPUT_FILE_pism": config["pism"].get("cli_input_file_pism"),
            #"NYEAR_pism_standalone": config["general"]["nyear"],
            #"latest_ex_file_pism": config["pism"]["outdata_targets"]["ex_file"],
            #"latest_restart_file_pism": config["pism"]["restart_in_in_work"]["restart"],
            #"restart_file_pism": config["pism"]["restart_in_in_work"]["restart"],
            #"RUN_DATE_STAMP": config["general"]["run_datestamp"],
            #"LAST_RUN_DATE_STAMP": config["general"]["last_run_datestamp"],
            }
    print (environment_dict)
    return environment_dict






