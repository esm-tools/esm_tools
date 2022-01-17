def prepare_environment(config):
    default_input_grid = config["general"]["experiment_couple_dir"] +"/ice.griddes"
    environment_dict = {
            "ATMOSPHERE_TO_PISM": int(config["general"]["first_run_in_chunk"]),
            "PISM_TO_ATMOSPHERE": int(config["general"]["last_run_in_chunk"]),
            "iterative_coupling_atmosphere_pism_regrid_method": "DOWNSCALE", 
            "iterative_coupling_atmosphere_pism_ablation_method": "DEBM",
            "MY_OBLIQUITY": "23.446", 
            "CHUNK_START_DATE_pism": config["general"]["chunk_start_date"],
            "CHUNK_END_DATE_pism": config["general"]["chunk_end_date"],
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            "DOWNSCALE_TEMP": 1, 
            "DOWNSCALING_LAPSE_RATE": -0.007,
            "DOWNSCALE_PRECIP": 1, 
            "VERSION_pism": config["pism"]["version"].replace("github", "").replace("index", "").replace("snowflake", "")[:3],
            "POOL_DIR_pism": config["pism"]["pool_dir"],
            "DOMAIN_pism": config["pism"]["domain"],
            "EXE_pism": config["pism"]["executable"],
            "RES_pism": config["pism"]["resolution"],
            "RUN_NUMBER_pism": config["general"]["run_number"],

            "YR0_pism": config["general"]["start_date"].syear,
            "M0_pism": config["general"]["start_date"].smonth,
            "D0_pism": config["general"]["start_date"].sday,

            "END_YEAR_pism": config["general"]["end_date"].syear,
            "END_MONTH_pism": config["general"]["end_date"].smonth,
            "END_DAY_pism": config["general"]["end_date"].sday,
            
            "CURRENT_YEAR_pism": config["general"]["current_date"].syear,
            "EXP_ID": config["general"]["command_line_config"]["expid"],
            "OUTPUT_DIR_pism": config["pism"]["experiment_outdata_dir"],
            "SPINUP_FILE_pism": config["pism"]["spinup_file"],
            "CHUNK_SIZE_pism_standalone": config["model2"]["chunk_size"],
            "INPUT_FILE_pism": config["pism"].get("cli_input_file_pism"),
            
            "PISM_TO_OCEAN": 0,
            "OCEAN_TO_PISM": int(config["general"]["first_run_in_chunk"]),
            "CURRENT_YEAR_pism": config["general"]["current_date"].syear,
            "END_YEAR_pism": config["general"]["end_date"].syear,
            "EXP_ID": config["general"]["command_line_config"]["expid"],
            #"ICEBERG_DIR": config["general"]["iceberg_dir"], 
            "OUTPUT_DIR_pism": config["pism"]["experiment_outdata_dir"],
            "SPINUP_FILE_pism": config["pism"]["spinup_file"],
            #"MESH_DIR_fesom": config["general"]["mesh_dir"],
            "FUNCTION_PATH": "/pf/a/a270124/esm_tools/runscripts/awicm-pism/coupling",
            "CHUNK_SIZE_pism_standalone": config["model2"]["chunk_size"],
            #"iter_coup_interact_method_ice2oce": "BASALSHELF_WATER_ICEBERG_MODEL",
            }
    print (environment_dict)
    return environment_dict
