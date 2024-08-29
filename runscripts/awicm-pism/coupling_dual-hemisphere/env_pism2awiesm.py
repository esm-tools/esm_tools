def prepare_environment(config):
    default_input_grid = config["general"]["experiment_couple_dir"] +"/ice.griddes"
    environment_dict = {
            "ATMOSPHERE_TO_PISM": int(config["general"]["first_run_in_chunk"]),
            "PISM_TO_ATMOSPHERE": int(config["general"]["last_run_in_chunk"]),
            "CHUNK_START_DATE_pism": config["general"]["chunk_start_date"],
            "CHUNK_END_DATE_pism": config["general"]["chunk_end_date"],
            "CHUNK_START_YEAR_pism": config["general"]["chunk_start_date"].syear,
            "CHUNK_END_YEAR_pism": config["general"]["chunk_end_date"].syear,
            "NYEAR": config["general"]["nyear"],
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            "DOWNSCALE_TEMP": 1, 
            "DOWNSCALING_LAPSE_RATE": config[config["general"]["setup_name"]]["lapse_rate"],
            "DOWNSCALE_PRECIP": 1, 
            "VERSION_pism": config[config["general"]["setup_name"]]["version"].replace("github", "").replace("index", "").replace("snowflake", "")[:3],
            "POOL_DIR_pism": config[config["general"]["setup_name"]]["pool_dir"],
            "DOMAIN_pism": config[config["general"]["setup_name"]]["domain"],
            "EXE_pism": config[config["general"]["setup_name"]]["executable"],
            "RES_pism": config[config["general"]["setup_name"]]["resolution"],
            "RUN_NUMBER_pism": config["general"]["run_number"],
            "EX_INT": config[config["general"]["setup_name"]]["ex_interval"],

            "YR0_pism": config["general"]["start_date"].syear,
            "M0_pism": config["general"]["start_date"].smonth,
            "D0_pism": config["general"]["start_date"].sday,

            "END_YEAR_pism": config["general"]["end_date"].syear,
            "END_MONTH_pism": config["general"]["end_date"].smonth,
            "END_DAY_pism": config["general"]["end_date"].sday,
            "MIN_MON_SELECT": int(config[config["general"]["setup_name"]].get("select_min_glacial_depth", True)),

            "CRITICAL_THK_FOR_MASK_pism": config[config["general"]["setup_name"]].get("thk_threshold", 5.0), 
            "CURRENT_YEAR_pism": config["general"]["current_date"].syear,
            "EXP_ID": config["general"]["command_line_config"]["expid"],
            "OUTPUT_DIR_pism": config[config["general"]["setup_name"]]["experiment_outdata_dir"],
            "SPINUP_FILE_pism": config[config["general"]["setup_name"]]["spinup_file"],
            "CHUNK_SIZE_pism_standalone": config["model2"]["chunk_size"],
            "INPUT_FILE_pism": config[config["general"]["setup_name"]].get("cli_input_file_pism"),
            
            "PISM_TO_OCEAN": 0,
            "OCEAN_TO_PISM": int(config["general"]["first_run_in_chunk"]),
            "fesom_use_iceberg": int(config[config["general"]["setup_name"]].get("iceberg_coupling", False)), 
            "CURRENT_YEAR_pism": config["general"]["current_date"].syear,
            "END_YEAR_pism": config["general"]["end_date"].syear,
            "EXP_ID": config["general"]["command_line_config"]["expid"],
            #"ICEBERG_DIR": config["general"]["iceberg_dir"], 
            "OUTPUT_DIR_pism": config[config["general"]["setup_name"]]["experiment_outdata_dir"],
            "SPINUP_FILE_pism": config[config["general"]["setup_name"]]["spinup_file"],
            #"MESH_DIR_fesom": config["general"]["mesh_dir"],
            "FUNCTION_PATH": config[config["general"]["setup_name"]]["workflow"]["subjobs"]["couple_in"]["script_dir"],
            "CHUNK_SIZE_pism_standalone": config["model2"]["chunk_size"],
            #"iter_coup_interact_method_ice2oce": "BASALSHELF_WATER_ICEBERG_MODEL",
            "MACHINE": config["computer"]["name"],
            "PISM_HEMISPHERE": config["general"]["setup_name"]
            }
    print (environment_dict)
    return environment_dict
