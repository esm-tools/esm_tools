def prepare_environment(config):
    default_input_grid = config["general"]["experiment_couple_dir"] +"/ice.griddes"
    environment_dict = {
            "PISM_TO_OCEAN": 0,
            "OCEAN_TO_PISM": int(config["general"]["first_run_in_chunk"]),
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            "VERSION_pism": config[config["general"]["setup_name"]]["version"].replace("github", "").replace("index", "").replace("snowflake", "")[:3],
            "POOL_DIR_pism": config[config["general"]["setup_name"]]["pool_dir"],
            
            "YR0_pism": config["general"]["start_date"].syear,
            "M0_pism": config["general"]["start_date"].smonth,
            "D0_pism": config["general"]["start_date"].sday,

            "END_YEAR_pism": config["general"]["end_date"].syear,
            "END_MONTH_pism": config["general"]["end_date"].smonth,
            "END_DAY_pism": config["general"]["end_date"].sday,
            
            "CURRENT_YEAR_pism": config["general"]["current_date"].syear,
            "EX_INT": config[config["general"]["setup_name"]]["ex_interval"], 
            "RUN_NUMBER_pism": config["general"]["run_number"],
            "CHUNK_START_DATE_pism": config["general"]["chunk_start_date"],
            "CHUNK_END_DATE_pism": config["general"]["chunk_end_date"],
            "CHUNK_START_YEAR_pism": config["general"]["chunk_start_date"].syear,
            "CHUNK_END_YEAR_pism": config["general"]["chunk_end_date"].syear,
            "EXP_ID": config["general"]["command_line_config"]["expid"],
            #"ICEBERG_DIR": config["general"]["iceberg_dir"], 
            "OUTPUT_DIR_pism": config[config["general"]["setup_name"]]["experiment_outdata_dir"],
            "SPINUP_FILE_pism": config[config["general"]["setup_name"]]["spinup_file"],
            #"MESH_DIR_fesom": config["general"]["mesh_dir"],
            "FUNCTION_PATH": config[config["general"]["setup_name"]]["workflow"]["subjobs"]["couple_in"]["script_dir"],
            "CHUNK_SIZE_pism_standalone": config["model2"]["chunk_size"],
            #"iter_coup_interact_method_ice2oce": "BASALSHELF_WATER_ICEBERG_MODEL",
            "MACHINE": config["computer"]["name"],
            "DOMAIN_pism": config[config["general"]["setup_name"]]["domain"],
            "RES_pism": config[config["general"]["setup_name"]]["resolution"],
            "EXE_pism": config[config["general"]["setup_name"]]["executable"],
            "iterative_coupling_atmosphere_pism_ablation_method": config[config["general"]["setup_name"]].get("ablation_method", "PDD"),
            "DEBM_EXE": config[config["general"]["setup_name"]].get("debm_path", ""),
            "MY_OBLIQUITY": config[config["general"]["setup_name"]].get("debm_obl", "23.441"),
            "DEBM_BETA": config[config["general"]["setup_name"]].get("debm_beta", 999),
            "iterative_coupling_atmosphere_pism_regrid_method": config[config["general"]["setup_name"]].get("regrid_method", "DOWNSCALE"), 
            "REDUCE_TEMP": int(config[config["general"]["setup_name"]].get("reduce_temp", 0)), 
            "REDUCE_TEMP_BY": config[config["general"]["setup_name"]].get("reduce_temp_by", 1), 
            "USE_YMONMEAN": config[config["general"]["setup_name"]].get("use_ymonmean", 0),
            "MULTI_YEAR_MEAN_SMB": config[config["general"]["setup_name"]].get("multi_year_mean_smb", 1),
            "CHANGE_OCEAN":config[config["general"]["setup_name"]].get("change_ocean", 1),  
            # UKK new environment variable: change_oceacn -------------- ^^^^^^^^^^^^
            #"PISM_OCEAN_PICO_BASINS_FILE": "/home/ollie/lackerma/pool_pism/basins/antarctica.16km.nc",

            "INPUT_FILE_pism": config[config["general"]["setup_name"]].get("cli_input_file_pism"),
            "TEMP2_BIAS_FILE": config[config["general"]["setup_name"]].get("temp2_bias_file"),
            "DOWNSCALING_LAPSE_RATE": config[config["general"]["setup_name"]].get("lapse_rate", -0.005),
            "DOWNSCALE_PRECIP": config[config["general"]["setup_name"]].get("downscale_precip", 0),

            # OCP-tool variables (for ice2oifs coupling)
            "OCP_POOL_DIR": config["general"].get("pool_dir", ""),
            "OCP_OIFS_RES": config.get("oifs", {}).get("resolution", ""),
            "OCP_OIFS_RES_NUMBER": config.get("oifs", {}).get("res_number", ""),
            "OCP_OIFS_TRUNCATION": config.get("oifs", {}).get("truncation", ""),
            "OCP_OIFS_LEVELS": config.get("oifs", {}).get("levels", ""),
            "OCP_OIFS_PREPIFS_EXPID": config.get("oifs", {}).get("prepifs_expid", ""),
            "OCP_OIFS_INPUT_EXPID": config.get("oifs", {}).get("input_expid", ""),
            "OCP_OIFS_VERSION": config.get("oifs", {}).get("version", ""),
            "OCP_FESOM_RES": config.get("fesom", {}).get("resolution", ""),
            }
    print (environment_dict)
    return environment_dict






