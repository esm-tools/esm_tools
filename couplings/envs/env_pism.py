def prepare_environment(config):
    default_input_grid = config["general"]["experiment_couple_dir"] +"/ice.griddes"
    environment_dict = {
            # general
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            "EXP_ID": config["general"]["command_line_config"]["expid"],
            "WORK_DIR": config["general"]["thisrun_work_dir"],
            "FUNCTION_PATH": config[config["general"]["setup_name"]]["workflow"]["subjobs"]["couple_in"]["script_dir"],
            "MACHINE": config["computer"]["name"],
            
            # PISM
            "ATMOSPHERE_TO_PISM": int(config["general"]["first_run_in_chunk"]),
            "PISM_TO_ATMOSPHERE": int(config["general"]["last_run_in_chunk"]),
            "PISM_TO_OCEAN": int(config[config["general"]["setup_name"]].get("iceberg_coupling", False).__bool__()),
            "OCEAN_TO_PISM": int(config["general"]["first_run_in_chunk"]),
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
            "OUTPUT_DIR_pism": config[config["general"]["setup_name"]]["experiment_outdata_dir"],
            "SPINUP_FILE_pism": config[config["general"]["setup_name"]]["spinup_file"],
            "CHUNK_SIZE_pism_standalone": config["model2"]["chunk_size"],
            "DOMAIN_pism": config[config["general"]["setup_name"]]["domain"],
            "RES_pism": config[config["general"]["setup_name"]]["resolution"],
            "EXE_pism": config[config["general"]["setup_name"]]["executable"],
            "INPUT_FILE_pism": config[config["general"]["setup_name"]].get("cli_input_file_pism"),
            
            # user defined input
            "iter_coup_interact_method_oce2ice": config[config["general"]["setup_name"]].get("iter_coup_interact_method_oce2ice", "OCEANTEMPSALT"),
            "iterative_coupling_atmosphere_pism_regrid_method": config[config["general"]["setup_name"]].get("regrid_method", "DOWNSCALE"), 
            "iterative_coupling_atmosphere_pism_ablation_method": config[config["general"]["setup_name"]].get("ablation_method", "PDD"),
            "DEBM_BETA": config[config["general"]["setup_name"]].get("debm_beta", 999),
            "MULTI_YEAR_MEAN_SMB": config[config["general"]["setup_name"]].get("multi_year_mean_smb", 1),
            "PISM_OCEAN_PICO_BASINS_FILE": config[config["general"]["setup_name"]].get("basin_file", ""),
            "DOWNSCALING_LAPSE_RATE": config[config["general"]["setup_name"]].get("lapse_rate", -0.005),
            "DEBM_EXE": config[config["general"]["setup_name"]].get("debm_path", ""),
            
            "SEA_LEVEL_FORCING": config[config["general"]["setup_name"]].get("sea_level_forcing", 0),
            
            "CHUNK_NUMBER_pism": config["general"]["chunk_number"],
            "NYEAR": config["general"]["nyear"],
            
            "iter_coup_interact_method_ice2oce": "BASALSHELF_WATER_ICEBERG_MODEL",
            "orog_reference_pism": config[config["general"]["setup_name"]].get("orog_reference_pism", 1),
            "first_year_in_chunk_input": config[config["general"]["setup_name"]]["experiment_input_dir"] + "/" + config["general"]["expid"] + "_pismr_input_"  + config["general"]["chunk_start_date"].syear + "0101-" + str(int( config["general"]["chunk_start_date"].syear ) + int( config["general"]["nyear"] - 1 )) + "1231.nc", 
            "last_year_in_chunk_restart": config[config["general"]["setup_name"]]["restart_out_targets"]["restart"],
            "USE_YMONMEAN": config[config["general"]["setup_name"]].get("use_ymonmean", 0),
            "CRITICAL_THK_FOR_MASK_pism": config["pism"].get("thk_threshold", 5.0), 
            "MIN_MON_SELECT": int(config["pism"].get("select_min_glacial_depth", 1)),
           
            # bias correction
            "ANOMALY_AIR_TEMPERATURE": int(config[config["general"]["setup_name"]].get("ANOMALY_AIR_TEMPERATURE", 0)),
            "ANOMALY_PRECIPITATION": int(config[config["general"]["setup_name"]].get("ANOMALY_PRECIPITATION", 0)),
            "ANOMALY_OCEAN_TEMPERATURE": int(config[config["general"]["setup_name"]].get("ANOMALY_OCEAN_TEMPERATURE", 0)),
            "ANOMALY_OCEAN_SALINITY": int(config[config["general"]["setup_name"]].get("ANOMALY_OCEAN_SALINITY", 0)),
            "REFERENCE_ATMOS_FNAME": config[config["general"]["setup_name"]].get("REFERENCE_ATMOS_FNAME", "/work/ab0246/a270096/share/pism/ANT.16km/ATMForcing.piControl.16km.nc"),
            "BASE_STATE_ATMOS_FNAME": config[config["general"]["setup_name"]].get("BASE_STATE_ATMOS_FNAME", "/work/ab0246/a270096/share/pism/ANT.16km/atmo_given_file_base_state.pism_sh.nc"),
            "REFERENCE_OCEAN_FNAME": config[config["general"]["setup_name"]].get("REFERENCE_OCEAN_FNAME", "/work/ab0246/a270096/share/pism/ANT.16km/OCEForcing.piControl.16km.nc"),
            "BASE_STATE_OCEAN_FNAME": config[config["general"]["setup_name"]].get("BASE_STATE_OCEAN_FNAME", "/work/ab0246/a270096/share/pism/ANT.16km/ocean_forcing4pism_base_state.pism_sh.nc"),
            }
    print (environment_dict)
    return environment_dict
