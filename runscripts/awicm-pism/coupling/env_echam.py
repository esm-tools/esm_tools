def prepare_environment(config):
    environment_dict = {
            "ICE_TO_ECHAM": 1,
            "ECHAM_TO_ICE": 1,
            "ECHAM_TO_ISM_multiyear_mean": 1,
            "ISM_TO_ECHAM_update_orography": 1, 
            "ISM_TO_ECHAM_update_glacial_mask": 1, 
            "ISM_TO_ECHAM_update_land_runoff": 1,
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            "RES_echam": config["echam"]["resolution"], 
            "EXP_ID": config["general"]["command_line_config"]["expid"],
            "RESTART_DIR_echam": config["echam"]["experiment_restart_out_dir"],
            "DATA_DIR_echam": config["echam"]["experiment_outdata_dir"],
            "INIT_DIR_echam": config["echam"]["experiment_input_dir"],
            "number_of_years_for_forcing": config["model1"]["chunk_size"],
            "CHUNK_START_DATE_echam": config["general"]["chunk_start_date"],
            "CHUNK_END_DATE_echam": config["general"]["chunk_end_date"],
            "END_YEAR_echam": config["general"]["chunk_end_date"].syear,
            "END_MONTH_echam": config["general"]["chunk_end_date"].smonth,
            "END_DAY_echam": config["general"]["chunk_end_date"].sday,
            "FUNCTION_PATH": "/home/ollie/lackerma/esm_tools/runscripts/awicm-pism/coupling",
            "FORCING_DIR_jsbach": config["jsbach"]["thisrun_forcing_dir"],
            "RESTART_DIR_jsbach": config["jsbach"]["experiment_restart_out_dir"],
            }
    
    #if environment_dict["ADD_UNCHANGED_ICE"] == False:
    #    environment_dict["ADD_UNCHANGED_ICE"] = 0
    #elif environment_dict["ADD_UNCHANGED_ICE"] == True:
    #    environment_dict["ADD_UNCHANGED_ICE"] = 1


    print(environment_dict)
    return environment_dict




