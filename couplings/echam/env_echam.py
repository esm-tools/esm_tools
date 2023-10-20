def prepare_environment(config):
    environment_dict = {
            "ICE_TO_ECHAM": int(config["general"]["first_run_in_chunk"]),
            "ECHAM_TO_ICE": int(config["general"]["last_run_in_chunk"]),
            "ECHAM_TO_ISM_multiyear_mean": 1,
            "ISM_TO_ECHAM_update_orography": 1, 
            "ISM_TO_ECHAM_update_glacial_mask": int(config["echam"].get("update_glacial_mask", True)), 
            "ISM_TO_ECHAM_update_land_runoff": 1,
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            "RES_echam": config["echam"]["resolution"], 
            "EXP_ID": config["general"]["command_line_config"]["expid"],
            "RESTART_DIR_echam": config["echam"]["experiment_restart_out_dir"],
            "DATA_DIR_echam": config["echam"]["experiment_outdata_dir"],
            "INIT_DIR_echam": config["echam"]["experiment_input_dir"],
            "WORK_DIR": config["general"]["thisrun_work_dir"],
            "number_of_years_for_forcing": config["model1"]["chunk_size"],
            "CHUNK_START_DATE_echam": config["general"]["chunk_start_date"],
            "CHUNK_END_DATE_echam": config["general"]["chunk_end_date"],
            "END_YEAR_echam": config["general"]["chunk_end_date"].syear,
            "END_MONTH_echam": config["general"]["chunk_end_date"].smonth,
            "END_DAY_echam": config["general"]["chunk_end_date"].sday,
            "FUNCTION_PATH": config["echam"]["workflow"]["subjobs"]["couple_in"]["script_dir"],
            "FORCING_DIR_jsbach": config["jsbach"]["experiment_input_dir"],
            "RESTART_DIR_jsbach": config["jsbach"]["experiment_restart_out_dir"],
            "POOL_DIR_jsbach": config["computer"]["pool_dir"],
            "POOL_DIR_echam": config["computer"]["pool_dir"],
            "MACHINE": config["computer"]["name"],
            "MESH_PATH_FESOM": config["fesom"]["mesh_dir"],
            "HOSING_FILE_LANDICE_LOSS": config["fesom"].get("fwf_path", config["general"]["experiment_couple_dir"]),
            "HOSING_CORRECTION": int(config["echam"].get("hosing_correction", False)), # LA: Not needed anymore with Lu's ECHAM gfw fix
            "CELL_AREA_FESOM_FILE": config["fesom"].get("fesom_cell_area_file", "fesom.mesh.diag.nc"),
            "ECHAM_ALBEDO_ON_GLACIERS": config["echam"].get("albedo_on_glaciers", 0.7),
            "ECHAM_GLACIAL_THRESHOLD": config["echam"].get("glacial_threshold", 0.5),
            }
    
    #if environment_dict["ADD_UNCHANGED_ICE"] == False:
    #    environment_dict["ADD_UNCHANGED_ICE"] = 0
    #elif environment_dict["ADD_UNCHANGED_ICE"] == True:
    #    environment_dict["ADD_UNCHANGED_ICE"] = 1


    print(environment_dict)
    return environment_dict




