def prepare_environment(config):
    environment_dict = {
            "ICE_TO_FESOM": config["fesom"].get("use_icebergs", False),
            "FESOM_TO_ICE": int(config["general"]["first_run_in_chunk"]),
            "MESH_DIR_fesom": config["fesom"]["mesh_dir"],
            "MESH_ROTATED_fesom": config["fesom"]["mesh_rotated"],
            "DATA_DIR_fesom": config["fesom"]["experiment_outdata_dir"],
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            "number_of_years_for_forcing": config["model1"]["chunk_size"],
            "CHUNK_SIZE_pism_standalone": config["model2"]["chunk_size"],
            "CHUNK_START_DATE_fesom": config["general"]["chunk_start_date"],
            "CHUNK_END_DATE_fesom": config["general"]["chunk_end_date"],
            "FUNCTION_PATH": config["fesom"]["workflow"]["subjobs"]["couple_in"]["script_dir"],
            "PYFESOM_PATH": "/pf/a/a270124/pyfesom2/",
            "EXP_ID": config["general"]["command_line_config"]["expid"],
            "iter_coup_regrid_method_ice2oce": "INTERPOLATE",
            "fesom_use_icebergs": config["fesom"].get("use_icebergs", ""), 
            "MACHINE": config["computer"]["name"],
            "ICEBERG_DIR": config["fesom"].get("iceberg_dir", ""),
    print(environment_dict)
    return environment_dict
