"""Coupling environment for one ice sheet domain (couple_in, awiesm2pism).

Replaces the old ``env_pism.py``. The only structural change is that the
component no longer assumes it is *the* ice sheet: it exports which domain it
is (``ICE_DOMAIN``) and which domains exist in total (``ICE_DOMAINS``), and the
shell functions derive ``ICE_COUPLE_DIR`` from that.
"""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import ice_domain_env  # noqa: E402


def prepare_environment(config):
    setup_name = config["general"]["setup_name"]
    ice = config[setup_name]

    environment_dict = {
        # general
        "COUPLE_DIR": config["general"]["experiment_couple_dir"],
        "EXP_ID": config["general"]["command_line_config"]["expid"],
        "WORK_DIR": config["general"]["thisrun_work_dir"],
        "FUNCTION_PATH": ice["workflow"]["subjobs"]["couple_in"]["script_dir"],
        "MACHINE": config["computer"]["name"],

        # which ice domain am I, and who else is there
        "ICE_DOMAIN": ice_domain_env.this_ice_domain(config),

        # PISM
        "ATMOSPHERE_TO_PISM": int(config["general"]["first_run_in_chunk"]),
        "PISM_TO_ATMOSPHERE": int(config["general"]["last_run_in_chunk"]),
        "PISM_TO_OCEAN": int(ice.get("iceberg_coupling", False).__bool__()),
        "OCEAN_TO_PISM": int(config["general"]["first_run_in_chunk"]),
        "VERSION_pism": ice["version"]
        .replace("github", "")
        .replace("index", "")
        .replace("snowflake", "")[:3],
        "POOL_DIR_pism": ice["pool_dir"],
        "YR0_pism": config["general"]["start_date"].syear,
        "M0_pism": config["general"]["start_date"].smonth,
        "D0_pism": config["general"]["start_date"].sday,
        "END_YEAR_pism": config["general"]["end_date"].syear,
        "END_MONTH_pism": config["general"]["end_date"].smonth,
        "END_DAY_pism": config["general"]["end_date"].sday,
        "CURRENT_YEAR_pism": config["general"]["current_date"].syear,
        "EX_INT": ice["ex_interval"],
        "RUN_NUMBER_pism": config["general"]["run_number"],
        "CHUNK_START_DATE_pism": config["general"]["chunk_start_date"],
        "CHUNK_END_DATE_pism": config["general"]["chunk_end_date"],
        "CHUNK_START_YEAR_pism": config["general"]["chunk_start_date"].syear,
        "CHUNK_END_YEAR_pism": config["general"]["chunk_end_date"].syear,
        "OUTPUT_DIR_pism": ice["experiment_outdata_dir"],
        "RESTART_DIR_pism": ice["experiment_restart_out_dir"],
        "SPINUP_FILE_pism": ice["spinup_file"],
        # This model's own chunk size -- the old scripts read model2's, which
        # is wrong as soon as there is more than one ice sheet.
        "CHUNK_SIZE_pism_standalone": ice_domain_env.this_chunk_size(config),
        "DOMAIN_pism": ice["domain"],
        "RES_pism": ice["resolution"],
        "EXE_pism": ice["executable"],
        "INPUT_FILE_pism": ice.get("cli_input_file_pism"),

        # user defined input
        "iter_coup_interact_method_oce2ice": ice.get(
            "iter_coup_interact_method_oce2ice",
            ice.get("ocean_ablation_method", "OCEANTEMPSALT"),
        ),
        "iterative_coupling_atmosphere_pism_regrid_method": ice.get(
            "regrid_method", "DOWNSCALE"
        ),
        "iterative_coupling_atmosphere_pism_ablation_method": ice.get(
            "ablation_method", "PDD"
        ),
        "DEBM_BETA": ice.get("debm_beta", 999),
        "MY_OBLIQUITY": ice.get("debm_obl", "23.441"),
        "MULTI_YEAR_MEAN_SMB": ice.get("multi_year_mean_smb", 1),
        "PISM_OCEAN_PICO_BASINS_FILE": ice.get("basin_file", ""),
        "DOWNSCALING_LAPSE_RATE": ice.get("lapse_rate", -0.005),
        # Downscaling switches are read by atmosphere2pism, which runs in
        # couple_in -- so they have to be exported here and not only in
        # env_ice2awiesm.py.
        "DOWNSCALE_TEMP": int(ice.get("downscale_temp", 1)),
        "DOWNSCALE_PRECIP": int(ice.get("downscale_precip", 1)),
        "DEBM_EXE": ice.get("debm_path", ""),
        "REDUCE_TEMP": int(ice.get("reduce_temp", 0)),
        "REDUCE_TEMP_BY": ice.get("reduce_temp_by", 1),
        "SEA_LEVEL_FORCING": ice.get("sea_level_forcing", 0),
        "CHUNK_NUMBER_pism": config["general"]["chunk_number"],
        "NYEAR": config["general"]["nyear"],
        "iter_coup_interact_method_ice2oce": "BASALSHELF_WATER_ICEBERG_MODEL",
        "orog_reference_pism": ice.get("orog_reference_pism", 1),
        "first_year_in_chunk_input": ice["experiment_input_dir"]
        + "/"
        + config["general"]["expid"]
        + "_pismr_input_"
        + config["general"]["chunk_start_date"].syear
        + "0101-"
        + str(
            int(config["general"]["chunk_start_date"].syear)
            + int(config["general"]["nyear"] - 1)
        )
        + "1231.nc",
        "last_year_in_chunk_restart": ice["restart_out_targets"]["restart"],
        "USE_YMONMEAN": ice.get("use_ymonmean", 0),
        "CRITICAL_THK_FOR_MASK_pism": ice.get("thk_threshold", 5.0),
        "MIN_MON_SELECT": int(ice.get("select_min_glacial_depth", 1)),
        "TEMP2_BIAS_FILE": ice.get("temp2_bias_file", ""),

        # bias correction
        "ANOMALY_AIR_TEMPERATURE": int(ice.get("ANOMALY_AIR_TEMPERATURE", 0)),
        "ANOMALY_PRECIPITATION": int(ice.get("ANOMALY_PRECIPITATION", 0)),
        "ANOMALY_OCEAN_TEMPERATURE": int(ice.get("ANOMALY_OCEAN_TEMPERATURE", 0)),
        "ANOMALY_OCEAN_SALINITY": int(ice.get("ANOMALY_OCEAN_SALINITY", 0)),
        "REFERENCE_ATMOS_FNAME": ice.get("REFERENCE_ATMOS_FNAME", ""),
        "BASE_STATE_ATMOS_FNAME": ice.get("BASE_STATE_ATMOS_FNAME", ""),
        "REFERENCE_OCEAN_FNAME": ice.get("REFERENCE_OCEAN_FNAME", ""),
        "BASE_STATE_OCEAN_FNAME": ice.get("BASE_STATE_OCEAN_FNAME", ""),
    }

    environment_dict.update(ice_domain_env.ice_domain_environment(config))

    print(environment_dict)
    return environment_dict
