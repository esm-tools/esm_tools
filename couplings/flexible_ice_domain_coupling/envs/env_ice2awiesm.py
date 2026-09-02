"""Coupling environment for one ice sheet domain (couple_out, pism2awiesm).

Replaces the old ``env_pism2awiesm.py``. Same idea as ``env_ice.py``: the
component says which ice domain it is, and everything it writes ends up in that
domain's couple sub directory.
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

        "ICE_DOMAIN": ice_domain_env.this_ice_domain(config),

        "ATMOSPHERE_TO_PISM": int(config["general"]["first_run_in_chunk"]),
        "PISM_TO_ATMOSPHERE": int(config["general"]["last_run_in_chunk"]),
        "PISM_TO_OCEAN": int(ice.get("iceberg_coupling", False).__bool__()),
        "OCEAN_TO_PISM": int(config["general"]["first_run_in_chunk"]),
        "fesom_use_iceberg": int(ice.get("iceberg_coupling", False).__bool__()),

        "CHUNK_START_DATE_pism": config["general"]["chunk_start_date"],
        "CHUNK_END_DATE_pism": config["general"]["chunk_end_date"],
        "CHUNK_START_YEAR_pism": config["general"]["chunk_start_date"].syear,
        "CHUNK_END_YEAR_pism": config["general"]["chunk_end_date"].syear,
        "CHUNK_NUMBER_pism": config["general"]["chunk_number"],
        "CHUNK_SIZE_pism_standalone": ice_domain_env.this_chunk_size(config),
        "NYEAR": config["general"]["nyear"],

        "DOWNSCALE_TEMP": int(ice.get("downscale_temp", 1)),
        "DOWNSCALE_PRECIP": int(ice.get("downscale_precip", 1)),
        "DOWNSCALING_LAPSE_RATE": ice.get("lapse_rate", -0.005),

        "VERSION_pism": ice["version"]
        .replace("github", "")
        .replace("index", "")
        .replace("snowflake", "")[:3],
        "POOL_DIR_pism": ice["pool_dir"],
        "DOMAIN_pism": ice["domain"],
        "EXE_pism": ice["executable"],
        "RES_pism": ice["resolution"],
        "RUN_NUMBER_pism": config["general"]["run_number"],
        "EX_INT": ice["ex_interval"],

        "YR0_pism": config["general"]["start_date"].syear,
        "M0_pism": config["general"]["start_date"].smonth,
        "D0_pism": config["general"]["start_date"].sday,
        "END_YEAR_pism": config["general"]["end_date"].syear,
        "END_MONTH_pism": config["general"]["end_date"].smonth,
        "END_DAY_pism": config["general"]["end_date"].sday,
        "CURRENT_YEAR_pism": config["general"]["current_date"].syear,

        "MIN_MON_SELECT": int(ice.get("select_min_glacial_depth", 1)),
        "CRITICAL_THK_FOR_MASK_pism": ice.get("thk_threshold", 5.0),
        "OUTPUT_DIR_pism": ice["experiment_outdata_dir"],
        "RESTART_DIR_pism": ice["experiment_restart_out_dir"],
        "SPINUP_FILE_pism": ice["spinup_file"],
        "INPUT_FILE_pism": ice.get("cli_input_file_pism"),
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

        "iter_coup_interact_method_ice2oce": "BASALSHELF_WATER_ICEBERG_MODEL",
        "account_all_fw_input": ice.get("account_all_fw_input", 0),
        "USE_YMONMEAN": ice.get("use_ymonmean", 0),
        "orog_reference_pism": ice.get("orog_reference_pism", 1),
    }

    environment_dict.update(ice_domain_env.ice_domain_environment(config))

    print(environment_dict)
    return environment_dict
