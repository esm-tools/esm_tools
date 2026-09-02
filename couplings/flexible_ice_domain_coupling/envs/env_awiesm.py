"""Coupling environment for the ESM side (ECHAM/JSBACH + FESOM).

Same as the single ice sheet version, plus the ice domain registry:
ICE_DOMAINS tells ice2echam and ice2fesom which ice sheets to loop
over and reduce.
"""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import ice_domain_env  # noqa: E402


def prepare_environment(config):
    environment_dict = {
            # general
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            "EXP_ID": config["general"]["command_line_config"]["expid"],
            "WORK_DIR": config["general"]["thisrun_work_dir"],
            # NOTE: AWIESM is a *coupled* setup, so esm_parser folds the runscript's
            # "awiesm:" section into "general" and deletes it (esm_parser.py, "if coupled_setup").
            # config["awiesm"] therefore does not exist here -- read the component instead.
            # All consumers use ${FUNCTION_PATH}/../<dir>/..., so echam vs fesom is equivalent.
            "FUNCTION_PATH": config["echam"]["workflow"]["subjobs"]["couple_in"]["script_dir"],
            "MACHINE": config["computer"]["name"],
            "CHUNK_SIZE": config["general"]["this_chunk_size"],  #!!!
            
            # ECHAM
            "ICE_TO_ECHAM": int(config["general"]["first_run_in_chunk"]),
            "ECHAM_TO_ICE": int(config["general"]["last_run_in_chunk"]),
            "RES_echam": config["echam"]["resolution"], 
            "RESTART_DIR_echam": config["echam"]["experiment_restart_out_dir"],
            "DATA_DIR_echam": config["echam"]["experiment_outdata_dir"],
            "INIT_DIR_echam": config["echam"]["experiment_input_dir"],
            "CHUNK_START_DATE_echam": config["general"]["chunk_start_date"],
            "CHUNK_END_DATE_echam": config["general"]["chunk_end_date"],
            "END_YEAR_echam": config["general"]["chunk_end_date"].syear,
            "END_MONTH_echam": config["general"]["chunk_end_date"].smonth,
            "END_DAY_echam": config["general"]["chunk_end_date"].sday,
            "FORCING_DIR_jsbach": config["jsbach"]["experiment_input_dir"],
            "RESTART_DIR_jsbach": config["jsbach"]["experiment_restart_out_dir"],
            "POOL_DIR_jsbach": config["computer"]["pool_dir"],
            "POOL_DIR_echam": config["computer"]["pool_dir"],
            
            # FESOM
            "ICE_TO_FESOM": int(config["general"]["first_run_in_chunk"]),
            "FESOM_TO_ICE": int(config["general"]["last_run_in_chunk"]),
            "MESH_DIR_fesom": config["fesom"]["mesh_dir"],
            "DATA_DIR_fesom": config["fesom"]["experiment_outdata_dir"],
            "CHUNK_START_DATE_fesom": config["general"]["chunk_start_date"],
            "CHUNK_END_DATE_fesom": config["general"]["chunk_end_date"],
            "ICEBERG_DIR": config["fesom"].get("iceberg_dir", ""),
            
            # user defined input
            "ISM_TO_ECHAM_update_orography": int(config["echam"].get("update_ice_orography", True).__bool__()), 
            "ISM_TO_ECHAM_update_glacial_mask": int(config["echam"].get("update_glacial_mask", True).__bool__()), 
            "ISM_TO_ECHAM_update_land_runoff": int(config["echam"].get("update_land_runoff", True).__bool__()),
            "oro_update_mod": config["echam"].get("oro_update_mod", 1),
            "oro_update_var": config["echam"].get("oro_update_var", "OROMEA"),
            "ECHAM_ALBEDO_ON_GLACIERS": config["echam"].get("albedo_on_glaciers", 0.7),
            "ECHAM_GLACIAL_THRESHOLD": config["echam"].get("glacial_threshold", 0.5),
            "paleo_time": config["echam"].get("paleo_time", "pi"),
            
            "MESH_ROTATED_fesom": config["fesom"]["mesh_rotated"], #??? TEST
            "iter_coup_regrid_method_ice2oce": config["fesom"].get("iter_coup_regrid_method_ice2oce", "INTERPOLATE"),
            
            "CONSTANT_FRESHWATER_HOSING_FILE": config["fesom"].get("constant_landice_hosing_file", ""),
            "CELL_AREA_FESOM_FILE": config["fesom"].get("cell_area_fesom_file", "fesom.mesh.diag.nc"),
            "PYFESOM_PATH": config["fesom"].get("pyfesom_path", "/pf/a/a270124/pyfesom2/"),
            "iter_coup_interact_method_oce2ice": config["fesom"].get(
                "iter_coup_interact_method_oce2ice", "OCEANTEMPSALT"
            ),

            # Gate for building the iceberg discharge in ice2fesom. Follows the
            # FESOM iceberg switch: "with_icb"/"use_icebergs" is what the
            # fesom-2.5/2.6 configs use, "fesom_use_iceberg" is the older
            # spelling still found in runscripts.
            "FESOM_PREP_ICEBERG_DISCHARGE": int(bool(
                config["fesom"].get(
                    "use_icebergs",
                    config["fesom"].get(
                        "fesom_use_iceberg",
                        config["general"].get("with_icb", False),
                    ),
                )
            )),

            # Wherever the ice sheets have to be merged into one global field
            # (iceberg discharge for the FESOM iceberg module) the domains may
            # live on different grids and need a common target grid. Defaults
            # to the ECHAM grid, which every domain can be remapped to.
            "ICE_DISCHARGE_TARGET_GRID": config["fesom"].get(
                "ice_discharge_target_grid",
                config["echam"]["resolution"] + "grid",
            ),
            "ICE_DISCHARGE_REMAP": config["fesom"].get(
                "ice_discharge_remap", "remapcon2"
            ),
            }

    # ICE_DOMAINS + the per domain box/weight attributes. This is what turns
    # the ECHAM and FESOM side into a loop over an arbitrary number of ice
    # sheets instead of the hard coded nh/sh pair.
    environment_dict.update(ice_domain_env.ice_domain_environment(config))

    print(environment_dict)
    return environment_dict




