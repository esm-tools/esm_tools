import os


def prepare_environment(config):
    general = config["general"]
    environment_dict = {
            "NYEAR": general["nyear"],
            "COUPLE_DIR": general["experiment_couple_dir"],
            # for couplings/general/harvest.functions (serial pool harvest;
            # keep in sync with env_fesom.harvest_environment)
            "FUNCTION_PATH": general["esm_couplings_dir"] + "/oifs",
            "EXP_ID": general["command_line_config"]["expid"],
            "CHUNK_START_DATE_fesom": general.get("chunk_start_date", ""),
            "CHUNK_END_DATE_fesom": general.get("chunk_end_date", ""),
            "COUPLING_MODE": general.get("coupling_mode", "serial"),
            "CHUNK_NUMBER": general.get("chunk_number", 0),
            "COUPLING_IDENTITY": _coupling_identity(config),
            "HARVEST_PARALLEL_INI": int(bool(general.get("harvest_parallel_ini", False))),
            "HARVEST_POOL_DIR": general.get("harvest_pool_dir") or general.get("pool_dir", ""),
            }
    print (environment_dict)
    return environment_dict


def _coupling_identity(config):
    # <fesom-mesh>_<pism-grid>_<oifs-res>; empty when pism_grid_tag is unset
    pism_tag = config["general"].get("pism_grid_tag", "")
    if not pism_tag:
        return ""
    mesh = os.path.basename(
        os.path.normpath(config["fesom"].get("max_mesh", config["fesom"]["mesh_dir"]))
    )
    return f"{mesh}_{pism_tag}_{config.get('oifs', {}).get('resolution', '')}"
