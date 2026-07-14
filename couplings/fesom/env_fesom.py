def prepare_environment(config):
    environment_dict = {
            "ICE_TO_FESOM": int(config["fesom"].get("use_icebergs", False).__bool__()),
            "CHANGE_OCEAN": int(config["fesom"].get("change_ocean", False).__bool__()),
            "FESOM_TO_ICE": int(config["general"]["first_run_in_chunk"]),
            "MESH_DIR_fesom": config["fesom"]["mesh_dir"],
            # FESOM install bin/ -- holds the native mesh partitioner
            # (fesom_meshpart, built for the -is variant via
            # -DBUILD_MESHPARTITIONER=ON) that build_submesh uses instead of the
            # meshtools Singularity container (user namespaces disabled on levante).
            "FESOM_BIN_DIR": config["fesom"]["model_dir"] + "/bin",
            # Max-mesh the dynamic submesh is carved from. Defaults to the
            # running mesh (mesh_dir) but can be overridden per experiment via
            # `fesom: { max_mesh: /path/to/larger_mesh/ }` in the runscript.
            "MAX_MESH": config["fesom"].get("max_mesh", config["fesom"]["mesh_dir"]),
            # Mesh the FIRST awiesm3 chunk ran on. Normally chunk 1 runs on the full
            # (max) mesh and this stays empty. If chunk 1 is pre-staged onto a
            # submesh from the pool (couple_in skipped there), the first mesh-change
            # leg has no `previous_submesh` to fall back on and would wrongly assume
            # the old mesh was the full mesh -- set this so it uses the right one.
            "CHUNK1_MESH": config["fesom"].get("chunk1_mesh", ""),
            # Node grid-description of the max-mesh, expected inside MAX_MESH.
            "MESH_GRIDDES_fesom": config["fesom"].get("griddes_nodes", "core2_griddes_nodes.nc"),
            "MESH_ROTATED_fesom": config["fesom"]["mesh_rotated"],
            # Number of FESOM MPI tasks; the submesh must be partitioned to match.
            "NPROC_fesom": config["fesom"]["nproc"],
            "DATA_DIR_fesom": config["fesom"]["experiment_outdata_dir"],
            "RESTART_DIR_fesom": config["fesom"]["experiment_restart_in_dir"],
            "COUPLE_DIR": config["general"]["experiment_couple_dir"],
            # Run work dir (holds the generated namcouple); used by
            # fix_namcouple_feom_dim to retag the feom grid to the submesh count.
            "WORK_DIR_fesom": config["general"]["thisrun_work_dir"],
            "number_of_years_for_forcing": config["model1"]["chunk_size"],
            "CHUNK_SIZE_pism_standalone": config["model2"]["chunk_size"],
            "CHUNK_START_DATE_fesom": config["general"]["chunk_start_date"],
            "CHUNK_END_DATE_fesom": config["general"]["chunk_end_date"],
            "FUNCTION_PATH": config["fesom"]["workflow"]["subjobs"]["couple_in"]["script_dir"],
            "PYFESOM_PATH": "/pf/a/a270124/pyfesom2/",
            "EXP_ID": config["general"]["command_line_config"]["expid"],
            "iter_coup_regrid_method_ice2oce": "INTERPOLATE",
            # ocean->ice-sheet interaction method. DIRECT = hand PISM FESOM's own
            # cavity fluxes (fw -> shelfbmassflux, sst@icebase -> shelfbtemp) via
            # -ocean given: one model owns the ice-ocean interface. The default
            # OCEANTEMPSALT (-ocean th) makes PISM recompute melt from a T/S
            # column average, which fed it surface water (up to +6.5 degC at the
            # ice base) and collapsed the shelves.
            "iter_coup_interact_method_oce2ice": config[config["general"]["setup_name"]].get("oce2ice_method", "OCEANTEMPSALT"),
            #"BASIN_FILE": config["fesom"].get("basin_file"),
            "MACHINE": config["computer"]["name"],
            "ICEBERG_DIR": config["fesom"].get("iceberg_dir", ""),

            # --- OASIS feom grid + remap-weight regeneration (ice2fesom) ---
            # Opt-in (default off) so non-dynamic setups are unaffected. When on,
            # ice2fesom regenerates grids/masks/areas + rmp_*feom* for the new
            # submesh into ${COUPLE_DIR}/oasis_regen via ocp-tool. The driver and
            # srun-worker pythons (and ocp-tool dir / template oasis dir) are
            # site-specific and set in the runscript fesom block.
            "REGEN_OASIS_WEIGHTS": int(config["fesom"].get("regen_oasis_weights", False).__bool__()),
            # Coupled model's OASIS build (has python/pyoasis + lib/liboasis.cbind.so).
            # Defaults to the oasis3mct model_dir; override with
            # `fesom: { oasis_build_path: ... }` if pyOASIS lives in another build.
            "OASIS_BUILD_PATH": config["fesom"].get(
                "oasis_build_path", config.get("oasis3mct", {}).get("model_dir", "")),
            "OCP_TOOL_DIR": config["fesom"].get("ocp_tool_dir", ""),
            "OCP_WEIGHTGEN_DRIVER_PY": config["fesom"].get("ocp_weightgen_driver_py", ""),
            "OCP_WEIGHTGEN_WORKER_PY": config["fesom"].get("ocp_weightgen_worker_py", ""),
            "OCP_WEIGHTGEN_THREADS": config["fesom"].get("ocp_weightgen_threads", 8),
            # Account/partition for the weight-gen srun when ice2fesom runs on the
            # login node (no allocation to inherit).
            "OCP_WEIGHTGEN_ACCOUNT": config["general"].get("account", ""),
            "OCP_WEIGHTGEN_PARTITION": config["fesom"].get("ocp_weightgen_partition", "compute"),
            "OASIS_REMAP_METHOD": config["fesom"].get("oasis_remap_method", "existing"),
            # Dir providing the atmosphere (A096) + runoff (RnfO) OASIS grids to
            # seed from; feom is overwritten for the new submesh.
            "OASIS_TEMPLATE_DIR": config["fesom"].get("oasis_template_dir", ""),
            # Dir with the existing rmp_*.nc. ocp-tool only regenerates the
            # mesh-dependent (feom) weights; the unchanged ones (e.g. runoff
            # R096->RnfA) are symlinked into oasis_regen from here so the OASIS
            # staging finds the complete set.
            "OASIS_RMP_TEMPLATE_DIR": config["fesom"].get("oasis_rmp_template_dir", ""),
            # ocp-tool config template (e.g. configs/TCO95_CORE2.yaml) driving the
            # full per-submesh atm-side regen (OASIS A096/feom/RnfO masks + the
            # modified OIFS ICMGG lsm/slt + runoff LSM + LPJ-GUESS slt). Its
            # input/ dirs must hold the base ICMGG<expid>INIT to modify.
            "OCP_TEMPLATE_CONFIG": config["fesom"].get("ocp_template_config", ""),
            # Output tag for the regenerated grid (output subdir + ICMGG suffix).
            "OCP_REGEN_GRID_TAG": config["fesom"].get("ocp_regen_grid_tag", "feomdyn"),

            #"FESOM_GRID_input": config["fesom"]["grid_input"],
            #"solidearth_ice_thickness_file":(
            #    config["general"]["experiment_couple_dir"] +
            #    "/ice_thickness.nc"
            #    ),
            #"ADD_UNCHANGED_ICE": config["vilma"].get("add_unchanged_ice", False),
            #"EISLASTFILE_vilma":  (
            #    config["vilma"]["experiment_input_dir"] +
            #    "/" +
            #    config["vilma"]["eislastfile"]
            #    ),
            #"RUN_NUMBER_vilma": config["general"]["run_number"],
            #"RUN_DATE_STAMP": config["general"]["run_datestamp"],
            #"LAST_RUN_DATE_STAMP": config["general"]["last_run_datestamp"],
            #"INITIAL_YEAR_vilma": config["general"]["initial_date"].syear,
            #"NYEAR_vilma_standalone": config["general"]["nyear"],
            #"FINAL_YEAR_vilma": config["general"]["final_date"].syear,
            #"EISLASTCONF_vilma":(
            #    config["vilma"]["experiment_config_dir"] +
            #    "/inp/" +
            #    config["vilma"]["eislastconf"]
            #    )

            }
    
    #if environment_dict["ADD_UNCHANGED_ICE"] == False:
    #    environment_dict["ADD_UNCHANGED_ICE"] = 0
    #elif environment_dict["ADD_UNCHANGED_ICE"] == True:
    #    environment_dict["ADD_UNCHANGED_ICE"] = 1


    print(environment_dict)
    return environment_dict




