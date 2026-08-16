import os
import subprocess
import importlib.util


def _find_ocp_tool_dir():
    """Locate the ocp-tool install without hardcoding a path.

    esm_tools installs ocp-tool as the importable ``ocp_tool`` package
    (``general.required_plugins`` -> pip, see esm_plugin_manager), so we ask
    Python where it went. The repo root that holds ``configs/`` and
    ``environment.yaml`` is the parent of the package directory.
    """
    try:
        spec = importlib.util.find_spec("ocp_tool")
    except (ImportError, ValueError):
        spec = None
    if spec and spec.origin:
        return os.path.dirname(os.path.dirname(spec.origin))
    return ""


def _find_pyfesom_dir():
    """Locate pyfesom2 without hardcoding a path.

    pyfesom2 is a dependency of ocp-tool (``general.required_plugins`` -> pip), so
    it installs alongside it and Python can find it. Returns the package directory
    (so a symlink to it is importable as ``pyfesom2``), or "" if not importable.
    """
    try:
        spec = importlib.util.find_spec("pyfesom2")
    except (ImportError, ValueError):
        spec = None
    if spec and spec.origin:
        return os.path.dirname(spec.origin)
    return ""


def _ocp_env_name(ocp_tool_dir):
    """Conda env name ocp-tool declares in its ``environment.yaml`` (``name:``).

    Falls back to ``ocp-tool2`` (the name shipped in the repo) if the file is
    missing or unreadable.
    """
    env_yaml = os.path.join(ocp_tool_dir or "", "environment.yaml")
    try:
        with open(env_yaml) as fh:
            for line in fh:
                if line.strip().startswith("name:"):
                    return line.split(":", 1)[1].strip()
    except OSError:
        pass
    return "ocp-tool2"


def _conda_env_python(env_name):
    """Absolute python path for a named conda env, via ``conda env list``.

    Works for any ``envs_dir`` independent of the active base (conda lists every
    known env by full path; we match on the basename). Returns "" if conda is
    unavailable or the env is not found, so callers can fall back to an explicit
    config value.
    """
    if not env_name:
        return ""
    conda = os.environ.get("CONDA_EXE") or "conda"
    try:
        out = subprocess.check_output(
            [conda, "env", "list"], stderr=subprocess.DEVNULL
        ).decode()
    except (OSError, subprocess.CalledProcessError):
        return ""
    for line in out.splitlines():
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        path = line.split()[-1]  # named or path-only entries both end in the path
        if os.path.basename(path) == env_name:
            py = os.path.join(path, "bin", "python")
            if os.path.exists(py):
                return py
    return ""


def coupling_identity(config):
    """Identity tag of a harvested bootstrap set: <fesom-mesh>_<pism-grid>_<oifs-res>.

    Empty if general.pism_grid_tag is unset (harvest then skips with a warning).
    """
    pism_tag = config["general"].get("pism_grid_tag", "")
    if not pism_tag:
        return ""
    mesh = os.path.basename(
        os.path.normpath(config["fesom"].get("max_mesh", config["fesom"]["mesh_dir"]))
    )
    oifs_res = config.get("oifs", {}).get("resolution", "")
    return f"{mesh}_{pism_tag}_{oifs_res}"


def harvest_environment(config):
    """Env vars for couplings/general/harvest.functions (serial pool harvest)
    and the concurrent-coupling markers/sentinels."""
    general = config["general"]
    concurrent = general.get("coupling_mode", "serial") == "concurrent"
    return {
        "COUPLING_MODE": general.get("coupling_mode", "serial"),
        "CHUNK_NUMBER": general.get("chunk_number", 0),
        "COUPLING_FAIL_SUFFIX": "." + general["setup_name"] if concurrent else "",
        "COUPLING_IDENTITY": coupling_identity(config),
        "HARVEST_PARALLEL_INI": int(bool(general.get("harvest_parallel_ini", False))),
        "HARVEST_POOL_DIR": general.get("harvest_pool_dir") or general.get("pool_dir", ""),
    }



def _oifs_source(config, key):
    """Pool path of one of OIFS's unmodified initial files.

    Built from ``prepifs_dir`` and ``prepifs_expid`` the same way oifs.yaml
    builds its own ``input_sources``, rather than read from ``input_sources``
    itself: ``reuse_sources`` rewrites that to the experiment's pooled copy from
    run 2 on, and renames it to ``input_expid``, so it stops being a pool path.
    ICMCL is the odd one out, since the pool file keeps the input id while the
    link has to carry the prepifs id.
    """
    oifs = config.get("oifs", {})
    prepifs_dir = str(oifs.get("prepifs_dir", "") or "").rstrip("/")
    expid = str(oifs.get("prepifs_expid", "") or "")
    if key == "ICMCL_INIT":
        icmcl_dir = str(oifs.get("icmcl_dir", "") or "").rstrip("/")
        icmcl_file = str(oifs.get("icmcl_file", "") or "")
        return f"{icmcl_dir}/{icmcl_file}" if icmcl_dir and icmcl_file else ""
    if not prepifs_dir or not expid:
        return ""
    suffix = {
        "ICMGG_INIT": str(oifs.get("ICMGG_INIT_name", "") or ""),
        "ICMSH_INIT": str(oifs.get("ICMSH_INIT_name", "") or ""),
    }.get(key, "")
    stem = {"ICMGG_INIT": "ICMGG", "ICMGG_INIUA": "ICMGG",
            "ICMSH_INIT": "ICMSH"}.get(key)
    if not stem:
        return ""
    tail = "INIUA" if key.endswith("INIUA") else "INIT"
    return f"{prepifs_dir}/{stem}{expid}{tail}{suffix}"


def prepare_environment(config):
    # --- Auto-discover the ocp-tool OASIS-regen toolchain (ice2fesom) ---------
    # esm_tools installs ocp-tool (required_plugin) and the coupled model's OASIS
    # build, so we derive their locations here instead of hardcoding them in
    # every runscript. Every value still honours an explicit runscript override.
    fesom = config["fesom"]
    general = config["general"]

    ocp_tool_dir = fesom.get("ocp_tool_dir") or _find_ocp_tool_dir()
    resolution = general.get("resolution") or (
        f"{config['oifs']['resolution']}_{fesom['resolution']}"
    )
    # Driver env: the conda env ocp-tool declares (pyfesom2 + eccodes). Worker
    # env: the OASIS/mpi4py env (default 'ece4', overridable by NAME). Both are
    # resolved to their python by conda, so only a stable env name is needed --
    # not a full path in the runscript.
    driver_py = fesom.get("ocp_weightgen_driver_py") or _conda_env_python(
        _ocp_env_name(ocp_tool_dir)
    )
    worker_py = fesom.get("ocp_weightgen_worker_py") or _conda_env_python(
        fesom.get("ocp_weightgen_worker_env", "ece4")
    )

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
            "PYFESOM_PATH": config["fesom"].get("pyfesom_path") or _find_pyfesom_dir(),
            "EXP_ID": config["general"]["command_line_config"]["expid"],
            "iter_coup_regrid_method_ice2oce": "INTERPOLATE",
            # ocean->ice-sheet interaction method. DIRECT = hand PISM FESOM's own
            # cavity fluxes (fw -> shelfbmassflux, sst@icebase -> shelfbtemp) via
            # -ocean given: one model owns the ice-ocean interface. The default
            # OCEANTEMPSALT (-ocean th) makes PISM recompute melt from a T/S
            # column average, which fed it surface water (up to +6.5 degC at the
            # ice base) and collapsed the shelves.
            "iter_coup_interact_method_oce2ice": config["fesom"].get("oce2ice_method", "OCEANTEMPSALT"),
            # Debug: flip the o2a (rstos-backed) namcouple fields to EXPOUT so
            # OASIS dumps every exchange to netcdf (see fix_namcouple_feom_dim).
            "OASIS_EXPOUT_O2A": int(config["fesom"].get("oasis_expout_o2a", False).__bool__()),
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
            # Built by esm_master at ${general.model_dir}/oasis; falls back to the
            # oasis3mct model_dir. Override with `fesom: { oasis_build_path: ... }`
            # if pyOASIS lives in another build.
            "OASIS_BUILD_PATH": (
                fesom.get("oasis_build_path")
                or config.get("oasis3mct", {}).get("model_dir")
                or (general.get("model_dir", "").rstrip("/") + "/oasis")),
            # Discovered from the installed ocp_tool package (see _find_ocp_tool_dir).
            "OCP_TOOL_DIR": ocp_tool_dir,
            # Resolved by conda from the env names (driver = ocp-tool's own
            # environment.yaml; worker = 'ece4'); see helpers above.
            "OCP_WEIGHTGEN_DRIVER_PY": driver_py,
            "OCP_WEIGHTGEN_WORKER_PY": worker_py,
            "OCP_WEIGHTGEN_THREADS": config["fesom"].get("ocp_weightgen_threads", 64),
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
            # staging finds the complete set. Derived from the pool layout
            # (<pool>/oasis/cy<oifs.version>/<oifs.res>-<fesom.res>/<nproc>).
            "OASIS_RMP_TEMPLATE_DIR": (
                fesom.get("oasis_rmp_template_dir")
                or (f"{general.get('pool_dir', '').rstrip('/')}/oasis/"
                    f"cy{config.get('oifs', {}).get('version', '')}/"
                    f"{config.get('oifs', {}).get('resolution', '')}-{fesom.get('resolution', '')}/"
                    f"{fesom.get('nproc', '')}")),
            # ocp-tool config template driving the full per-submesh atm-side regen
            # (OASIS A096/feom/RnfO masks + the modified OIFS ICMGG lsm/slt +
            # runoff LSM + LPJ-GUESS slt). Its input/ dirs must hold the base
            # ICMGG<expid>INIT to modify. Derived as <ocp_tool_dir>/configs/
            # <resolution>.yaml (e.g. configs/TCO95_CORE3.yaml).
            "OCP_TEMPLATE_CONFIG": (
                fesom.get("ocp_template_config")
                or f"{ocp_tool_dir}/configs/{resolution}.yaml"),
            # Output tag for the regenerated grid (output subdir + ICMGG suffix).
            "OCP_REGEN_GRID_TAG": config["fesom"].get("ocp_regen_grid_tag", "feomdyn"),
            # ocp-tool reads the unmodified OIFS initial files from its own
            # input/openifs_input_default/, named after the experiment id in its
            # config. A fresh checkout ships only a small sample set, so the
            # files this experiment needs are linked in at runtime. Sources are
            # the ones OIFS itself uses, so the two cannot drift apart.
            "OCP_OPENIFS_INPUT_DIR": (
                f"{ocp_tool_dir}/input/openifs_input_default" if ocp_tool_dir else ""),
            "OIFS_PREPIFS_EXPID": config.get("oifs", {}).get("prepifs_expid", ""),
            "OIFS_ICMGG_INIT": _oifs_source(config, "ICMGG_INIT"),
            "OIFS_ICMGG_INIUA": _oifs_source(config, "ICMGG_INIUA"),
            "OIFS_ICMSH_INIT": _oifs_source(config, "ICMSH_INIT"),
            "OIFS_ICMCL_INIT": _oifs_source(config, "ICMCL_INIT"),

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
    environment_dict.update(harvest_environment(config))

    #if environment_dict["ADD_UNCHANGED_ICE"] == False:
    #    environment_dict["ADD_UNCHANGED_ICE"] = 0
    #elif environment_dict["ADD_UNCHANGED_ICE"] == True:
    #    environment_dict["ADD_UNCHANGED_ICE"] = 1


    print(environment_dict)
    return environment_dict




