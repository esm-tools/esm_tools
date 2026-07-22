import os
import sys
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


def _self_can_drive():
    """True when running inside a virtual env that already carries the whole
    ocp-tool weight-regen toolchain.

    In the esm_tools venv the required packages install ocp_tool + pyfesom2 +
    eccodes right here, so the weight-regen driver should be THIS interpreter:
    then OCP_TOOL_DIR (discovered by find_spec in this same interpreter) and the
    driver share one Python and cannot ABI-mismatch a differently-versioned conda
    env. Guarded on actually being in a venv so non-venv runs are unchanged and
    keep using the conda env ocp-tool declares.
    """
    if sys.prefix == sys.base_prefix:  # not in a venv -> use the conda env
        return False
    for mod in ("ocp_tool", "pyfesom2", "eccodes"):
        try:
            if importlib.util.find_spec(mod) is None:
                return False
        except (ImportError, ValueError):
            return False
    return True


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
    # Driver interpreter: prefer THIS Python when it already carries the ocp-tool
    # toolchain (the esm_tools venv, populated by the required packages), so
    # OCP_TOOL_DIR and the driver stay on one interpreter and cannot ABI-mismatch.
    # Otherwise fall back to the conda env ocp-tool declares (pyfesom2 + eccodes).
    # Worker env: the OASIS/mpi4py env (default 'ece4', overridable by NAME). All
    # resolved to their python here, so no full path is needed in the runscript.
    driver_py = (
        fesom.get("ocp_weightgen_driver_py")
        or (sys.executable if _self_can_drive() else "")
        or _conda_env_python(_ocp_env_name(ocp_tool_dir))
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




