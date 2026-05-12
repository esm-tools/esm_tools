#!/bin/bash
# Test harness for the PISM<->ECHAM<->FESOM<->OpenIFS coupling pipeline.
#
# All runtime artifacts are placed under TEST_WORK_DIR (default /work/...),
# so the source tree (couplings/pism/) stays clean across runs. Override
# the location with:   TEST_WORK_DIR=/somewhere/else ./test.sh

# Capture script dir before anything touches PWD.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
echo "Script directory: ${SCRIPT_DIR}"

TEST_WORK_DIR="${TEST_WORK_DIR:-/work/ab0246/a270092/cavity_coupling_test}"
echo "Work directory:   ${TEST_WORK_DIR}"

# Check for clean flag
if [[ "$1" == "clean" ]] || [[ "$1" == "--clean" ]]; then
    echo "[CLEAN] Removing ${TEST_WORK_DIR}"
    rm -rf "${TEST_WORK_DIR}"
    echo "[CLEAN] Done. Run ./test.sh to start fresh test."
    exit 0
fi

mkdir -p "${TEST_WORK_DIR}"

# Save python3 path and HOME before env file overwrites them.
PYTHON3_DIR="$(dirname "$(which python3)")"
ORIG_HOME="${HOME}"

echo "Sourcing environments..."
source "${SCRIPT_DIR}/env_pism2awiesm.txt"

# Restore HOME (env file sets it to a different user)
export HOME="${ORIG_HOME}"
echo "...done"

# Set COUPLE_DIR inside the work dir.
export COUPLE_DIR="${TEST_WORK_DIR}/test_couple"
mkdir -p "${COUPLE_DIR}"
echo "Using COUPLE_DIR: ${COUPLE_DIR}"

# Copy grid files if they don't exist
if [ ! -f "${COUPLE_DIR}/ice.griddes" ]; then
    echo "[SETUP] Copying required grid description files..."
    cp /work/ab0246/a270122/lars_and_paul/experiments/test_env/couple/ice.griddes "${COUPLE_DIR}/"
    cp /work/ab0246/a270122/lars_and_paul/experiments/test_env/couple/ocean.griddes "${COUPLE_DIR}/"
fi

# Override OUTPUT_DIR_pism from env file to use readable directory
export OUTPUT_DIR_pism="/work/ab0246/a270122/lars_and_paul/experiments/test_env/outdata/pism"
echo "Reading PISM output from: ${OUTPUT_DIR_pism}"

# Restore python3 to PATH (env file overwrites it)
export PATH="${PYTHON3_DIR}:${PATH}"
export PATH="/sw/spack-levante/singularity-3.8.5-w53g5a/bin/:${PATH}"
export PATH="${PATH}:/usr/bin/"
export PATH2COUPLE="${COUPLE_DIR}"
export MAX_MESH='/work/ba0989/a270124/PalModII/experiments/ICEBERGS/mesh_core2/'
export MAXMESH_DIR_fesom="${MAX_MESH}"
export CHANGE_OCEAN='1'
export ICE_TO_FESOM=1

# remap_fesom_restart inputs.
# RESTART_DIR_fesom is the dir holding the old-mesh restart .nc files
# (temp.nc, salt.nc, ...). In production this comes from env_fesom.py
# (config["fesom"]["experiment_restart_in_dir"]). For the test, point at the
# reference restart directly.
export RESTART_DIR_fesom='/work/ab0246/a270122/lars_and_paul/experiments/E130ka_coupled2/restart/fesom/fesom.5076.oce.restart'

# Pre-build remap_restart if the binary is missing.
REMAP_BIN="${SCRIPT_DIR}/../fesom/remap_restart/remap_restart"
if [ ! -x "${REMAP_BIN}" ]; then
    echo "[SETUP] Building remap_restart..."
    bash "${SCRIPT_DIR}/../fesom/remap_restart/build.sh"
fi

# Override FUNCTION_PATH (env_pism2awiesm.txt points to a stale tree) so this
# test exercises the *local* esm_tools sources, not the captured one.
export FUNCTION_PATH="${SCRIPT_DIR}"

# build_submesh needs core2_griddes_nodes.nc in cwd; it is too large for git
# so symlink it from the reference tree into the work dir when missing.
if [ ! -e "${TEST_WORK_DIR}/core2_griddes_nodes.nc" ]; then
    ln -sf /work/ab0246/a270122/lars_and_paul/esm_tools/couplings/pism/core2_griddes_nodes.nc \
           "${TEST_WORK_DIR}/core2_griddes_nodes.nc"
fi
export SUBMESH_DIR="submesh_5007-12-31T00:00:00"

# Copy submesh for ocp-tool testing (needs mesh_cst.nc + nod2d.out, etc.).
if [ ! -f "${COUPLE_DIR}/${SUBMESH_DIR}/mesh_cst.nc" ]; then
    echo "[SETUP] Copying submesh test data..."
    mkdir -p "${COUPLE_DIR}/${SUBMESH_DIR}"
    cp -r /work/ab0246/a270122/lars_and_paul/experiments/test_env/couple/${SUBMESH_DIR}/* "${COUPLE_DIR}/${SUBMESH_DIR}/"
fi

# OCP-tool environment variables (hardcoded for testing; in production these
# come from env_pism.py via ESM-Tools config)
export OCP_POOL_DIR="/work/ab0246/a270092/input"
export OCP_OIFS_RES="TCO95"
export OCP_OIFS_RES_NUMBER=95
export OCP_OIFS_TRUNCATION="TCO"
export OCP_OIFS_LEVELS="L91"
export OCP_OIFS_PREPIFS_EXPID="ab45"
export OCP_OIFS_INPUT_EXPID="awi3"
export OCP_OIFS_VERSION="48r1"
export OCP_FESOM_RES="CORE2"

# Source coupling functions from the repo (absolute paths). Stays unaffected
# by the cwd change below.
. "${SCRIPT_DIR}/coupling_pism2esm.functions"
. "${SCRIPT_DIR}/../fesom/coupling_ice2fesom_interactive_mesh.functions"
. "${SCRIPT_DIR}/../oifs/coupling_ice2oifs.functions"

# Run everything from the work dir so all cwd-relative artifacts
# (topo_raw.txt, cavity_raw.txt, mask.txt, *.mod, fesom_submesh.x, *.log)
# land there instead of polluting the source tree.
cd "${TEST_WORK_DIR}"

echo "[TEST] Running pism2esm coupling..."
pism2esm 2>> ./stderr_awiesm
echo "[TEST] Running ice2fesom coupling..."
ice2fesom 2>> ./stderr_fesom

echo "[TEST] Activating ocp-tool conda env..."
source "${ORIG_HOME}/loadconda.sh"
conda activate ocp-tool2
# ocp_tool is checked out as a source tree (not pip-installed into the env)
export PYTHONPATH="/work/ab0246/a270092/software/ocp-tool:${PYTHONPATH}"

echo "[TEST] Running ice2oifs (ocp-tool) coupling..."
ice2oifs 2>> ./stderr_oifs

conda deactivate
