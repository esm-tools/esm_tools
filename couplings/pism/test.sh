#!/bin/bash
# Save script directory BEFORE sourcing env file (which overrides PWD)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
echo "Script directory: ${SCRIPT_DIR}"

# Check for clean flag
if [[ "$1" == "clean" ]] || [[ "$1" == "--clean" ]]; then
    echo "[CLEAN] Removing all generated test files..."
    rm -rf ${SCRIPT_DIR}/test_couple/
    rm -f ${SCRIPT_DIR}/stderr_awiesm ${SCRIPT_DIR}/stderr_fesom
    echo "[CLEAN] Done. Run ./test.sh to start fresh test."
    exit 0
fi

# Save python3 path and HOME before env file overwrites them
PYTHON3_DIR="$(dirname "$(which python3)")"
ORIG_HOME="${HOME}"

echo "Sourcing environments..."
source ./env_pism2awiesm.txt

# Restore HOME (env file sets it to a different user)
export HOME="${ORIG_HOME}"
echo "env pism2awiesm"
#source ./env_pism2ocean.txt
echo "env pism2ocean"
echo "...done"

# Set COUPLE_DIR to a local writable directory
export COUPLE_DIR="${SCRIPT_DIR}/test_couple"
mkdir -p ${COUPLE_DIR}
echo "Using local COUPLE_DIR: ${COUPLE_DIR}"

# Copy grid files if they don't exist
if [ ! -f ${COUPLE_DIR}/ice.griddes ]; then
    echo "[SETUP] Copying required grid description files..."
    cp /work/ab0246/a270122/lars_and_paul/experiments/test_env/couple/ice.griddes ${COUPLE_DIR}/
    cp /work/ab0246/a270122/lars_and_paul/experiments/test_env/couple/ocean.griddes ${COUPLE_DIR}/
fi

# Override OUTPUT_DIR_pism from env file to use readable directory
export OUTPUT_DIR_pism="/work/ab0246/a270122/lars_and_paul/experiments/test_env/outdata/pism"
echo "Reading PISM output from: ${OUTPUT_DIR_pism}"

# Restore python3 to PATH (env file overwrites it)
export PATH="${PYTHON3_DIR}:${PATH}"
export PATH="/sw/spack-levante/singularity-3.8.5-w53g5a/bin/:${PATH}"
export PATH="${PATH}:/usr/bin/"
export PATH2COUPLE=${COUPLE_DIR}
export MAX_MESH='/work/ba0989/a270124/PalModII/experiments/ICEBERGS/mesh_core2/'
export CHANGE_OCEAN='1'
export ICE_TO_FESOM=1
export SUBMESH_DIR="submesh_5007-12-31T00:00:00"
echo $PATH

# Copy submesh for ocp-tool testing (needs mesh_cst.nc + nod2d.out, elem2d.out, etc.)
if [ ! -f ${COUPLE_DIR}/${SUBMESH_DIR}/mesh_cst.nc ]; then
    echo "[SETUP] Copying submesh test data..."
    mkdir -p ${COUPLE_DIR}/${SUBMESH_DIR}
    cp -r /work/ab0246/a270122/lars_and_paul/experiments/test_env/couple/${SUBMESH_DIR}/* ${COUPLE_DIR}/${SUBMESH_DIR}/
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

# Source local updated coupling functions (use SCRIPT_DIR captured before PWD override)
. ${SCRIPT_DIR}/coupling_pism2esm.functions
. ${SCRIPT_DIR}/../fesom/coupling_ice2fesomUKK.functions
. ${SCRIPT_DIR}/../oifs/coupling_ice2oifs.functions

echo "[TEST] Running pism2esm coupling..."
pism2esm 2>> ./stderr_awiesm
echo "[TEST] Running ice2fesom coupling..."
ice2fesom 2>> ./stderr_fesom
echo "[TEST] Running ice2oifs (ocp-tool) coupling..."
ice2oifs 2>> ./stderr_oifs
#pism2ocean 2>> ./stderr_pism2ocean

exit
