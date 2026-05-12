#!/bin/ksh
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

echo "Sourcing environments..."
source ./env_pism2awiesm.txt
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

export PATH="/sw/spack-levante/mambaforge-22.9.0-2-Linux-x86_64-kptncg/bin/:${PATH}"
export PATH="/sw/spack-levante/singularity-3.8.5-w53g5a/bin/:${PATH}"
export PATH="/usr/bin/:${PATH}"
export PATH2COUPLE=${COUPLE_DIR}
export MAX_MESH='/work/ba0989/a270124/PalModII/experiments/ICEBERGS/mesh_core2/'
export CHANGE_OCEAN='1'
export ICE_TO_FESOM=1
echo $PATH

# Source local updated coupling functions (use SCRIPT_DIR captured before PWD override)
. ${SCRIPT_DIR}/coupling_pism2esm.functions
. ${SCRIPT_DIR}/../fesom/coupling_ice2fesom_interactive_mesh.functions

echo "[TEST] Running pism2esm coupling..."
pism2esm 2>> ./stderr_awiesm
echo "[TEST] Running ice2fesom coupling..."
ice2fesom 2>> ./stderr_fesom
#pism2ocean 2>> ./stderr_pism2ocean

exit
