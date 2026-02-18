#!/bin/ksh
echo "Sourcing environments..."
source ./env_pism2awiesm.txt
echo "env pism2awiesm"
#source ./env_pism2ocean.txt
echo "env pism2ocean"
echo "...done"
export PATH="/sw/spack-levante/mambaforge-22.9.0-2-Linux-x86_64-kptncg/bin/:${PATH}"
export PATH="/sw/spack-levante/singularity-3.8.5-w53g5a/bin/:${PATH}"
export PATH="/usr/bin/:${PATH}"
export PATH2COUPLE=${COUPLE_DIR}
export MAX_MESH='/work/ba0989/a270124/PalModII/experiments/ICEBERGS/mesh_core2/'
export CHANGE_OCEAN='1'
echo $PATH
. /work/ab0246/a270122/lars_and_paul/esm_tools/couplings/pism/coupling_pism2esm.functions
. /work/ab0246/a270122/lars_and_paul/esm_tools/couplings/fesom/coupling_ice2fesomUKK.functions
echo "UKK ******"
pism2esm 2>> ./stderr_awiesm
echo "UKK1 ******"
ice2fesom 2>> ./stderr_fesom
#pism2ocean 2>> ./stderr_pism2ocean

exit
