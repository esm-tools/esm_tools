#!/bin/bash
# Prebuild awiesm3 chunk <chunk>'s submesh and OASIS set from the PISM chain
# (general.couple_in_prebuild_in_pism; see prebuild_couple_in in
# coupling_ice2fesom_interactive_mesh.functions).
#
#   prebuild_couple_in.sh <couple_dir> <awiesm3 chunk> [<pism outdata dir>]
#
# Runs in a clean shell with the awiesm3 couple_in environment that chunk-1's
# couple_namcouple saved, so the PISM chain's own modules and variables cannot
# leak in (pism2esm deliberately never sources the fesom coupling files), plus
# this allocation's SLURM variables so the weight generation sruns into it.
# Exit 0 without a .ready file means "nothing prebuilt"; ice2fesom then builds
# everything itself.

couple_dir=${1:?usage: prebuild_couple_in.sh <couple_dir> <awiesm3 chunk>}
chunk=${2:?usage: prebuild_couple_in.sh <couple_dir> <awiesm3 chunk>}
pism_outdir=${3:-}
envf=${couple_dir}/prebuild/env.chunk_${chunk}.sh

if [ ! -f "${envf}" ]; then
    echo " *   ${envf} not there (awiesm3 chunk $(( chunk - 1 )) has not started); nothing prebuilt"
    exit 0
fi

slurmf=$(mktemp "${couple_dir}/prebuild/slurm_env.XXXXXX") || exit 1
declare -p | grep -E '^declare -[a-zA-Z-]* SLURM_' > "${slurmf}"

exec env -i HOME="${HOME}" USER="${USER}" LOGNAME="${LOGNAME:-${USER}}" TERM=dumb \
    bash --noprofile --norc -c "
        . '${envf}' || exit 1
        . '${slurmf}'; rm -f '${slurmf}'
        [ -n '${pism_outdir}' ] && export OUTPUT_DIR_pism='${pism_outdir}'
        . \"\${FUNCTION_PATH}/coupling_ice2fesom_interactive_mesh.functions\" || exit 1
        # A failure here must not stop the awiesm3 chain: no sentinel, just the exit.
        couple_fail() { echo \" *** prebuild: \$1 ***\"; exit 1; }
        prebuild_couple_in
    "
