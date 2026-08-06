#!/bin/bash
# Executable entry point for the couple_in subjob.
#
# esm_runscripts has two ways of running a subjob's `script` (dataprocess.py):
# the shell path sources it and then calls `call_function`, while the batch path
# runs `srun <script>` and ignores `call_function` altogether. A functions file
# works on the first and dies on the second with
#
#   execve(): coupling_ice2fesom_interactive_mesh.functions: Permission denied
#
# because it has no shebang and no execute bit. This wrapper is what the batch
# path needs: a real script that sources the functions and calls ice2fesom
# itself. Point the subjob at this and drop `call_function`, and both paths then
# do the same thing.
#
# Everything ice2fesom reads comes from the environment esm_runscripts exports
# ahead of the task list, and srun passes that through.
set -eu

# Undo any thread cap inherited from the launcher. esm_runscripts is started on
# a login node where numpy's OpenBLAS wants 128 threads at import and RLIMIT_NPROC
# will not give them, so the launcher is capped; SLURM then propagates that
# environment into this job. build_submesh runs fesom_mesh_to_cdo multithreaded
# and must not inherit the cap, so set the threading from what the allocation
# actually gives us.
_cpus=${SLURM_CPUS_PER_TASK:-${SLURM_CPUS_ON_NODE:-1}}
export OMP_NUM_THREADS=${OMP_NUM_THREADS_ICE2FESOM:-${_cpus}}
export OPENBLAS_NUM_THREADS=${OMP_NUM_THREADS}
export MKL_NUM_THREADS=${OMP_NUM_THREADS}
export NUMEXPR_NUM_THREADS=${OMP_NUM_THREADS}
echo "run_ice2fesom: threads set to ${OMP_NUM_THREADS}"

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
funcs=${here}/coupling_ice2fesom_interactive_mesh.functions

if [ ! -r "${funcs}" ]; then
    echo "run_ice2fesom: cannot read ${funcs}" >&2
    exit 1
fi

# shellcheck source=/dev/null
. "${funcs}"
ice2fesom
