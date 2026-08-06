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

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
funcs=${here}/coupling_ice2fesom_interactive_mesh.functions

if [ ! -r "${funcs}" ]; then
    echo "run_ice2fesom: cannot read ${funcs}" >&2
    exit 1
fi

# shellcheck source=/dev/null
. "${funcs}"
ice2fesom
