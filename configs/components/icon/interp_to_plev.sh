#!/bin/bash
# Postprocessing step icon_interp_to_plev (see choose_interp_to_plev in icon.yaml):
# interpolate the monthly and the daily 3d model-level output of one run to pressure
# levels by calling interp_to_pressure_levels.py twice. The monthly files are small and
# done first (seconds), then the daily files.
#
# Usage:
#   interp_to_plev.sh <path> <start_date> <end_date> \
#       <monthly_pattern> <monthly_ml_files> <daily_pattern> <daily_ml_files> [args...]
# <start_date>, <end_date>: YYYYMMDD (dates in the file names of the run)
# <*_ml_files>: move | remove | keep (see interp_to_pressure_levels.py)
# An empty pattern ("") skips that step. Further args are passed to both calls.
# The monthly step uses --missing-ok: a run that ends within a month (short test runs)
# has no monthly file. A missing daily file is an error.
# Both steps always run; the exit status is non-zero if one of them failed.

if [ $# -lt 7 ]; then
    sed -n '2,15p' "$0"
    exit 2
fi

# ESM-Tools starts this script with the machine's launcher. mpirun without -np (nesh)
# starts one copy per slot of the exclusive node, and the copies overwrite each other's
# output. Only the first copy (rank 0) works, the others exit at once.
rank=${OMPI_COMM_WORLD_RANK:-${PMI_RANK:-${PMIX_RANK:-${SLURM_PROCID:-0}}}}
if [ "$rank" != 0 ]; then
    exit 0
fi

path=$1 start_date=$2 end_date=$3
monthly_pattern=$4 monthly_ml_files=$5
daily_pattern=$6 daily_ml_files=$7
shift 7

script="$(dirname "$(readlink -f "$0")")/interp_to_pressure_levels.py"
status=0

run_step() {
    local label=$1 pattern=$2 ml_files=$3
    shift 3
    # remaining args: extra options of interp_to_pressure_levels.py
    if [ -z "$pattern" ]; then
        echo "=== ${label}: no pattern, skipped"
        return 0
    fi
    echo "=== ${label}: ${pattern} (--ml-files ${ml_files})"
    "$script" --path "$path" --pattern "$pattern" \
        --start-date "$start_date" --end-date "$end_date" \
        --ml-files "$ml_files" "$@"
    local rc=$?
    echo "=== ${label}: exit status ${rc}"
    return $rc
}

run_step monthly "$monthly_pattern" "$monthly_ml_files" --missing-ok "$@" || status=1
run_step daily "$daily_pattern" "$daily_ml_files" "$@" || status=1
exit $status
