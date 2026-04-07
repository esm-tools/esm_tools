#!/usr/bin/env bash
#SBATCH --job-name=esm-scan
#SBATCH --partition=mpp
#SBATCH --cpus-per-task=128
#SBATCH --mem=0
#SBATCH --time=08:00:00
#SBATCH --output=scan-%A_%a.log
#
# Scan ESM experiments into per-experiment DuckDB catalogs.
#
# Each SLURM array task reads one line from a CSV file and scans
# that experiment using all available cores on the node.
#
# Hardware (albedo prod nodes):
#   2x sockets, 64 cores each, 128 CPUs, ~252GB RAM
#   --mem=0 requests all memory on the node.
#
# Usage:
#   # Count non-comment lines to set array range:
#   N=$(grep -cv '^#' experiments.csv)
#   sbatch --array=1-${N} scan_experiments.slurm experiments.csv /output/dir
#
#   # Or scan a single experiment (no CSV):
#   sbatch scan_experiments.slurm /path/to/experiment /output/dir
#
# Output:
#   <output_dir>/<experiment_name>.duckdb
#
# After all tasks complete, federate the catalogs:
#   esm-catalog serve --catalog /output/dir/mis31.duckdb \
#                      --catalog /output/dir/eoi400.duckdb
set -euo pipefail

# ── Arguments ─────────────────────────────────────────────────
INPUT="${1:?Usage: sbatch scan_experiments.slurm <experiments.csv|experiment_path> <output_dir>}"
OUTPUT_DIR="${2:?Usage: sbatch scan_experiments.slurm <experiments.csv|experiment_path> <output_dir>}"

# ── Resolve experiment path ───────────────────────────────────
if [[ -f "${INPUT}" && "${INPUT}" == *.csv ]]; then
    # CSV mode: read line ${SLURM_ARRAY_TASK_ID} (skip comments)
    TASK_ID="${SLURM_ARRAY_TASK_ID:?CSV mode requires --array}"
    EXP_PATH=$(grep -v '^#' "${INPUT}" | sed -n "${TASK_ID}p" | cut -d',' -f1 | xargs)
    if [[ -z "${EXP_PATH}" ]]; then
        echo "ERROR: No experiment at line ${TASK_ID} in ${INPUT}"
        exit 1
    fi
else
    # Direct mode: INPUT is the experiment path itself
    EXP_PATH="${INPUT}"
fi

# ── Derive output DB name from experiment directory ───────────
EXP_NAME=$(basename "${EXP_PATH}")
DB_PATH="${OUTPUT_DIR}/${EXP_NAME}.duckdb"

mkdir -p "${OUTPUT_DIR}"

# ── Workers = available cores (leave 2 for main + OS) ─────────
WORKERS=$(( ${SLURM_CPUS_PER_TASK:-128} - 2 ))
if [[ ${WORKERS} -lt 1 ]]; then
    WORKERS=1
fi

echo "============================================"
echo "  Experiment: ${EXP_PATH}"
echo "  Output DB:  ${DB_PATH}"
echo "  Workers:    ${WORKERS}"
echo "  Node:       $(hostname)"
echo "  CPUs:       ${SLURM_CPUS_PER_TASK:-?}"
echo "  Memory:     ${SLURM_MEM_PER_NODE:-all}MB"
echo "  Job ID:     ${SLURM_JOB_ID:-local}"
echo "  Array task: ${SLURM_ARRAY_TASK_ID:-N/A}"
echo "============================================"

# ── Run scan ──────────────────────────────────────────────────
# ProcessPoolExecutor spawns 126 worker processes, each opening
# files with xarray independently. For 393k files this means
# ~3100 files per worker -- should finish well within walltime.
#
# Add --distributed for Dask LocalCluster (marginally more
# overhead but gives you the dashboard at http://node:8787).
esm-catalog scan "${EXP_PATH}" \
    --db "${DB_PATH}" \
    -j "${WORKERS}" \
    -v
