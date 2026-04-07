#!/usr/bin/env bash
#SBATCH --job-name=dask-worker
#SBATCH --partition=mpp
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=128
#SBATCH --mem=0
#SBATCH --time=04:00:00
#SBATCH --output=dask-worker-%j.log
#
# Submit Dask workers that connect to an existing scheduler.
#
# Usage:
#   # Start scheduler first (on login node):
#   ./start_dask_scheduler.sh &
#
#   # Submit workers (scale by adjusting --array):
#   DASK_SCHEDULER=tcp://login-node:8786 sbatch --array=1-4 submit_dask_workers.sh
#
# Each array task spawns one multi-threaded Dask worker process.
# With --array=1-4 and --cpus-per-task=16, you get 4 workers with
# 16 threads each = 64 cores total.
#
# Adjust to your cluster:
#   --partition:      your SLURM partition name
#   --cpus-per-task:  cores per worker
#   --mem:            memory per worker
#   --time:           walltime (should exceed expected scan time)
set -euo pipefail

# Scheduler address -- set via environment or default
SCHEDULER="${DASK_SCHEDULER:-tcp://$(hostname):8786}"
NTHREADS="${SLURM_CPUS_PER_TASK:-16}"
MEMORY_LIMIT="${SLURM_MEM_PER_NODE:-64000}MB"

echo "Dask worker starting on $(hostname)"
echo "  Scheduler:    ${SCHEDULER}"
echo "  Threads:      ${NTHREADS}"
echo "  Memory limit: ${MEMORY_LIMIT}"
echo "  SLURM job:    ${SLURM_JOB_ID:-local}"

# Use /tmp for worker scratch space (fast local SSD on most clusters)
WORKER_DIR="/tmp/dask-worker-${SLURM_JOB_ID:-$$}"
mkdir -p "${WORKER_DIR}"

exec dask worker "${SCHEDULER}" \
    --nworkers 1 \
    --nthreads "${NTHREADS}" \
    --memory-limit "${MEMORY_LIMIT}" \
    --local-directory "${WORKER_DIR}" \
    --name "slurm-${SLURM_ARRAY_JOB_ID:-0}-${SLURM_ARRAY_TASK_ID:-0}" \
    --death-timeout 120
