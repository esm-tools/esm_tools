#!/usr/bin/env bash
# Start a Dask scheduler on a login/interactive node.
#
# Usage:
#   ./start_dask_scheduler.sh
#
# The scheduler listens on tcp://<hostname>:8786 and serves
# a dashboard at http://<hostname>:8787.
#
# After starting, submit workers via:
#   sbatch submit_dask_workers.sh
#
# Then run the scan:
#   esm-catalog scan /path/to/experiment \
#       --db catalog.duckdb \
#       --scheduler tcp://$(hostname):8786
set -euo pipefail

SCHEDULER_PORT="${DASK_SCHEDULER_PORT:-8786}"
DASHBOARD_PORT="${DASK_DASHBOARD_PORT:-8787}"

echo "Starting Dask scheduler on $(hostname):${SCHEDULER_PORT}"
echo "Dashboard: http://$(hostname):${DASHBOARD_PORT}"
echo ""
echo "To connect workers:"
echo "  sbatch submit_dask_workers.sh"
echo ""
echo "To scan:"
echo "  esm-catalog scan /path/to/experiment \\"
echo "      --db catalog.duckdb \\"
echo "      --scheduler tcp://$(hostname):${SCHEDULER_PORT}"
echo ""

exec dask scheduler \
    --port "${SCHEDULER_PORT}" \
    --dashboard-address ":${DASHBOARD_PORT}"
