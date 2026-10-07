#!/bin/bash
# Workflow jobs of the iceberg budget (awiesm3 general.icb_from_budget); all arguments go to
# iceberg_budget.py.
#   seed    before prepcompute. Needs numpy only and must not be skipped: without the seed files the
#           leg has no icebergs to read, so a missing python is an error.
#   budget  after tidy. Needs xarray; if that is missing it is left to the next leg's seed step,
#           which catches up.
here=$(cd "$(dirname "$0")" && pwd)
py=${ICB_PYTHON:-python3}
if [ "$1" = "budget" ] && ! "$py" -c "import numpy, xarray" 2>/dev/null; then
    echo " *   iceberg budget: $py has no numpy/xarray; left to the next leg's preparation"
    exit 0
fi
exec "$py" -W ignore "$here/iceberg_budget.py" "$@"
