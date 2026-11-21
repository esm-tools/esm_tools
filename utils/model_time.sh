#!/bin/bash

# Help flag
if [[ "$1" == "--help" || "$1" == "-h" ]]; then
    echo "Usage: $0 <simulation_pattern>"
    echo "Example: $0 'rundir/test_*'"
    exit 0
fi

SIM_PATTERN=$1
FILE_PATTERN="${SIM_PATTERN}/log/${SIM_PATTERN}_*_compute_????????-????????_*.log"

for file in $FILE_PATTERN; do
    # Find real time in the file
    REAL_TIME=$(grep -oP 'real\t.*' "$file" | head -n 1 | awk '{print $2}')
    # Extract SIM
    SIM=$(echo $file | grep -o '^[^/]*')
    # Extract date from the file name
    DATE=$(echo $file | grep -oP 'compute_\K[0-9]{8}-[0-9]{8}')
    echo $SIM $DATE $REAL_TIME
done
