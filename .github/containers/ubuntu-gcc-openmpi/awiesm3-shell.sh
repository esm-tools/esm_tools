#!/bin/bash
#
# Open a shell in the AWI-ESM3 container with Slurm running.
#
# Everything lives side by side in one base folder (override with AWIESM3_BASE).
# By default that is the folder containing the esm_tools checkout this script is
# in, or, if the script was copied out of esm_tools, the folder containing its
# own directory:
#   esm_tools checkout   -> /home/esm/esm_tools   ($BASE/esm_tools if copied out)
#   $BASE/model_codes    -> /home/esm/model_codes
#   $BASE/ocp-tool       -> /home/esm/ocp-tool    (only if it exists)
#   $BASE/awiesm3_pool   -> /pool                 (input data; AWIESM3_POOL)
#   $BASE/awiesm3_work   -> /work                 (experiments; AWIESM3_WORK)
# The esm_tools venv is kept on the Docker volume awiesm3-venv (/opt/venv).
#
# --privileged is needed for Slurm's cgroup setup inside the container.

HERE="$(cd "$(dirname "$0")" && pwd)"
if [ -f "$HERE/../../../setup.py" ] && [ -d "$HERE/../../../configs/machines" ]; then
    ESM_TOOLS_DIR="$(cd "$HERE/../../.." && pwd)"          # inside an esm_tools checkout
    BASE="${AWIESM3_BASE:-$(cd "$ESM_TOOLS_DIR/.." && pwd)}"
else
    BASE="${AWIESM3_BASE:-$(cd "$HERE/.." && pwd)}"
    ESM_TOOLS_DIR="$BASE/esm_tools"
fi
IMAGE="${AWIESM3_IMAGE:-awiesm3-env:dev}"
POOL="${AWIESM3_POOL:-$BASE/awiesm3_pool}"
WORK="${AWIESM3_WORK:-$BASE/awiesm3_work}"
mkdir -p "$POOL" "$WORK" "$BASE/model_codes"

if [ ! -f "$ESM_TOOLS_DIR/setup.py" ]; then
    echo "No esm_tools checkout at $ESM_TOOLS_DIR (see README.md)" >&2
    exit 1
fi

OCP_MOUNT=()
if [ -d "$BASE/ocp-tool" ]; then
    OCP_MOUNT=(-v "$BASE/ocp-tool:/home/esm/ocp-tool")
fi

exec docker run --rm -it --privileged \
    --hostname awiesm3 \
    -v "$ESM_TOOLS_DIR:/home/esm/esm_tools" \
    -v "$BASE/model_codes:/home/esm/model_codes" \
    "${OCP_MOUNT[@]}" \
    -v "$POOL:/pool" \
    -v "$WORK:/work" \
    -v "${AWIESM3_VENV_VOLUME:-awiesm3-venv}:/opt/venv" \
    "$IMAGE" \
    bash -c 'start-slurm.sh && setup-esm-tools.sh && exec bash'
