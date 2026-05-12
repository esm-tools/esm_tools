#!/bin/bash
# Build remap_restart.
#
# Sources FESOM's own machine env (env/<machine>/shell) so the same compiler
# + netcdf modules used to build FESOM itself are used here. The Makefile
# honours $FC and $FFLAGS, and FESOM's env exports FC=mpif90, which gives
# Intel ifort + netcdf-{c,fortran}-openmpi-intel on Levante — same stack as
# the rest of the cavity-coupling pipeline.
#
# Override the FESOM checkout location with FESOM_DIR=/path/to/fesom-2.x .
# When invoked from esm_master the env is already configured and the `source`
# is a no-op replay.

set -e
HERE="$(cd "$(dirname "$0")" && pwd)"

# Locate a FESOM checkout to source its arch env from.
FESOM_DIR="${FESOM_DIR:-}"
if [ -z "${FESOM_DIR}" ]; then
    for candidate in \
        /work/ab0246/a270092/model_codes/awiesm3-develop/fesom-2.7 \
        /work/ab0246/a270092/model_codes/awiesm3-develop/fesom-2.6 \
        /work/ab0246/a270092/model_codes/awiesm3-develop/fesom-2.5
    do
        [ -d "${candidate}/env" ] && { FESOM_DIR="${candidate}"; break; }
    done
fi

# Map the host to a FESOM env subdir.
case "$(hostname -f 2>/dev/null || hostname)" in
    *levante*|*lvt*) FESOM_HOST=levante.dkrz.de ;;
    *)               FESOM_HOST="" ;;
esac

if [ -n "${FESOM_DIR}" ] && [ -n "${FESOM_HOST}" ] && [ -f "${FESOM_DIR}/env/${FESOM_HOST}/shell" ]; then
    echo "[build.sh] sourcing ${FESOM_DIR}/env/${FESOM_HOST}/shell"
    # shellcheck disable=SC1090
    source "${FESOM_DIR}/env/${FESOM_HOST}/shell"
else
    echo "[build.sh] no FESOM env found (FESOM_DIR=${FESOM_DIR}, host=${FESOM_HOST}); using current env"
fi

cd "${HERE}"
make "$@"
