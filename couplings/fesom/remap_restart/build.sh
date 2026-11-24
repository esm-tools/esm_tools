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
    # Newest checkout first: this list decides which compiler and netCDF the
    # tool is built with, and the run uses the newest one (fesom-2.8 here).
    for candidate in \
        /albedo/work/user/${USER}/model_code/awiesm3-develop-is/fesom-2.8 \
        /albedo/work/user/${USER}/model_code/awiesm3-develop-is/fesom-2.7 \
        /work/ab0246/a270092/model_codes/awiesm3-develop/fesom-2.8 \
        /work/ab0246/a270092/model_codes/awiesm3-develop-is/fesom-2.8 \
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
    *albedo*|prod-*|fat-*|gpu-*) FESOM_HOST=albedo ;;
    *)               FESOM_HOST="" ;;
esac

# Preserve the RUN environment's netCDF. esm_tools exports NETCDFFROOT/NETCDFROOT
# in the experiment's env.sh and prepends $NETCDFFROOT/lib to LD_LIBRARY_PATH,
# but the FESOM env sourced below module-loads its own netcdf-fortran, and that
# module file does `pushenv NETCDFFROOT`, i.e. it OVERWRITES the value. The tool
# then compiles against one netcdf-fortran (4.5.4 on albedo) and, since both have
# soname libnetcdff.so.7, silently loads the other at runtime (4.6.1) -- the
# nf90_* call sequence disagrees and it dies with a nil dereference inside
# netcdf, naming an intact restart file (antftest8/antftest9 cycle 2,
# 2026-10-07). So save them here and put them back after.
_RUN_NETCDFFROOT="${NETCDFFROOT:-}"
_RUN_NETCDFROOT="${NETCDFROOT:-}"

if [ -n "${FESOM_DIR}" ] && [ -n "${FESOM_HOST}" ] && [ -f "${FESOM_DIR}/env/${FESOM_HOST}/shell" ]; then
    echo "[build.sh] sourcing ${FESOM_DIR}/env/${FESOM_HOST}/shell"
    # shellcheck disable=SC1090
    source "${FESOM_DIR}/env/${FESOM_HOST}/shell"
else
    echo "[build.sh] no FESOM env found (FESOM_DIR=${FESOM_DIR}, host=${FESOM_HOST}); using current env"
fi

# Restore the run env's netCDF over whatever the FESOM env's module pushed, and
# prepend its lib dir so the LINK and the RUN resolve the same libnetcdff.
if [ -n "${_RUN_NETCDFFROOT}" ]; then
    if [ "${_RUN_NETCDFFROOT%/}" != "${NETCDFFROOT%/}" ]; then
        echo "[build.sh] run env netcdf-fortran ${_RUN_NETCDFFROOT} overrides the FESOM env's ${NETCDFFROOT}"
    fi
    export NETCDFFROOT="${_RUN_NETCDFFROOT}"
    export LD_LIBRARY_PATH="${NETCDFFROOT%/}/lib:${LD_LIBRARY_PATH:-}"
fi
[ -n "${_RUN_NETCDFROOT}" ] && export NETCDFROOT="${_RUN_NETCDFROOT}"

cd "${HERE}"
make "$@"
