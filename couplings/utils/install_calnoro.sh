#!/bin/bash
                
module load gcc/12.1.0
fortran_compiler=gfortran
nc_config=/albedo/soft/sw/spack-sw/netcdf-fortran/4.5.4-yb7woqz/bin/nf-config
NETCDF_LIB=$($nc_config --flibs)
NETCDF_INCLUDE=-I$($nc_config --includedir)

thisdir=$(dirname $0)
thisdir=$(readlink -f $thisdir)
old_dir=$(pwd)

rm -rf ${thisdir}/_build/calnoro
mkdir -p ${thisdir}/_build/



echo $fortran_compiler \
        ${thisdir}/calnoro.f90 \
        ${thisdir}/grid_noro.f90 \
        -o ${thisdir}/_build/calnoro \
        $NETCDF_LIB $NETCDF_INCLUDE 
$fortran_compiler \
        ${thisdir}/calnoro.f90 \
        ${thisdir}/grid_noro.f90 \
        -o ${thisdir}/_build/calnoro \
        $NETCDF_LIB $NETCDF_INCLUDE 

mv _build/calnoro calnoro
