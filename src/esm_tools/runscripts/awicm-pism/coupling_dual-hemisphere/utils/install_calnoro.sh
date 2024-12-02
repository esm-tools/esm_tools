#!/bin/bash
                
module load gcc/11.2.0-gcc-11.2.0 #gcc/6.4.0
fortran_compiler=gfortran
nc_config=/sw/spack-levante.2022-02-17/netcdf-fortran-4.5.3-jlxcfz/bin/nf-config #/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.4-gcc64/bin/nf-config
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
