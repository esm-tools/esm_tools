#!/bin/bash -l

module load gcc/12.1.0
fortran_compiler=gfortran
nc_config=/albedo/soft/sw/spack-sw/netcdf-fortran/4.5.4-yb7woqz/bin/nf-config
NETCDF_LIB=$($nc_config --flibs)
NETCDF_INCLUDE=-I$($nc_config --includedir)

echo "Done loading environment!"

thisdir=$(dirname $0)
thisdir=$(readlink -f $thisdir)
old_dir=$(pwd)

prog=jsbach_init_file
srcdir=${thisdir}
rm -rf ${srcdir}/_build/${prog}
mkdir -p ${thisdir}/_build/

F90=$fortran_compiler 

echo "Compile ${srcdir}/${prog}.f90..."
echo ${F90} ${F90FLAGS} -o ${srcdir}/_build/${prog} ${srcdir}/${prog}.f90 ${NETCDF_INCLUDE} ${NETCDF_LIBDIR} ${NETCDF_LIB} 
${F90} ${F90FLAGS} -o ${srcdir}/_build/${prog} ${srcdir}/${prog}.f90 ${NETCDF_INCLUDE} ${NETCDF_LIBDIR} ${NETCDF_LIB} 
mv mo_vegparams.mod mo_kinds.mod ${srcdir}/_build  
mv _build/jsbach_init_file jsbach_init_file
