#!/bin/bash -l

module load gcc/11.2.0-gcc-11.2.0 #gcc/6.4.0
fortran_compiler=gfortran
nc_config=/sw/spack-levante.2022-02-17/netcdf-fortran-4.5.3-jlxcfz/bin/nf-config #/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.4-gcc64/bin/nf-config
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
