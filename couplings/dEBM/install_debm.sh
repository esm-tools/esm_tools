#!/bin/bash

MACHINE=$1

if [[ ${MACHINE} == "albedo" ]]; then
    module load nco/5.0.1 netcdf-fortran/4.5.4-intel2021.6.0
    module load intel-oneapi-compilers/2022.1.0
    fortran_compiler=ifort #gfortran
    nc_config=/albedo/soft/sw/spack-sw/netcdf-fortran/4.5.4-yb7woqz/bin/nf-config
    export NETCDF_LIB=$($nc_config --flibs)
    export NETCDF_INC=$($nc_config --includedir)
    NETCDFFROOT=/albedo/soft/sw/spack-sw/netcdf-fortran/4.5.4-yb7woqz
    export PATH=${NETCDFFROOT}:${NETCDFFROOT}/include:${NETCDFFROOT}/lib:$PATH
    export LD_LIBRARY_PATH=${NETCDFFROOT}/lib:${NETCDFROOT}/lib:$LD_LIBRARY_PATH
elif [[ ${MACHINE} == "levante" ]]; then
    module load nco/5.0.6-gcc-11.2.0 intel-oneapi-compilers/2022.0.1-gcc-11.2.0
    module load intel-oneapi-mkl/2022.0.1-gcc-11.2.0 openmpi/4.1.2-intel-2021.5.0 netcdf-fortran/4.5.3-openmpi-4.1.2-intel-2021.5.0
    nc_config=/sw/spack-levante/netcdf-fortran-4.5.3-pywf2l/bin/nf-config
    export NETCDF_LIB=$($nc_config --flibs)
    export NETCDF_INC=$($nc_config --includedir)
    NETCDFFROOT=/sw/spack-levante/netcdf-fortran-4.5.3-pywf2l/
    export PATH=${NETCDFFROOT}:${NETCDFFROOT}/include:${NETCDFFROOT}/lib:$PATH
    export LD_LIBRARY_PATH=${NETCDFFROOT}/lib:${NETCDFROOT}/lib:$LD_LIBRARY_PATH
else
    echo " * Unknown machine! No installation possible"
    exit
fi

thisdir=$(dirname $0)
thisdir=$(readlink -f $thisdir)
old_dir=$(pwd)

cd ${thisdir}/Fortran/

make
