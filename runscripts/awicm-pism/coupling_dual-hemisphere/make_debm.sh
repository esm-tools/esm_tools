#!/bin/bash

module purge
module load python3 #anaconda3/bleeding_edge
module load gcc/7.1.0
module load cdo nco netcdf_c netcdf-fortran
module load cmake #/3.13.3
#module load intel/18.0.4 intelmpi/2018.5.288 inteltools/2018

module unload intelmpi
module load intel #/18.0.4 
module load intelmpi #/2017.3.196 
module load autoconf #/2.69
export MPIFC=mpiifort F77=mpiifort FC=mpiifort CC=mpiicc CXX=mpiicpc
export MPIROOT=$($MPIFC -show | perl -lne 'm{ -I(.*?)/include } and print $1')

#module load intel openmpi/1.8.4-intel14
#module load intel intelmpi

# No. of cores for make
N=2

BASE_DIR="/pf/a/a270124/pism_build_sebastian02"

PNETCDF_ROOT="${BASE_DIR}/local/modules/pnetcdf"
PNETCDF_ROOT="/sw/rhel6-x64/netcdf/parallel_netcdf-1.10.0-openmpi2-intel14"
#PNETCDF_VER="1.8.1"
#PNETCDF_VER="1.12.0"
PNETCDF_VER="1.10.0"
PNETCDF_INCLUDES="${PNETCDF_ROOT}/include"
PNETCDF_LIBRARIES="${PNETCDF_ROOT}/lib"

#HDF5_ROOT="${BASE_DIR}/local/modules/hdf5"
HDF5_ROOT="/sw/rhel6-x64/hdf5/hdf5-1.8.21-gcc64"
#HDF5_VER="1.8.15"
HDF5_VER="1.8.21"
#HDF5_VER="1.10.5"
HDF5_INCLUDES="${HDF5_ROOT}/include"
HDF5_LIBRARIES="${HDF5_ROOT}/lib"

#NETCDF_ROOT="${BASE_DIR}/local/modules/netcdf"
NETCDF_ROOT="/sw/rhel6-x64/netcdf/netcdf_c-4.7.2-gcc64"
#NETCDF_VER="4.4.1.1"
#NETCDF_VER="4.7.3"
NETCDF_VER="4.7.2"
NETCDF_INCLUDES="${NETCDF_ROOT}/include"
NETCDF_LIBRARIES="${NETCDF_ROOT}/lib"

cd Fortran-v1.0

compiler=$(nc-config --fc)
include=$(nc-config --fflags)
libs=$(nc-config --flibs)

## For ollie, a bad hack: the library -lsz and -lcurl don't
## appear to be found on the compute nodes, we throw them out:
#bad_libs="-lsz -lcurl"
#for bad_lib in ${bad_libs}; do
#        libs="${libs//$bad_lib/}"
#done
echo -e "\t\t- INFO: compiler=$compiler"
echo -e "\t\t- INFO: include=$include"
echo -e "\t\t- INFO: libs=$libs"
echo -e "\t\t- INFO: the compile command will be:"
echo "$compiler $include $libs MOD_PRE.f90 MOD_DATA.f90 MOD_MAIN.f90 MOD_OUTPUT.f90 dEBMmain.f90 -o dEBMmain"
$compiler \
        $include \
        $libs \
        MOD_PRE.f90 \
        MOD_DATA.f90 \
        MOD_MAIN.f90 \
        MOD_OUTPUT.f90 \
        dEBMmain.f90 \
        -o dEBMmain \
        2>> _stdout_stderr
#make comp
#make run
cd ..
