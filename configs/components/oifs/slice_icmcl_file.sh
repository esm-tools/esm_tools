#!/bin/bash

## 
## Usage: ./oifs_change_dataDate_initfiles.sh MACHINE INDIR EXPID YYYY-MM-DDTHH:MM:SS YYYY-MM-DDTHH:MM:SS OUTDIR
## where EXPID is the 4-character experiment ID, e.g. gu5a if the filename is ICMGGgu5aINIT etc.
## YYYYMMDD is the new date you want to set in the grib files
## DIR is the location of the original data. 
##

indir=$1
icmcl_file=$2
outexpid=$3
startdate=$4
enddate=$5
outdir=$6

if [[ "$(hostname -f)" =~ ollie ]] ; then
    module purge
    module load intel.compiler
    module load cdo netcdf/4.6.2_intel
    export PATH=/home/ollie/jstreffi/ecmwf/grib_api_intel_hdf5_1.10.2_gnu/bin:$PATH

elif [[ "$(hostname -f)" =~ dkrz.de ]] ; then
    export PATH=/sw/rhel6-x64/grib_api/grib_api-1.15.0-intel14/bin:$PATH
    module purge
    module load netcdf_c/4.3.2-gcc48
    module load cdo

elif [[ "$(hostname -f)" =~ hlrn.de ]] ; then
    module load cdo
    module load intel/19.0.5 impi/2019.5
elif [[ "$(hostname -f)" =~ juwels ]] ; then
	 
    # new Intel 2019 settings 
    # self compiled netcdf etc from Sebastian Wahl
    module --force purge
    module use $OTHERSTAGES
    module load Stages/Devel-2019a
    #module load Intel/2019.3.199-GCC-8.3.0
    #module load IntelMPI/2019.6.154
    module load Intel/2019.5.281-GCC-8.3.0
    module load ParaStationMPI/5.4.4-1-mt    
    module load Python/3.6.8
    module load imkl/2019.3.199
    #export IO_LIB_ROOT=/p/project/hirace/HPC_libraries/intel2019.3.199_impi2019.6.154_20200703/
    export IO_LIB_ROOT=/p/project/hirace/HPC_libraries/intel2019.5.281_parastation_5.4.4-1-mt_20201113/
    export PATH=$IO_LIB_ROOT/bin:$PATH
    export LD_LIBRARY_PATH=$IO_LIB_ROOT/lib:$LD_LIBRARY_PATH
else
   echo
	echo $0 has not been adapted for $(hostname)
	echo
	exit 1
fi 

echo " OpenIFS preprocessing "
echo " ===================== "
echo " "
echo " Machine: $(hostname) "
echo " grib_set: $(which grib_set) "
echo " cdo -V: "
cdo -V 
echo " "
echo " Input dir: $indir "
echo " Output dir: $outdir "
echo " Exp ID: $expid "
echo " Start date: $startdate "
echo " End date: $enddate "

starttime=$(date -u -d "${startdate}" +%Y-%m-%dT%T)
endtime=$(date -u -d "${enddate}" +%Y-%m-%dT%T)

echo " Start time: $starttime "
echo " End time: $endtime "
echo " "

echo " * Cut ICMCL file "
cdo -select,startdate=${starttime},enddate=${endtime} ${icmcl_file} ${outdir}/ICMCL${outexpid}INIT




