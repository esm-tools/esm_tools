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

source $indir/../../scripts/env.sh

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




