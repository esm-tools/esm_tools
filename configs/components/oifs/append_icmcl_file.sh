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

startyear=$(date -u -d "${startdate}" +%Y)
endyear=$(date -u -d "${enddate}" +%Y)

echo " Start year: $startyear "
echo " End year: $endyear "
echo " "

echo " * Cut orinal ICMCL file into months"
for ((m = 1 ; m < 13 ; m++)); do
    cdo seltimestep,$m ${icmcl_file} $outdir/ICMCL_$m &
done
wait

echo " *Create ICMCL files for all months & years needed for run"
for ((y = $startyear ; y < $(($endyear+1)) ; y++)); do
    for ((m = 1 ; m < 13 ; m++)); do
        grib_set -s dataDate=$y$(printf "%02g" $m)15 $outdir/ICMCL_$m $outdir/ICMCL_${y}_$m &
    done
done
wait

echo " *Clean up monthly ICMCL files with original year"
for ((m = 1 ; m < 13 ; m++)); do
    rm $outdir/ICMCL_$m &
done
wait

echo " *Write list of all ICMCL files"
filelist=" "
for ((y = $startyear ; y < $(($endyear+1)) ; y++)); do
    for ((m = 1 ; m < 13 ; m++)); do
        export filelist="$filelist $outdir/ICMCL_${y}_$m"
    done
done

echo " *Merge monthly ICMCL files into single file"
rm -f ${outdir}/ICMCL${outexpid}INIT
cdo mergetime $filelist ${outdir}/ICMCL${outexpid}INIT

echo " *Cleanup monthly ICMCL files"
rm $filelist




