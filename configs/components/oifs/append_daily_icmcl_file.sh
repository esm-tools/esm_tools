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
pooldir=$7

source $indir/../../scripts/env.sh

echo " OpenIFS preprocessing "
echo " ===================== "
echo " "
echo " Machine: $(hostname) "
echo " grib_set: $(which grib_set) "
echo " "
echo " Working dir $(pwd)"
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

mkdir -p $outdir/icmcl/
mkdir -p $outdir/icmcl/out/

echo " *Using grib_ls to extract the dataDates of original ICMCL file"
echo "grib_copy -w shortName=lai_lv ${icmcl_file} $outdir/icmcl/ICMCL_for_dataDate_list"
grib_copy -w shortName=lai_lv ${icmcl_file} $outdir/icmcl/ICMCL_for_dataDate_list
dataDate_list=$(grib_ls -p dataDate $outdir/icmcl/ICMCL_for_dataDate_list | tail -n +3 | head -n -3)

echo " *Checking if original ICMCL file has already been cut"
exists=1
for dataDate in $dataDate_list; do
    if [ ! -e "$pooldir/icmcl/ICMCL_$dataDate" ]; then
        exists=0
    fi
done

if [ "$exists" -eq 0 ]; then
    echo " *Cutting original ICMCL file into daily files"
    for dataDate in $dataDate_list; do
        echo "grib_copy -w dataDate=$dataDate ${icmcl_file} $outdir/icmcl/ICMCL_$dataDate"
        grib_copy -w dataDate=$dataDate ${icmcl_file} $outdir/icmcl/ICMCL_$dataDate &
    done
    wait
else
    echo " *Orinal ICMCL file is already cut into daily files; linking"
    for dataDate in $dataDate_list; do
        ln -s $pooldir/icmcl/ICMCL_$dataDate $outdir/icmcl/ &
    done
    wait
fi

orig_year=${dataDate:0:4}

echo " *Creating ICMCL files for all days, months & years needed for run"
filelist=" "
for ((y = $startyear ; y < $(($endyear+1)) ; y++)); do
    # Figure out if leap year
    if [ $((y % 4)) -eq 0 ] && [ $((y % 100)) -ne 0 ] || [ $((y % 400)) -eq 0 ]; then
      leapyear=1
    else
      leapyear=0
    fi
    for ((m = 1 ; m < 13 ; m++)); do 
        # Set number of days per month
        if [ "$m" -eq 4 ] || [ "$m" -eq 6 ] || [ "$m" -eq 9 ] || [ "$m" -eq 11 ]; then 
            day_per_month=30
        elif [ "$m" -eq 1 ] || [ "$m" -eq 3 ] || [ "$m" -eq 5 ] || [ "$m" -eq 7 ] || [ "$m" -eq 8 ] || [ "$m" -eq 10 ] || [ "$m" -eq 12 ]; then
            day_per_month=31
        elif [ "$m" -eq 2 ]; then
            if [ "$leapyear" -eq 1 ]; then
                day_per_month=29
            else
                day_per_month=28
            fi
        fi
        # Create files with correct dates
        for ((d = 1 ; d < $((day_per_month+1)) ; d++)); do
            grib_set -s dataDate=$(printf "%04d" $y)$(printf "%02d" $m)$(printf "%02d" $d) $outdir/icmcl/ICMCL_$orig_year$(printf "%02d" $m)$(printf "%02d" $d) $outdir/icmcl/out/ICMCL_$(printf "%04d" $y)$(printf "%02d" $m)$(printf "%02d" $d) &
            export filelist="$filelist $outdir/icmcl/out/ICMCL_$(printf "%04d" $y)$(printf "%02d" $m)$(printf "%02d" $d)"
        done
    done
done
wait

echo " *Mergeing daily ICMCL files into single file"
rm -f ${outdir}/ICMCL${outexpid}INIT
grib_copy $filelist ${outdir}/ICMCL${outexpid}INIT

if [ "$exists" -eq 0 ]; then
    if [ -w "$pooldir" ]; then
        echo " *Storing original ICMCL cut into daily files to $pooldir/icmcl"
        mkdir -p $pooldir/icmcl
        for dataDate in $dataDate_list; do
            mv $outdir/icmcl/ICMCL_$dataDate $pooldir/icmcl/ &
        done
        wait
    fi
fi
echo " *Cleaning up the temporary folder $outdir/icmcl"
rm -rf $outdir/icmcl &

