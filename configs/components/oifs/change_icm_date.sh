#!/bin/bash

## 
## Usage: ./oifs_change_dataDate_initfiles.sh MACHINE INDIR EXPID YYYY-MM-DDTHH:MM:SS YYYY-MM-DDTHH:MM:SS OUTDIR
## where EXPID is the 4-character experiment ID, e.g. gu5a if the filename is ICMGGgu5aINIT etc.
## YYYYMMDD is the new date you want to set in the grib files
## DIR is the location of the original data. 
##

indir=$1
inexpid=$2
outexpid=$3
inidate=$4
startdate=$5
with_wam=$6
perturb=$7
nx=$8
ensemble_id=$9

outdir=$indir

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
echo " InExp ID: $inexpid "
echo " OutExp ID: $outexpid "
echo " Start date: $startdate "
echo " End date: $enddate "
echo " Perturb: $perturb "
ndate=$(date -u -d "${inidate}" +%Y%m%d)
initime=$(date -u -d "${inidate}" +%Y-%m-%dT%T)
starttime=$(date -u -d "${startdate}" +%Y-%m-%dT%T)
endtime=$(date -u -d "${enddate}" +%Y-%m-%dT%T)
echo " New date: $ndate "
echo " Initial time: $initime "
echo " Start time: $starttime "
echo " End time: $endtime "
echo " "

echo " * Change dataDate in files: "
echo " $files "

old=${indir}/ICMGG${inexpid}INIT
new=${outdir}/ICMGG${outexpid}INIT
newgginit=${new}
if [ -f $old ]; then
    cp $old $old\_ori
    rm $old
    grib_set -s dataDate=$ndate $old\_ori $new
    echo " Made new file: " $new " with date " $ndate
else
    echo " Could not find file " $old
    exit
fi

old=${indir}/ICMGG${inexpid}INIUA
new=${outdir}/ICMGG${outexpid}INIUA
if [ -f $old ]; then
    cp $old $old\_ori
    rm $old
    grib_set -s dataDate=$ndate $old\_ori $new
    echo " Made new file: " $new " with date " $ndate
else
    echo " Could not find file " $old
    exit
fi

# Modify SKT (skin-temp) field to create ensemble members
if [[ "x${perturb}" == "x1" ]] ; then
    
    echo "          Modifying OpenIFS inital field SKT to generate ensemble member No ${ensemble_id}" 
    
    rm -rf tmp.grb tmp2.grb
    # Generate random numbers on a 1D array of same size as reduced grid
    echo "cdo -f nc -remapnn,r${nx}x1 -divc,10 -subc,0.5 -random,r${nx}x1,${ensemble_id} ${outdir}/random_1d.nc"
    cdo -O -f nc -remapnn,r${nx}x1 -divc,10 -subc,0.5 -random,r${nx}x1,${ensemble_id} ${outdir}/random_1d.nc
    # Now take grid description from ICMGG 
    echo "cdo -setgrid,${newgginit} ${outdir}/random_1d.nc ${outdir}/random.nc "
    cdo -O -f grb -setgrid,${newgginit} ${outdir}/random_1d.nc ${outdir}/random.grb
    # Merge with ICMGG
    echo "cdo merge ${outdir}/ICMGG${outexpid}INIT ${outdir}/random.grb ${outdir}/tmp.grb"
    cdo -O merge ${outdir}/ICMGG${outexpid}INIT ${outdir}/random.grb ${outdir}/tmp.grb
    # Add to SKT (var 235)
    echo "cdo expr,'var235=var1+var235;' ${outdir}/tmp.grb ${outdir}/tmp2.grb "
    cdo -O expr,'var235=var1+var235;' ${outdir}/tmp.grb ${outdir}/tmp2.grb
    # Replace values
    echo "cdo replace ${outdir}/ICMGG${outexpid}INIT ${outdir}/tmp2.grb ${outdir}/ICMGG${outexpid}INIT_new"
    cdo -O replace ${outdir}/ICMGG${outexpid}INIT ${outdir}/tmp2.grb ${outdir}/ICMGG${outexpid}INIT_new
    # Control: Save diffs
    cdo -O sub ${outdir}/ICMGG${outexpid}INIT_new ${outdir}/ICMGG${outexpid}INIT ${outdir}/ICMGG${outexpid}INIT_sub
    # Replace file
    echo "mv ${outdir}/ICMGG${outexpid}INIT_new ${outdir}/ICMGG${outexpid}INIT"
    mv ${outdir}/ICMGG${outexpid}INIT_new ${outdir}/ICMGG${outexpid}INIT
    # Control: Make nc file    
    cdo -O -f nc -t ecmwf -setgridtype,regular ${outdir}/ICMGG${outexpid}INIT_sub ${outdir}/ICMGG${outexpid}INIT_sub.nc 
    # Clean up crew
    echo "          Modification of OpenIFS initial files for ensemble complete"
    
    
fi

old=${indir}/ICMSH${inexpid}INIT
new=${outdir}/ICMSH${outexpid}INIT
if [ -f $old ]; then
    cp $old $old\_ori
    rm $old  
    grib_set -s dataDate=$ndate $old\_ori $new
    echo " Made new file: " $new " with date " $ndate
else
    echo " Could not find file " $old
    exit
fi

if [[ "x${with_wam}" == "x1" ]] ; then
    files="cdwavein sfcwindin specwavein uwavein"
    for file in $files ; do

        ## old file
        old=${indir}/$file 
        
        ## new file
        new=${outdir}/$file

        if [ -f $old ]; then
            cp $old $old\_ori
            rm $old 
            ## use grib_set to make new files
            grib_set -s dataDate=$ndate $old\_ori $new
            echo " Made new file: " $new " with date " $ndate
        else
            echo " Could not find file " $old
            exit
        fi
    done
fi

