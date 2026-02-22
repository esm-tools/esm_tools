#!/bin/bash

##This script prepares medusa output for the next fesom run

indir=$1 #medusa output directory
outid=$2 #fesom input directory
enddate=$3 #date of the last fesom year
cfreq=$4 # model years of medusa run

#1. check if running successful
lastline=`tail -2 $indir/medusa.dbg`
testline=`echo $lastline | grep ': error:'`
if [ "$testline" ]
then
   echo "something in MEDUSA went wrong"
   echo $lastline
   exit
else
   echo "MEDUSA normal end"

#2. create medusa pickups for the next run (done through esm_tools?)
cp $indir/medusa_flx.$enddate.nc $outid/medusa_flx_restart.nc

# create medusa.tsi for the next run
awk '{if($3=="atime") printf  "    %i %s %s\n", $1+$cfreq, $2, $3; else print $0}' $indir/medusa.tsi > $indir/medusa.tsi.tmp
mv $indir/medusa.tsi $indir/medusa.tsi.old
mv $indir/medusa.tsi.tmp $indir/medusa.tsi

# cleaning up and saving medusa output
ls medusa*.nc | grep -v 'restart' | xargs mv -t $SAVEDIR2
#mv medusa*.nc $SAVEDIR2
cp medusa.dbg ./medusa.dbg.$year
cp medusa.err ./medusa.err.$year
cp medusa.log ./medusa.log.$year
cp medusa-convergence.log ./medusa-convergence.log.$year
rm *.fesom.ave.*.nc

# check if the coupled run is completed
max_year=4500  # last year of simulations
if [ "$year" -ge "$max_year" ]; then
  echo "Last year finished. Terminate and exit."
  exit
else
  echo "the current coupled fesom-recom-medusa is done!"
# prepare for the next fesom run
  cp $SAVEDIR/restart_files/fesom.$year.*.restart.nc .
  cp $SAVEDIR2/medusa_flux2fesom.$year.nc ./medusa_flux2fesom.nc
# submit the next job
  cd $JOBDIR
  sbatch job_fesom_gmd_coupled_lb_new
fi

fi
