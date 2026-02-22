#!/bin/bash

##This script prepares fesom output of concentrations of solutes
##(DIC, Alk, DIN, DSi, O2, DIC_13/14) and sinking fluxes of particles
##(POC, PON, calcite, opal and C isotopes in calcite and POC)

# define directories
indir=$1     
#fesom output: /work/ollie/yye/AWIESM_runs/PI_echamcold_nociso/outdata/fesom
outdir=$2     
#save postprocessed files: experiment_dir/input/medusa?
startdate=$3 #date of the first fesom file to be postprocessed
enddate=$4   #date of the last fesom file: same as the current fesom year

#source .../scripts/env.sh

echo " MEDUSA preprocessing "
echo " ===================== "
echo " "
echo " Machine: $(hostname) "
echo " "
echo " Input dir: $indir "
echo " Output dir: $outdir "
#echo " InExp ID: $inexpid "
#echo " OutExp ID: $outexpid "
echo " averaged time period of fesom output: ${startdate} - ${enddate} "
echo " averaging fesom output: "

trac4medusa=('temp' 'salt' 'DIC' 'Alk' 'DIN' 'DSi' 'O2' 'DIC_13' 'DIC_14')
for i in {0..8}
  do
     cdo ensmean ${indir}/${trac4medusa[i]}.fesom.[$startdate..$enddate]*.nc ${outdir}/${trac4medusa[i]}.fesom.ave.$startdate-$enddate.nc
  done

flx4medusa=('POC' 'PON' 'Opal' 'Calc' 'C13' 'C14' 'Cal13' 'Cal14')
for i in {0..7}
  do
     cdo ensmean ${indir}/sink${flx4medusa[i]}.fesom.[$startdate..$enddate]*.nc ${outdir}/sink${flx4medusa[i]}.fesom.ave.$startdate-$enddate.nc
  done

exit
