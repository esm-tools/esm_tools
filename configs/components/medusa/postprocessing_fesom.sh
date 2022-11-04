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

####The following is to run fesom:####

# check if it is a coupled run with medusa: in esm-tools?

# check if it is the first equilibrium run of medusa
# if not:
if [ -e $medusa_out/medusa_flx_restart.nc ]
then
  echo "continuation coupled fesom run"  
else
  echo "first coupled fesom run"
# additional namelist needed to switch on the coupling with medusa
# and read sedimentary input from medusa:
  cp ${fesom.model_dir}/config/namelist.recom.ciso.medusa namelist.recom
# copy fesom input to drive medusa or create link?
  ln -s $INITDIR/restart_files/fesom.3000.*.restart.nc .
  cp $INITDIR2/medusa_flux2fesom.3000.nc ./medusa_flux2fesom.nc
fi


####to restart medusa: medusa restart files (via esm-tools)###
  cp $INITDIR2/medusa_flx.3000.nc medusa_flx_restart.nc
  cp $INITDIR2/medusa_reaclay.3000.nc medusa_reaclay_restart.nc
  cp $INITDIR2/fesom.mesh.diag.nc .

####to reduce fesom output####
  echo "reducing output files"
  cp fesom.$enddate.*.restart.nc $SAVEDIR/restart_files
  cp *.fesom.$enddate.nc $SAVEDIR/tracer_output
  rm *.fesom.*.nc
  rm fesom.*.restart.nc


####should be done with esmtools:####
  echo "copy input files for medusa run"
  cp $SAVEDIR/input4medusa/*.fesom.ave.$startdate-$enddate.nc .

