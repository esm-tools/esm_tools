#!/bin/bash

##This script prepares fesom output of concentrations of solutes
##(DIC, Alk, DIN, DSi, O2, DIC_13/14) and sinking fluxes of particles
##(POC, PON, calcite, opal and C isotopes in calcite and POC)

# define directories
indir=$1     
#fesom output: /work/ollie/yye/AWIESM_runs/PI_echamcold_nociso/outdata/fesom
outid=$2     
#save postprocessed files: experiment_dir/input/medusa?
outexpid=$3  #what for are outexpid and inexpid?
outidate=$4  #date of postprocessed files: $startdate-$enddate(current fesom year)
startdate=$5 #date of the first fesom file to be postprocessed
enddate=$6   #date of the last fesom file: same as the current fesom year

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
     cdo ensmean ${indir}/${trac4medusa[i]}.fesom.{$startdate..$enddate}.nc ${outdir}/${trac4medusa[i]}.fesom.ave.$startdate-$enddate.nc
  done

flx4medusa=('POC' 'PON' 'Opal' 'Calc' 'C13' 'C14' 'Cal13' 'Cal14')
for i in {0..7}
  do
     cdo ensmean ${indir}/sink${flx4medusa[i]}.fesom.*.nc ${outdir}/sink${flx4medusa[i]}.fesom.ave.$startdate-$enddate.nc
  done

# The following belongs to preprocessing for medusa:
# names of input/restart files need to be changed before each medusa run
# names of medusa output files adapted to the current fesom year
cat > medusa_recom_paleo_files.nml << EOF
&nml_cfg
cfn_nmlin_init     = 'medusa_seafloor_init.nml'
ctitle_ncfiles     = 'MEDUSA/RECOM_PALEO (r400,uXML_33)'
cfn_ncin_init      = 'medusa_reaclay_restart.nc'
cfn_ncin_flx       = 'medusa_flx_restart.nc'
!cfn_ncin_sedcore   = 'medusa_sedcore_restart.nc'
cfn_ncout_reaclay  = 'medusa_reaclay.$enddate.nc'
cfn_ncout_reaction = 'medusa_reaction.$enddate.nc'
cfn_ncout_procrate = 'medusa_procrate.$enddate.nc'
cfn_ncout_bc       = 'medusa_bc.$enddate.nc'
cfn_ncout_flx      = 'medusa_flx.$enddate.nc'
cfn_ncout_fesom    = 'medusa_flux2fesom.$enddate.nc'
!cfn_ncout_sedcore  = 'medusa_sedcore.$enddate.nc'
/
&nml_extra
cfn_ncout_aux      = 'medusa_aux.nc'
cfn_nmlin_forcing  = 'medusa_recom_paleo_forcing.nml'
cfn_csvout_summary = 'medusa_recom_paleo_summary.csv'
/
EOF

# update names of fesom input files
cat > medusa_recom_paleo_forcing.nml << EOF
&recom_file_list
cfn_fesomgrid = 'fesom.mesh.diag.nc'
cfn_temp = 'temp.fesom.ave.$startdate-$enddate.nc'
cfn_salt = 'salt.fesom.ave.$startdate-$enddate.nc'
cfn_dic = 'DIC.fesom.ave.$startdate-$enddate.nc',
cfn_din = 'DIN.fesom.ave.$startdate-$enddate.nc',
cfn_dsi = 'DSi.fesom.ave.$startdate-$enddate.nc',
cfn_alk = 'Alk.fesom.ave.$startdate-$enddate.nc',
cfn_o2  = 'O2.fesom.ave.$startdate-$enddate.nc',
!cfn_dic_13 = 'DIC_13.fesom.ave.$startdate-$enddate.nc',
!cfn_dic_14 = 'DIC_14.fesom.ave.$startdate-$enddate.nc',
cfn_sinkpoc = 'sinkPOC.fesom.ave.$startdate-$enddate.nc',
cfn_sinkpon = 'sinkPON.fesom.ave.$startdate-$enddate.nc',
cfn_sinkopal = 'sinkOpal.fesom.ave.$startdate-$enddate.nc',
cfn_sinkcalc = 'sinkCalc.fesom.ave.$startdate-$enddate.nc',
!cfn_sinkpoc13 = 'sinkC13.fesom.ave.$startdate-$enddate.nc',
!cfn_sinkpoc14 = 'sinkC14.fesom.ave.$startdate-$enddate.nc',
!cfn_sinkcal13 = 'sinkCal13.fesom.ave.$startdate-$enddate.nc',
!cfn_sinkcal14 = 'sinkCal14.fesom.ave.$startdate-$enddate.nc',
cfn_sinkclay = 'DustClimYearlyAlbani_PI.nc',
/
EOF


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


####to start or restart medusa###
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

