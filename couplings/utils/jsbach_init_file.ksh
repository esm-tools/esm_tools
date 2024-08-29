#!/bin/ksh
#------------------------------------------------------------------------------
# Script to compile and run jsbach_init_file.f90
# For more information read the header of jsbach_init_file.f90.
#
# The script is usually called by create_input_cosmos.ksh, were several
# variables are exported (interactive=false).
# Alternatively, it can be run interactively (interactive=true), to just create
# a specific initial file for jsbach.
#
# To generate a series of vegetation cover maps for runs with landcover change,
# the script needs to be called interactivly with landcover_series=true
#
# Veronika Gayler
# Stiig Wilkenskjeld, 2012-01: Added variables for 5 layer soil scheme
#------------------------------------------------------------------------------
#
# !NOTE FOR USAGE ON MISTRAL: MAKE SURE TO LOAD THE MODULE 'NAG' PRIOR TO
# EXECUTION OF THIS SCRIPT, OR THE PREDEFINED COMPILER (nagfor) WILL NOT BE
# FOUND!
#						 Christian Stepanek, 20.04.2016
#
#------------------------------------------------------------------------------
# Updates to this script so it is suitable for iterative coupling.
# * Throws away unneeded library definition variables.
# * Turns echam input files into passable arguments. 
# 
#                                               Paul Gierz, 15.08.2018
#------------------------------------------------------------------------------

set -e

# Variables that need to be defined if the sript is used interctively.
# If called from create_input_cosmos.ksh these variables are exported.
#
res_atm=T63                 # horizontal grid resopution
res_oce=GR15                # ocean model grid (for a coupled setup)

ntiles=11                   # number of jsbach tiles

dynveg=true                 # setup for dynamic vegetation 
c3c4crop=true               # differentiate between C3 and C4 crops
cmip5_pasture=true          # use landuse maps for pastures and crops

year_ct=1850                # year the cover_types are derived from
year_cf=1850                # year cover fractions are derived from

landcover_series=false      # generate a series of files with cover_types of 
                            # year_ct and fractions from year_cf to year_cf2
year_cf2=1859               # only used with landcover_series

echam_fractional=false      # initial file for echam runs with fractional
                            # land sea mask
# TODO: Replace this with a ${POOL_DIR}, which should be defined by esm-runscripts...
pool=/work/ollie/pool/ECHAM6/T63 # directories with echam input data
pool_land=/work/ollie/pool/JSBACH/prepare/T63
srcdir=./

# TODO: Something that copies prog from utils here

if [[ -f ${prog} ]]; then
        #------------------------------------------------------------------------------
        # prepare the namelist
        #------------------------------------------------------------------------------

        [[ ${res_oce} = "" ]] && lcouple=.false. || lcouple=.true.
        [[ ${dynveg} = true ]] && ldynveg=.true. || ldynveg=.false.
        [[ ${c3c4crop} = true ]] && lc3c4crop=.true. || lc3c4crop=.false.
        [[ ${cmip5_pasture} = true ]] && lcmip5_pasture=.true. || lcmip5_pasture=.false.

        if [[ ${ntiles} -eq 11 || ${dynveg} = true ]]; then
                lpasture=.true.
        else
                lpasture=.false.
        fi

        desert_only=.false.         # setup for a desert-only experiment
        grass_only=.false.          # setup for a grass-only experiment
        woods_only=.false.          # setup for a woods-only experiment

        cat > namelist <<EOF
&INITCTL
  res_atm="${res_atm}"
  res_oce="${res_oce}"
  ntiles=${ntiles}
  nlct=21
  year_ct=${year_ct}
  year_cf=${year_cf}
  lcouple=${lcouple}
  ldynveg=${ldynveg}
  lc3c4crop=${lc3c4crop}
  lpasture=${lpasture}
  lcmip5_pasture=${lcmip5_pasture}
  echam_fractional=.${echam_fractional}.
  desert_only=${desert_only}
  grass_only=${grass_only}
  woods_only=${woods_only}
  cover_fract_only=.${landcover_series}.
  info=.false.
/
EOF

        #------------------------------------------------------------------------------
        # get input data from the pools
        #------------------------------------------------------------------------------
        JAN_SURF_file=$1
        VGRATCLIM_file=$2
        VLTCLIM_file=$3
        JSBACH_RESTART_file=$4


        ln -sf $JAN_SURF_file T63GR15_jan_surf.nc
        ln -sf $VGRATCLIM_file ${res_atm}${res_oce}_VGRATCLIM.nc
        ln -sf $VLTCLIM_file   ${res_atm}${res_oce}_VLTCLIM.nc

        ln -sf ${pool}/${res_atm}_TSLCLIM2.nc   .

        if [ $(echo ${res_atm} | cut -c1) != T ]; then
                res_atmg=${res_atm}
        else
                res_atmg=${res_atm}gauss
        fi
        if [[ ${cmip5_pasture} = true ]]; then
                ln -sf ${pool_land}/vegtype_1850_${res_atmg}_pa14.nc \
                        vegtype_${year_cf}_${res_atm}gauss_pa14.nc
        else
                ln -sf ${pool_land}/vegtype_${year_cf}_${res_atmg}_pa14.nc\
                        vegtype_${year_cf}_${res_atm}gauss_pa14.nc
        fi
        if [[ ${year_cf} != ${year_ct} ]]; then
                ln -sf ${pool_land}/vegtype_${year_ct}_${res_atmg}_pa14.nc \
                        vegtype_${year_ct}_${res_atm}gauss_pa14.nc
        fi
        ln -sf ${pool_land}/vegmax_6_${res_atm}.lola  .
        ln -sf ${pool_land}/${res_atm}_topo_75.lola   .
        ln -sf ${pool_land}/albedo_${res_atm}.lola    .
        ln -sf ${pool_land}/C3C4_mask_${res_atmg}.nc \
                      C3C4_mask_${res_atm}gauss.nc
        ln -sf ${pool_land}/potveg_${res_atm}.nc      .
        if [[ ${c3c4crop} = true ]]; then
                ln -sf ${pool_land}/C3C4_crop_${res_atm}.nc   .
        fi
        if [[ ${cmip5_pasture} = true ]]; then
                if [[ -f ${pool_land}/LUH_states_${res_atm}.nc ]]; then
                        ln -sf ${pool_land}/LUH_states_${res_atm}.nc .
                elif [[ -f ${pool_land}/LUH_states_${res_atm}.nc.gz ]]; then
                        gunzip -c ${pool_land}/LUH_states_${res_atm}.nc.gz  \
                                > LUH_states_${res_atm}.nc
                fi
                cdo selyear,${year_cf} LUH_states_${res_atm}.nc \
                           LUH_states_${year_cf}_${res_atm}.nc
        fi

        ln -sf ${pool_land}/soil_parameters_${res_atm}.nc .

        #------------------------------------------------------------------------------
        # run the program
        #------------------------------------------------------------------------------
        echo "Run ${prog}..."
        chmod 755 ${prog}

        yr=${year_cf}
        [[ ${landcover_series} = true ]] || year_cf2=${year_cf}
        while [[ ${yr} -le ${year_cf2} ]]; do
                sed "s/year_cf=.*/year_cf=${yr}/" namelist > namelist.tmp
                mv namelist.tmp namelist
                ./${prog}
                (( yr = yr + 1 ))
        done

        #------------------------------------------------------------------------------
        # clean up
        #------------------------------------------------------------------------------
        if [[ $? -eq 0 ]]; then
                rm namelist
                rm ${res_atm}${res_oce}_jan_surf.nc
                rm ${res_atm}${res_oce}_VGRATCLIM.nc 
                rm ${res_atm}${res_oce}_VLTCLIM.nc
                rm ${res_atm}_TSLCLIM2.nc
                rm vegtype_${year_cf}_${res_atm}gauss_pa14.nc
                if [[ ${year_cf} != ${year_ct} ]]; then
                        rm vegtype_${year_ct}_${res_atm}gauss_pa14.nc
                fi
                rm vegmax_6_${res_atm}.lola
                rm ${res_atm}_topo_75.lola
                rm albedo_${res_atm}.lola
                rm C3C4_mask_${res_atm}gauss.nc
                rm potveg_${res_atm}.nc
                if [[ ${c3c4crop} = true ]]; then
                        rm C3C4_crop_${res_atm}.nc
                fi
                if [[ ${cmip5_pasture} = true ]]; then
                        rm LUH_states_${res_atm}.nc
                        rm LUH_states_${year_cf}_${res_atm}.nc
                fi
                [[ -f mo_kinds.mod ]] && rm mo_kinds.mod
                [[ -f mo_vegparams.mod ]] && rm mo_vegparams.mod
                rm -f 5soillayers_${res_atm}.nc soil_parameters_${res_atm}.nc
        else
                echo "error in ${prog}"
                exit 1  
        fi
else
        echo "${prog} could not be created"
        exit 1
fi

ofile=jsbach_T63GR15_11tiles_5layers_Lev_21ka_noTOPO_xzhang.nc
mv jsbach_T63GR15_11tiles_5layers_1850.nc ${ofile}
