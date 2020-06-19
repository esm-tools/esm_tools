#!/bin/bash

# Usage:
# ./oifs-43r3-mistral-pp.sh mistral /work/dir/ expid start_date end_date 

# Arguments
MACHINE=$1

if [[ "x$MACHINE" == "xmistral" ]] ; then
   export GRIB_API_BIN_DIR=/sw/rhel6-x64/grib_api/grib_api-1.15.0-intel14/bin/
   grib_copy=/sw/rhel6-x64/grib_api/grib_api-1.15.0-intel14/bin/grib_copy
   grib_set=/sw/rhel6-x64/grib_api/grib_api-1.15.0-intel14/bin/grib_set
   grib_ls=/sw/rhel6-x64/grib_api/grib_api-1.15.0-intel14/bin/grib_ls
   
   module purge
   module load netcdf_c/4.3.2-gcc48 
   module load cdo/1.9.8-gcc64 
else
   echo "Machine: $MACHINE not supported"
fi

echo " Purpose  " 
echo " Convert OpenIFS output to netCDF files  "
echo " "
echo " Methods  "
echo " -------  " 
echo " * Use CDO to split the ICMSH and ICMGG files  "
echo "   to one file vertical coordinate, e.g. hybrid, surface etc. "
echo "   As surface variables are always stored as GRIB1 and hybrid variables "
echo "   It may be necessary to split according to GRIB edition sometime later " 
echo " * Use CDO to convert from reduced Gaussian or spectral to regular Gaussian grid "
echo " * Use CDO to convert the grib file to netCDF4/HDF5. "
echo "   Level of compression set by CDO_ZIP variable. "
echo " "

PP_MODE="geomar"
#PP_MODE="awi"

# Settings for this run 
VERSION="43r3"
TRUNC="cubic"
WITH_WAM="0"
OUTPUT_DIR=$2
TMP_DIR=${OUTPUT_DIR}/TMP-IFS2NC/
PROCESSED_DIR=$OUTPUT_DIR/../outdata/oifs/
EXPID=$3
START_DATE=$4
END_DATE=$5

if [[ "x${TRUNC}" == "xcubic" ]] ; then
   sp2gp="-sp2gp,cubic"
fi

echo " Settings for post processing "
echo " ============================ "
echo " "
echo " Machine: $MACHINE "
echo " Style: $PP_MODE "
echo " Version: $VERSION "
echo " "
echo " Exp ID: $EXPID "
echo " Start date: $START_DATE "
echo " End date: $END_DATE "
echo " With WAM: $WITH_WAM "
echo " "
echo " Raw output dir: $OUTPUT_DIR "
echo " Processed output dir: $PROCESSED_DIR "
echo " " 
echo " grib_copy: $grib_copy"
echo " cdo -V "
cdo -V 
echo " ncdump "
which ncdump 
echo " "

mkdir -vp $TMP_DIR
mkdir -vp $PROCESSED_DIR

if [[ "x$PP_MODE" == "xgeomar" ]]; then # GEOMAR post processing
    
    echo " OpenIFS post-processing, GEOMAR-style " 
    
    echo "    - Concatenating OIFS output files... "
    pwd
    ls ICMGG${EXPID}+*
    ls ICMSH${EXPID}+*
    cat ICMGG${EXPID}+* > ${TMP_DIR}/ICMGGCAT
    cat ICMSH${EXPID}+* > ${TMP_DIR}/ICMSHCAT
    if [[ "x${VERSION}" == "x43r3" ]] ; then
        ls ICMUA${EXPID}+*
        cat ICMUA${EXPID}+* > ${TMP_DIR}/ICMUACAT
    fi
    if [[ "x${WITH_WAM}" == "x1" ]]; then
        # WAM output comes in two files
	# One for all time steps except the last, and one for the last
        if files=$(ls MPP*); then
   	    cat MPP* > ${TMP_DIR}/MPPCAT
	else
	    echo " Warning: Did not find MPP files from WAM for post processing "
	fi
    fi 
    
    echo "   - Converting data to NetCDF " 
    		
    # 
    # So far I have only put in four levels types
    levelTypes="surface hybrid isobaricInhPa isobaricInPa theta pv"
    for typeOfLevel in ${levelTypes}
    do 

        echo "       * Remap OpenIFS ${typeOfLevel} output to regular Gaussian grid, and netCDF format "
        # We must split to one file per level type, since CDO can only deal with one 
        # vertical coordinate at a time
	$grib_copy -w typeOfLevel=$typeOfLevel ${TMP_DIR}/ICMGGCAT ${TMP_DIR}/ICMGGCAT_${typeOfLevel}			 
	$grib_copy -w typeOfLevel=$typeOfLevel ${TMP_DIR}/ICMSHCAT ${TMP_DIR}/ICMSHCAT_${typeOfLevel}
	if [[ "x${VERSION}" == "x43r3" ]] ; then 
	   $grib_copy -w typeOfLevel=$typeOfLevel ${TMP_DIR}/ICMUACAT ${TMP_DIR}/ICMUACAT_${typeOfLevel}
      fi
      
        # Test if this file exists, i.e. this level type is available
        if [[ -e ${TMP_DIR}/ICMGGCAT_${typeOfLevel} ]]; then
            ICMGG_NAME=${EXPID}_${START_DATE}_${END_DATE}_GG_${typeOfLevel}.nc 
            cdo --eccodes -f nc -t ecmwf copy -setgridtype,regular ${TMP_DIR}/ICMGGCAT_${typeOfLevel} ${TMP_DIR}/${ICMGG_NAME}
            rm -rf ${TMP_DIR}/ICMGGCAT_${typeOfLevel}
        fi 
	if [[ -e ${TMP_DIR}/ICMSHCAT_${typeOfLevel} ]]; then
            ICMSH_NAME=${EXPID}_${START_DATE}_${END_DATE}_SH_${typeOfLevel}.nc
            cdo --eccodes -f nc -t ecmwf copy ${sp2gp} ${TMP_DIR}/ICMSHCAT_${typeOfLevel} ${TMP_DIR}/${ICMSH_NAME}
            rm -rf ${TMP_DIR}/ICMSHCAT_${typeOfLevel}
        fi
        if [[ "x${VERSION}" == "x43r3" ]] ; then
            if [[ -e ${TMP_DIR}/ICMUACAT_${typeOfLevel} ]]; then
                ICMUA_NAME=${EXPID}_${START_DATE}_${END_DATE}_UA_${typeOfLevel}.nc
                cdo --eccodes -f nc -t ecmwf copy -setgridtype,regular ${sp2gp} ${TMP_DIR}/ICMUACAT_${typeOfLevel} ${TMP_DIR}/${ICMUA_NAME}
                rm -rf ${TMP_DIR}/ICMUACAT_${typeOfLevel}
            fi
        fi
    
    done		
		
    if [[ "x${oifs_WITH_WAM}" == "x1" ]]; then
        # 
        # Convert WAM output to netCDF
        #
        MPP_NAME=${EXPID}_${START_DATE}_${END_DATE}_WAM.nc
        echo "       * Convert WAM output to netCDF "
        if [[ -e ${TMP_DIR}/MPPCAT ]] ; then 
            cdo --eccodes -f nc -t ecmwf copy -setgridtype,regular ${TMP_DIR}/MPPCAT ${TMP_DIR}/${MPP_NAME}
        else
            echo "      WARNING: No MPPCAT file found "
        fi
    fi
				
    #echo "Removing raw OIFS output"
    #rm -f ICMGG* ICMSH* ICMUA* MPP*
		
    echo "      * Cutting off the last OIFS timestep that we write because of ifs_lastout"
    
    echo "ICMGG_NAME: $ICMGG_NAME"
    ncdump -h ${TMP_DIR}/${ICMGG_NAME} >> ${TMP_DIR}/temp
    INPUT=$(awk 'NR==3' ${TMP_DIR}/temp)
    rm ${TMP_DIR}/temp
    SUBSTRING=$(echo $INPUT| cut -d'(' -f2 )
    LAST_TIME_STEP=$(echo $SUBSTRING| cut -d' ' -f1 )
    SECOND_LAST_TIME_STEP=$((LAST_TIME_STEP - 1))
		
    for typeOfLevel in ${levelTypes} 
    do
	ICMGG_NAME=${EXPID}_${START_DATE}_${END_DATE}_GG_${typeOfLevel}.nc
        ICMSH_NAME=${EXPID}_${START_DATE}_${END_DATE}_SH_${typeOfLevel}.nc
        if [[ -e ${TMP_DIR}/${ICMGG_NAME} ]] ; then
        
            cdo seltimestep,1/$SECOND_LAST_TIME_STEP ${TMP_DIR}/${ICMGG_NAME} ${TMP_DIR}/${ICMGG_NAME}_cut.nc
            rm -f ${TMP_DIR}/${ICMGG_NAME}
            mv ${TMP_DIR}/${ICMGG_NAME}_cut.nc ${ICMGG_NAME}

            echo "      * Calculating day/monthly mean values for OIFS output ${ICMGG_NAME}"
            cdo daymean ${ICMGG_NAME} ${EXPID}_1d_${START_DATE}_${END_DATE}_GG_${typeOfLevel}.nc
            cdo monmean ${ICMGG_NAME} ${EXPID}_1m_${START_DATE}_${END_DATE}_GG_${typeOfLevel}.nc
        
        fi
        
        if [[ -e ${TMP_DIR}/${ICMSH_NAME} ]]; then
            
            cdo seltimestep,1/$SECOND_LAST_TIME_STEP ${TMP_DIR}/${ICMSH_NAME} ${TMP_DIR}/${ICMSH_NAME}_cut.nc
	    rm -f ${TMP_DIR}/${ICMSH_NAME}
            mv ${TMP_DIR}/${ICMSH_NAME}_cut.nc ${ICMSH_NAME}

            echo "Calculating day/monthly mean values for OIFS output ${ICMSH_NAME}"
            cdo daymean ${ICMSH_NAME} ${EXPID}_1d_${START_DATE}_${END_DATE}_SH_${typeOfLevel}.nc
            cdo monmean ${ICMSH_NAME} ${EXPID}_1m_${START_DATE}_${END_DATE}_SH_${typeOfLevel}.nc
	    
        fi
	
        if [[ "x${VERSION}" == "x43r3" ]] ; then
        
            ICMUA_NAME=${EXPID}_${START_DATE}_${END_DATE}_UA_${typeOfLevel}.nc

            if [[ -e ${ICMUA_NAME} ]] ; then
            
                cdo seltimestep,1/$SECOND_LAST_TIME_STEP ${TMP_DIR}/${ICMUA_NAME} ${TMP_DIR}/${ICMUA_NAME}_cut.nc
                rm -f ${TMP_DIR}/${ICMUA_NAME}
                mv ${TMP_DIR}/${ICMUA_NAME}_cut.nc ${ICMUA_NAME}
			    
                echo "Calculating day/monthly mean values for OIFS output ${ICMUA_NAME}"
                cdo daymean ${ICMUA_NAME} ${EXPID}_1d_${START_DATE}_${END_DATE}_UA_${typeOfLevel}.nc
                cdo monmean ${ICMUA_NAME} ${EXPID}_1m_${START_DATE}_${END_DATE}_UA_${typeOfLevel}.nc
	    
            fi
	fi
    done                                
		
    echo "   - End of GEOMAR post-processing "
		
fi

