#!/usr/bin/ksh

function vilma2ice {
        echo "                =================================================================="
        echo "                 *** S T A R T I N G    vilma2ice *** "; echo

        echo "                VILMA_TO_ICE=${VILMA_TO_ICE}"

        if [[ "x$VILMA_TO_ICE" == "x1" ]]; then

                iterative_coupling_vilma_ice_write_names
                iterative_coupling_vilma_ice_make_forcing

        else
                echo " NOT generating solid earth forcing for ice"
        fi
        echo
        echo "                 *** F I N I S H E D    vilma2ice *** "
        echo "                =================================================================="
}

function iterative_coupling_vilma_ice_write_names {
        : > ${COUPLE_DIR}/solidearth_names_for_ice.dat
        SOLIDEARTH_grid=${VILMA_GRID_input}
        BEDROCK_CHANGE_VARNAME_vilma=rsl
        echo "bedrock_change_name=${BEDROCK_CHANGE_VARNAME_vilma}" >> ${COUPLE_DIR}/solidearth_names_for_ice.dat
        echo "RUN_NUMBER_solidearth=${RUN_NUMBER_vilma}" >> ${COUPLE_DIR}/solidearth_names_for_ice.dat
}

function iterative_coupling_vilma_ice_make_forcing {

        ice_bedrock_change_file=${COUPLE_DIR}/bedrock_change.nc

        cp ${DATA_DIR_vilma}/rsl.nc ${COUPLE_DIR}/new_bedrock.nc

        # Subtract the previous bedrock conditions to obtain the change
        # At first time step there are no previous conditions to subtract
        if [ "x${RUN_NUMBER_vilma}" == "x1" ]; then
                cp ${COUPLE_DIR}/new_bedrock.nc ${ice_bedrock_change_file}
        else
                cdo -s \
                        sub ${COUPLE_DIR}/new_bedrock.nc ${COUPLE_DIR}/old_bedrock.nc \
                        ${ice_bedrock_change_file}
        fi

        # rsl is minus the change in bedrock
        ncap2 -s 'rsl=-rsl' ${ice_bedrock_change_file} -A ${ice_bedrock_change_file}

        # The new conditions become the old for the next timestep 
        mv ${COUPLE_DIR}/new_bedrock.nc ${COUPLE_DIR}/old_bedrock.nc

}
