#!/bin/bash

# Use sed to replace namelist variables for OpenIFS
# and launch a series of experiments
# Useful to run ensembles

# WARNING: Be careful with this. 
# The script will launch a lot of runs which could lead to 
# you filling up the entire queue 
# If you have > 10 concurrent jobs the admin usually gets angry 
# and before you know it you are on trial for sabotaging government property

rs=oifs-43r3-tco95-1850-amip.yaml
expid="OIFS-1850"

# Dont do more than approx 10 in one go
for (( e=1 ; e<=10 ; e++ )) 
do
    # Runs will be labelled as "rip"
    # r = realisation
    # i = initialisation
    # p = physics
    #
    # Since our perturbation changes initial condtions
    # runs will be labelled e.g. r1i1p1, r1i2p1, r1i3p1 etc
    #
    rip=$(printf "r1i%dp1" $e)    
    echo " Run name: ${expid}_${rip}"
    sed -i "s/ensemble_id:.*/ensemble_id: ${e}/" ${rs}
    
    esm_runscripts ${rs} -e ${expid}_${rip}

done



