#!/bin/bash

# Use sed to replace namelist variables for OpenIFS
# and launch a series of experiments
# Useful for testing the sensitivity to various parameters

# WARNING: Be careful with this. 
# The script will launch a lot of runs which could lead to 
# you filling up the entire queue 
# If you have > 10 concurrent jobs the admin usually gets angry 
# and before you know it you are on trial for sabotaging government property

rs=oifs-43r3-tco95-tuning-blogin_annual.yaml

TEST_RVICE=0
TEST_ENTRORG=0
TEST_RPRCON=0
TEST_ALBSCALE=1
TEST_SCLCT=0

# RVICE: 0.1 0.13 0.15 0.18
if [[ "x$TEST_RVICE" == "x1" ]] ; then
    i=0
    for rvice in 0.1 0.15 0.18 0.13
    do 
    
        i=$(( $i + 1 ))
        ii=$(printf "%03d" $i)    
        echo $rvice $ii
        sed -i "s/RVICE:.*/RVICE: ${rvice}/" ${rs}
        
        esm_runscripts ${rs} -e oifs-tco95-g${ii}

    done
fi

# ENTRORG: 1.2 1.5 1.75 2.0
if [[ "x$TEST_ENTRORG" == "x1" ]] ; then
     i=0  
     for entrorg in 1.2 1.5 2.0 1.75 
     do
         i=$(( $i + 1 )) 
         ii=$(printf "%03d" $i)
         echo $entrorg $ii 
         sed -i "s/ENTRORG:.*/ENTRORG: ${entrorg}E-3/" ${rs}
         esm_runscripts ${rs} -e oifs-tco95-h${ii}
     
    done 
fi

# RPRCON: 1.1 1.3 1.7 1.9
if [[ "x$TEST_RPRCON" == "x1" ]] ; then
     i=0
     for rprcon in 1.1 1.7 1.9 1.3
     do
          i=$(( $i + 1 ))
          ii=$(printf "%03d" $i)
          echo "RPRCON: $rprcon i$ii" 
          sed -i "s/RPRCON:.*/RPRCON: ${rprcon}E-3/" ${rs}
          esm_runscripts ${rs} -e oifs-tco95-i${ii}
     done
fi

# ALBSCALE: 0.8 0.9 
if [[ "x$TEST_ALBSCALE" == "x1" ]] ; then
     i=0
     for alb in 0.8 0.9 1
     do
          i=$(( $i + 1 ))
          ii=$(printf "%03d" $i)
          echo $alb $ii
          sed -i "s/RALBSCALE_AR:.*/RALBSCALE_AR: ${alb}/" ${rs}
          esm_runscripts ${rs} -e oifs-tco95-j${ii}
     done
fi 

# SCLCT_SWITCH:
if [[ "x$TEST_SCLCT" == "x1" ]] ; then
    i=0
    for scl in 1 2 3 0
    do
        i=$(( $i + 1 ))
        ii=$(printf "%03d" $i)
        echo "SCLCT: " $scl $ii
        sed -i "s/SCLCT_SWITCH:.*/SCLCT_SWITCH: ${scl}/" ${rs}
        esm_runscripts ${rs} -e oifs-tco95-k${ii}
    done
fi 


