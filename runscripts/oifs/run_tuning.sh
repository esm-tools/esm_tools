#!/bin/bash

# Use sed to replace namelist variables for OpenIFS
# and launch a series of experiments
# Useful to run ensembles

# WARNING: Be careful with this. 
# The script will launch a lot of runs which could lead to 
# you filling up the entire queue 
# If you have > 10 concurrent jobs the admin usually gets angry 
# and before you know it you are on trial for sabotaging government property

rs=oifs-43r3-tco95-amip-blogin_qbo.yaml
expid_prefix="oifs-tco95"

rprcon=0
rvice=0
entrorg=0
detrpen=0
entrdd=0
ggaussb=1

# Dont do more than approx 10 in one go

## RPRCON
if [[ "x${rprcon}" == "x1" ]] ; then
    i=1
    
    for r in 1.0E-3 1.2E-3 1.6E-3 1.8E-3 1.4E-3 ; do        
        ii=$( printf "%03d" $i )
        echo " Run name: ${expid_prefix}_m${ii}"
        sed -i "s/RPRCON:.*/RPRCON: ${r}/" ${rs}    
        
        esm_runscripts ${rs} -e ${expid_prefix}_m${ii} 
        
        i=$(( $i + 1 ))
    done
fi

## RVICE
if [[ "x${rvice}" == "x1" ]] ;then
    i=1
    
    for r in 0.10 0.12 0.15 0.18 0.13 ; do
        ii=$( printf "%03d" $i )
        echo " Run name: ${expid_prefix}_n${ii}"
        sed -i "s/RVICE:.*/RVICE: ${r}/" ${rs}
	
        esm_runscripts ${rs} -e ${expid_prefix}_n${ii}
	
        i=$(( $i + 1 ))
    done
fi

## ENTRORG
if [[ "x${entrorg}" == "x1" ]] ;then
    i=1

    for r in 1.2E-3 1.4E-3 1.8E-3 2.0E-3 1.75E-3 ; do
        ii=$( printf "%03d" $i )
        echo " Run name: ${expid_prefix}_o${ii}"
        sed -i "s/ENTRORG:.*/ENTRORG: ${r}/" ${rs}
	
        esm_runscripts ${rs} -e ${expid_prefix}_o${ii}
	
        i=$(( $i + 1 ))
    done
fi

## DETRPEN
if [[ "x${detrpen}" == "x1" ]] ;then
    i=1

    for r in 0.4E-4 0.6E-4 0.9E-4 1.0E-4 0.75E-4 ; do
        ii=$( printf "%03d" $i )
        echo " Run name: ${expid_prefix}_p${ii}"
        sed -i "s/DETRPEN:.*/DETRPEN: ${r}/" ${rs}
	
        esm_runscripts ${rs} -e ${expid_prefix}_p${ii} 
	
        i=$(( $i + 1 ))
    done
fi

## ENTRDD
if [[ "x${entrdd}" == "x1" ]] ;then
    i=1

    for r in 1.0E-4 2.0E-4 4.0E-4 5.0E-4 3.0E-4 ; do
        ii=$( printf "%03d" $i )
        echo " Run name: ${expid_prefix}_q${ii}"
        sed -i "s/ENTRDD:.*/ENTRDD: ${r}/" ${rs}
	
        esm_runscripts ${rs} -e ${expid_prefix}_q${ii} 
	
        i=$(( $i + 1 ))
    done
fi

# GGAUSSB
if [[ "x${ggaussb}" == "x1" ]] ;then
    i=1

    for r in -0.35 -0.5 -0.75 -0.95 -0.25 ; do
	ii=$( printf "%03d" $i )
        echo " Run name: ${expid_prefix}_a${ii}"
        sed -i "s/GGAUSSB:.*/GGAUSSB: ${r}/" ${rs}

        esm_runscripts ${rs} -e ${expid_prefix}_a${ii} 

        i=$(( $i + 1 ))
    done
fi
