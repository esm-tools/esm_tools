#!/bin/bash
#
# OpenIFS model time is set by:
# * Date from ICMGG and ICMSH files (NINDAT variable)
# * Time step from rcf (CSTEP, set to NSTEP in model)
# 
# CTIME is used to identify the srf files, e.g. srf000003650000.0001
#
# WAM finds date using wam_namelist and waminfo
#

WORKDIR=$1
NEWDATE=$2
NEWDAYS=$3
NEWSTEP=$4

NEWDAYS_FMT=$(printf "%08d0000" $NEWDAYS)

MOD_WAMNML=0
MOD_RCF=1

echo " ========================== "
echo "    Modify OpenIFS date     "
echo "                            "
echo " MOD_WAMNML: ${MOD_WAMNML}  "
echo " MOD_RCF: ${MOD_RCF}        "
echo " NEWDATE: ${NEWDATE}        "
echo " NEWDAYS: ${NEWDAYS}        " 
echo " NEWSTEP: ${NEWSTEP}        "
echo " WORKDIR: ${WORKDIR}        " 
echo "                            " 
echo " ========================== "

if [[ "x${MOD_WAMNML}" == "x1" ]] ; then
    # Modify wam_namelist
    sed -i `grep -n "CBPLTDT" wam_namelist|cut -d ':' -f 1`"c \ CBPLTDT   = \"${INI_DATE_oifs}000000\"\," ${WORKDIR}/wam_namelist
    sed -i `grep -n "CDATECURA" wam_namelist|cut -d ':' -f 1`"c \ CDATECURA   = \"${INI_DATE_oifs}000000\"\," ${WORKDIR}/wam_namelist
    sed -i `grep -n "CDATEF" wam_namelist|cut -d ':' -f 1`"c \ CDATEF   = \"${INI_DATE_oifs}000000\"\," ${WORKDIR}/wam_namelist
fi

if [[ "x${MOD_RCF}" == "x1" ]] ; then
    # CSTEP is the time step OpenIFS thinks it starts on
    sed -i "s/CSTEP.*/CSTEP = \"${NEWSTEP}\",/" ${WORKDIR}/rcf 
    # CTIME is days since origin
    sed -i "s/CTIME.*/CTIME = \"${NEWDAYS_FMT}\",/" ${WORKDIR}/rcf
fi

