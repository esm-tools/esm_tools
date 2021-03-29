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
TIMESTEP=$4
SECONDS_SINCE_INITAL=$5
DAYS_SINCE_INITIAL=$6
WAM=$7

NEWDAYS_FMT=$(printf "%08d0000" $NEWDAYS)


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

# Modify OIFS restart control file
# CSTEP is the time step OpenIFS thinks it starts on
CSTEP=$(printf $(($((SECONDS_SINCE_INITAL))/$TIMESTEP)))
sed -i `grep -n "CSTEP" ${WORKDIR}/rcf|cut -d ':' -f 1`"c \ CSTEP   = \" $CSTEP\"\," ${WORKDIR}/rcf
#sed -i "s/CSTEP.*/CSTEP = \"$(($((SECONDS_SINCE_INITAL))/$((TIMESTEP))))\",/" ${WORKDIR}/rcf 
# CTIME is days since origin
CTIME=$(printf '%08d' $(($DAYS_SINCE_INITIAL)))0000
sed -i `grep -n "CTIME" ${WORKDIR}/rcf|cut -d ':' -f 1`"c \ CTIME   = \"$(printf '%-9s' $CTIME)  \"\," ${WORKDIR}/rcf
#sed -i "s/CTIME.*/CTIME = \"${NEWDAYS_FMT}\",/" ${WORKDIR}/rcf

if [[ "x${WAM}" == "x1" ]] ; then
    # Modify WAM restart controll file
    sed -i `grep -n "ANALYSIS FROM" ${WORKDIR}/waminfo|cut -d ':' -f 1`"c ANALYSIS FROM ${NEWDATE}000000 TO ${NEWDATE}000000" ${WORKDIR}/waminfo
    sed -i `grep -n "BEGIN DATE" ${WORKDIR}/waminfo|cut -d ':' -f 1`"c BEGIN DATE FOR USING SURFACE CURRENT = ${NEWDATE}000000" ${WORKDIR}/waminfo
fi
