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

RESTARTDIR=$1
NEWDATE=$2
TIMESTEP=$3
SECONDS_SINCE_INITAL=$4
DAYS_SINCE_INITIAL=$5
WAM=$6
# Optional, only passed on a branch-off: seconds from NEWDATE to the end of
# this leg (oifs.simulation_length_seconds). Used to also shift the run dates
# in waminfo, which otherwise keep the parent experiment's dates.
SIMULATION_LENGTH_SECONDS=$7
#NEWDAYS=$7

#NEWDAYS_FMT=$(printf "%08d0000" $NEWDAYS)


echo " ========================== "
echo "    Modify OpenIFS date     "
echo "                            "
#echo " MOD_WAMNML: ${MOD_WAMNML}  "
#echo " MOD_RCF: ${MOD_RCF}        "
echo " NEWDATE: ${NEWDATE}        "
echo " WAM: ${WAM}                "
#echo " NEWSTEP: ${NEWSTEP}        "
echo " RESTARTDIR: ${RESTARTDIR}  "
echo "                            "
echo " ========================== "

# Modify OIFS restart control file
# CSTEP is the time step OpenIFS thinks it starts on
CSTEP=$(printf $(($((SECONDS_SINCE_INITAL))/$TIMESTEP)))
sed -i "s/CSTEP[^,]*,/CSTEP   = \" $CSTEP\"\,/g" ${RESTARTDIR}/rcf
#sed -i "s/CSTEP.*/CSTEP = \"$(($((SECONDS_SINCE_INITAL))/$((TIMESTEP))))\",/" ${RESTARTDIR}/rcf
# CTIME is days since origin
CTIME=$(printf '%08d' $(($DAYS_SINCE_INITIAL)))0000
sed -i "s/CTIME[^,]*,/CTIME   = \"$(printf '%-9s' $CTIME)  \"\,/g" ${RESTARTDIR}/rcf
#sed -i "s/CTIME.*/CTIME = \"${NEWDAYS_FMT}\",/" ${RESTARTDIR}/rcf

if [[ "x${WAM}" == "x1" ]] || [[ "x${WAM}" == "xtrue" ]] || [[ "x${WAM}" == "xTrue" ]]; then
    sed -i `grep -n "ANALYSIS FROM" ${RESTARTDIR}/waminfo|cut -d ':' -f 1`"c ANALYSIS FROM ${NEWDATE}000000 TO ${NEWDATE}000000" ${RESTARTDIR}/waminfo
    sed -i `grep -n "BEGIN DATE" ${RESTARTDIR}/waminfo|cut -d ':' -f 1`"c BEGIN DATE FOR USING SURFACE CURRENT = ${NEWDATE}000000" ${RESTARTDIR}/waminfo

    if [[ -n "${SIMULATION_LENGTH_SECONDS}" ]]; then
        # Branch-off: waminfo still has the parent's run dates. Coupled WAM
        # (userin.F90) aborts unless the "RUN MODEL FROM" date equals the
        # atmosphere's start, i.e. one time step before this leg's start;
        # WAM itself writes the same pattern on a regular restart, e.g.
        # "RUN MODEL FROM 20741231234000 TO 20751231234000" for 2075.
        # The fixed column widths are the ones readsta.F90 reads.
        wam_date() {
            date -u -d "${NEWDATE:0:4}-${NEWDATE:4:2}-${NEWDATE:6:2} 00:00:00 UTC + $1 seconds" +%Y%m%d%H%M%S
        }
        RUN_FROM=$(wam_date $((SECONDS_SINCE_INITAL - TIMESTEP)))
        RUN_TO=$(wam_date $((SIMULATION_LENGTH_SECONDS - TIMESTEP)))
        FOREPD=$((SIMULATION_LENGTH_SECONDS - SECONDS_SINCE_INITAL))
        sed -i "s/^RUN MODEL FROM .*/RUN MODEL FROM ${RUN_FROM} TO ${RUN_TO}/" ${RESTARTDIR}/waminfo
        sed -i "s/^FORECAST PERIOD = .*/FORECAST PERIOD = $(printf '%10d' ${FOREPD})/" ${RESTARTDIR}/waminfo
        # Restart output date: WAM clamps it to the run and updates it while
        # running, but keep it inside this leg rather than the parent's.
        sed -i "s/^DATE FOR OUTPUT OF BOTH RESTART FILES = .*/DATE FOR OUTPUT OF BOTH RESTART FILES = ${RUN_TO}/" ${RESTARTDIR}/waminfo

        # The LAW (wind/stress) restart files start with one unformatted record
        # of four 14-character dates, the restart date first (bytes 4-59 after
        # the 4-byte record marker). WAM's INITMDL aborts if that date is not
        # within the run ("START DATE FROM RESTART FIELD IS NOT IN MODEL
        # PERIOD"), so shift all four by the same offset as the run dates.
        to_epoch() {
            date -u -d "${1:0:4}-${1:4:2}-${1:6:2} ${1:8:2}:${1:10:2}:${1:12:2} UTC" +%s
        }
        for law in ${RESTARTDIR}/LAW*; do
            [[ -f "${law}" ]] || continue
            # esm_tools links restart files from the parent experiment into the
            # work dir; never write through the link into the parent's files
            if [[ -L "${law}" ]]; then
                cp --remove-destination "$(readlink -f "${law}")" "${law}"
            fi
            # One read and one write per file: byte-wise dd is very slow on Lustre
            header=$(dd if="${law}" bs=56 count=1 skip=4 iflag=skip_bytes 2>/dev/null)
            if [[ ! "${header}" =~ ^[0-9]{56}$ ]]; then
                echo "WARNING: ${law}: no date header found, not shifted"
                continue
            fi
            offset=$(( $(to_epoch ${RUN_FROM}) - $(to_epoch ${header:0:14}) ))
            new_header=""
            for i in 0 14 28 42; do
                new_header+=$(date -u -d "@$(( $(to_epoch ${header:$i:14}) + offset ))" +%Y%m%d%H%M%S)
            done
            printf '%s' "${new_header}" | dd of="${law}" bs=56 count=1 seek=4 iflag=fullblock oflag=seek_bytes conv=notrunc 2>/dev/null
        done
        echo " LAW restart dates shifted to start ${RUN_FROM}"
    fi
fi
