# flist_* can hold lists of files, separated by commas
flist_sst=\'tosbcs_input4MIPs_SSTsAndSeaIce_CMIP_PCMDI-AMIP-1-1-0_gs1x1_187001-201512.nc\'
flist_sic=\'siconcbcs_input4MIPs_SSTsAndSeaIce_CMIP_PCMDI-AMIP-1-1-0_gs1x1_187001-201512.nc\'

cat << EOF
!-----------------------------------------------------------------------
&NAMAMIP
!-----------------------------------------------------------------------
    RunLengthSec = ${RUNTIME_amip}
    TimeStepSec  = ${TIME_STEP_amip}
    StartYear    = ${START_DATE_amip:0:4}
    StartMonth   = ${START_DATE_amip:4:2}
    StartDay     = ${START_DATE_amip:6:2}
    FixYear      = ${CMIP_FIXYEAR_amip}
    FileListSST  = ${flist_sst}
    FileListSIC  = ${flist_sic}
    LDebug       = false
    LInterpolate = true
!-----------------------------------------------------------------------
/
EOF
