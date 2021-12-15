#!/bin/bash
# Builds a netCDF file with a time contentiously increasing time axis
#
# The increasing time axis of $ELEMENTS_NO elements is written to the
# file $OUTPUT_NC. A given offset of $OFFSET is considered.
#
# NOTE: Each month has an identical length and the time are integer-like values
#
# Call:
#          CreateTimeAxisNC.bash ELEMENTS_NO (OUTPUT_NC) (OFFSET) (TYPE) (CALENDAR) (NUM_TYPE)
#
# ELEMENTS_NO : Number of time steps
# OUTPUT_NC   : Netcdf output file name, optional (default=axis.nc)
# OFFSET      : Time offset in years, optional (default=0)
# TYPE        : NetCDF file file type, as reported by `ncdump -k`
#               (default=classic;
#                valid=classic/nc3/3, "64-bit offset"/nc6/6,
#                      "64-bit data"/nc5/5, netCDF-4/nc4/4, 
#                      "netCDF-4 classic model"/nc7/7)
# CALENDAR    : Calendar type 360, 365, ... (default=360)
# NUM_TYPE    : Number type of time axis (default=double;
#               valid=double, float, int)
#
# Examples:
#          ./CreateTimeAxisNC.bash 100 TimeAxis.nc
#          ./CreateTimeAxisNC.bash 100 TimeAxis.360.nc 0 classic 360_day double
# 
# (c) Christian Rodehacke, AWI, 2017-09-27 (first version)
#     Christian Rodehacke, AWI, 2017-09-28 (NetCDF type:  optional 4th parameter)
#     Christian Rodehacke, AWI, 2018-09-12 (Calendar:     optional 5th parameter)
#     Christian Rodehacke, AWI, 2018-09-19 (Numeric tyupe:optional 6th parameter)
#
NO_LINES=25 ; NO_NEED_INPUTS=1
set -e
if [ $# -lt ${NO_NEED_INPUTS} ] ; then
    head -n ${NO_LINES} $0
    echo
    echo " Need at least ${NO_NEED_INPUTS} input parameter(s)"
    echo
    exit 1
fi

ELEMENTS_NO=$1
if [ $# -ge 2 ] ; then
    OUTPUT_NC=$2
else
    OUTPUT_NC="axis.nc" # default value"
fi
if [ $# -ge 3 ] ; then
    OFFSET=$3
else
    OFFSET=0
fi
if [ $# -ge 4 ] ; then
    TYPE=$4
else
    TYPE="classic"
fi
if [ $# -ge 5 ] ; then
    CALENDAR=$5
else
    CALENDAR="360"
fi

if [ $# -ge 6 ] ; then
    NUM_TYPE=$6
else
    NUM_TYPE="double"
fi


DEBUG_FLAG=0


# ========================================================================
#
# Subroutines, functions
#
# ========================================================================

function CreateTimeAxisNC() {
    # Creates a netcdf file with linearily increasing time axis starting at zero
    # Call: CreateTimeAxisNC NumberToGo0 TIME_AXIS_FILE (OffsetYear)
    NumberToGo0_=$1
    TIME_AXIS_FILE_=$2
    if [ $# -ge 3 ] ; then
	OffsetYear_=$3
    else
	OffsetYear_=0
    fi
    if [ $# -ge 4 ] ; then
	calendar_wanted_=$4
    else
	calendar_wanted_=360
    fi
    if [ $# -ge 5 ] ; then
	number_type_=$5
    else
	number_type_="double"
    fi
    #
    NoRequire__=2
    if [ $# -lt ${NoRequire__} ] ; then
	echo " ***** Missing required ${NoRequire__} input variables in 'CreateTimeAxisNC' in $0 *****"  1>&2
	echo "   *** $# :: Received $* *****"  1>&2
	exit 102
    fi
    #
    # Ensure a dummy value for optional parameter
    #
    OffsetYear_=${OffsetYear_:=0}


    #
    # Function body
    #
    SECONDS_PER_DAY_=86400
    MINUTES_PER_DAY_=1440

    calendar_wanted_=${calendar_wanted_:-360}

    case ${calendar_wanted_} in
	"366"|"366_day")
	    DAYS_PER_YEAR_=366           ; DAYS_PER_MONTH_=30.5         # 366_day; Not used in PISM !!
	    CALENDAR_="${DAYS_PER_YEAR_}_day"
	    ;;
	"365.242198781"|"standard")
	    DAYS_PER_YEAR_=365.242198781 ; DAYS_PER_MONTH_=30.436849898 # =(1/12)*365.242198781 : UDUNUITS-2
	    CALENDAR_="standard"
	    ;;
	"365"|"365_day"|"noleap")
	    DAYS_PER_YEAR_=365           ; DAYS_PER_MONTH_=30.416666667 # 365_day=noleap
	    CALENDAR_="${DAYS_PER_YEAR_}_day"
	    ;;
	"360"|"360_day"|"leap")
	    DAYS_PER_YEAR_=360           ; DAYS_PER_MONTH_=30           # 360_day
	    CALENDAR_="${DAYS_PER_YEAR_}_day"
	    ;;
	*)
	    echo " UNKNOWN calendar <<${calendar_wanted_}>>"
	    echo " S T O P   4"
	    exit 4
	    ;;
    esac
	

    #
    # *** If you change something here, also change the 'awk' computation below!!
    #
    ##DAYS_PER_YEAR_=366          ; DAYS_PER_MONTH_=30.5         # 366_day; Not used in PISM !!
    ##CALENDAR_="${DAYS_PER_YEAR_}_day"
    #DAYS_PER_YEAR_=365.242198781 ; DAYS_PER_MONTH_=30.436849898 # =(1/12)*365.242198781 : UDUNUITS-2
    #CALENDAR_="standard"
    #DAYS_PER_YEAR_=365           ; DAYS_PER_MONTH_=30.416666667 # 365_day=noleap
    #CALENDAR_="${DAYS_PER_YEAR_}_day"
    #DAYS_PER_YEAR_=360            ; DAYS_PER_MONTH_=30           # 360_day
    #CALENDAR_="${DAYS_PER_YEAR_}_day"
    #
    # *** If you change something here, also change the 'awk' computation below!!
    #

    CALENDAR_UNIT_="days"   ; OffTime_=$(echo "$OffsetYear_ * $DAYS_PER_YEAR_ " | bc -l )
    #CALENDAR_UNIT_="minutes"; OffTime_=$(echo "$OffsetYear_ * $DAYS_PER_YEAR_ * $MINUTES_PER_DAY_ " | bc -l )
    #CALENDAR_UNIT_="seconds"; OffTime_=$(echo "$OffsetYear_ * $DAYS_PER_YEAR_ * $SECONDS_PER_DAY_ " | bc -l )

    # DOES NOT WORK for calendar "standard"
    #CALENDAR_UNIT_="days"   ; OffTime_=$(( OffsetYear_ * DAYS_PER_YEAR_ ))
    #CALENDAR_UNIT_="minutes"; OffTime_=$(( OffsetYear_ * DAYS_PER_YEAR_ * MINUTES_PER_DAY_ ))
    #CALENDAR_UNIT_="seconds"; OffTime_=$(( OffsetYear_ * DAYS_PER_YEAR_ * SECONDS_PER_DAY_ ))


    time_=$(seq 0 ${NumberToGo0_} | awk "{if(NR==1){print \$1*$DAYS_PER_MONTH_+15.0+$OffTime_}else{printf(\",%i\\n\",\$1*$DAYS_PER_MONTH_+15.0+$OffTime_)}}")
    time_bnds_=$(seq 0 ${NumberToGo0_} | awk "{if(NR==1){print \$1*$DAYS_PER_MONTH_+$OffTime_\",\"(\$1+1)*$DAYS_PER_MONTH_+$OffTime_}else{print \",\"\$1*$DAYS_PER_MONTH_+$OffTime_\",\"(\$1+1)*$DAYS_PER_MONTH_+$OffTime_}}")


    cat > ${TIME_AXIS_FILE_}.cdl << EOF
netcdf time_axis {
dimensions:
	time = UNLIMITED ;
        t_nb2 = 2 ;
variables:
	//float time(time) ;
	//double time(time) ;
        ${number_type_} time(time) ;
                time:units = "${CALENDAR_UNIT_} since 0000-01-01 00:00:00" ;
		time:calendar = "${CALENDAR_}" ;
		time:long_name = "time" ;
		time:standard_name = "time" ;
		time:axis = "T" ;
                time:bounds = "time_bnds" ;
                time:comment =" Time axis constructed on $(date)" ;
	//double time_bnds(time, t_nb2) ;
	//double time_bnds(time, t_nb2) ;
        ${number_type_} time_bnds(time, t_nb2) ;
// global attributes:
		:Comments = "Generic build time axis" ;
		:Conventions = "CF-1.3" ;
		:Creators = "Christian Rodehacke" ;
		:History = "`date`: Created by $USER \n";
		:Title = "Generic time axis" ;

data:
        //For :calendar = "360_day" ;
        time = $time_ ;
	time_bnds = $time_bnds_ ;
	
}
EOF


#
## //For :calendar = "360_day" ;
## time = $(seq 0 ${NumberToGo0_} | awk "{if(NR==1){print \$1*30.0+15.0+$OffTime_}else{printf(\",%i\\n\",\$1*30.0+15.0+$OffTime_)}}") ;
## time_bnds = $(seq 0 ${NumberToGo0_} | awk "{if(NR==1){print \$1*30.0+$OffTime_\",\"(\$1+1)*30.0+$OffTime_}else{print \",\"\$1*30.0+$OffTime_\",\"(\$1+1)*30.0+$OffTime_}}") ;
## //For :calendar = "366_day" ;
## time = $(seq 0 ${NumberToGo0_} | awk "{if(NR==1){print \$1*30.5+15.0}else{printf(\",%i\\n\",\$1*30.5+15.0)}}") ;
## time_bnds = $(seq 0 ${NumberToGo0_} | awk "{if(NR==1){print \$1*30.5\",\"(\$1+1)*30.5}else{print \",\"\$1*30.5\",\"(\$1+1)*30.5}}") ;
## //For :calendar = "366_day" ;
## time = $(seq 0 ${NumberToGo0_} | awk "{if(NR==1){print \$1*30.5+15.0}else{printf(\",%i\\n\",\$1*30.5+15.0)}}") ;
## time = $(seq 0 ${NumberToGo0_} | awk "{if(NR==1){print \$1*30.5+15.0}else{print \",\"\$1*30.5+15.0}}") ;
#
#	//month: time = `seq -s\, 0 ${NumberToGo0_}` ;
#	//month: time_bnds = $(seq 0 ${NumberToGo0_} | awk "{if(NR==1){print \$1\",\"\$1+1}else{print \",\"\$1\",\"\$1+1}}") ;
#
#	//time = $(seq 0 ${NumberToGo0_} | awk "{if(NR==1){print \$1/\$STEPS_PER_YEAR}else{print \",\"\$1/\$STEPS_PER_YEAR}}") ;
#	//time_bnds = $(seq 0 ${NumberToGo0_} | awk "{if(NR==1){print \$1/\$STEPS_PER_YEAR\",\"(\$1+1)/\$STEPS_PER_YEAR}else{print \",\"\$1/\$STEPS_PER_YEAR\",\"(\$1+1)/\$STEPS_PER_YEAR}}") ;
#

    ncgen -o ${TIME_AXIS_FILE_} ${TIME_AXIS_FILE_}.cdl -k ${TYPE} && rm ${TIME_AXIS_FILE_}.cdl
}

# ========================================================================
#
# Main program
#
# ========================================================================

if [ ${DEBUG_FLAG} -gt 0 ] ; then
    echo
    echo "ELEMENTS_NO : ${ELEMENTS_NO}"
    echo "OUTPUT_NC   : ${OUTPUT_NC}"
    echo "OFFSET      : ${OFFSET:=0}"
    echo "CALENDAR    : ${CALENDAR}"
    echo "NUM_TYPE    : ${NUM_TYPE}"
fi

if [ -f ${OUTPUT_NC} ] ;then
    echo
    echo " OUTPUT_NC=${OUTPUT_NC} exits:  $(ls -l ${OUTPUT_NC})"
    echo " S T O P   2"
    exit 2
fi

DIR=$(dirname ${OUTPUT_NC})
if [ ! -d ${DIR} ] ; then
    echo
    echo " directory '${DIR}' of '${OUTPUT_NC}' does not exist"
    echo " S T O P   3"
    exit 3
fi

#
# in the function we compute dates from [0 to $ELEMENTS_NO] times TimeStepWith
# Hence we have to reduce $ELEMENTS_NO by one to actually get the requested
# number of elements
#
ELEMENTS_NO=$(( ELEMENTS_NO - 1 ))

CreateTimeAxisNC ${ELEMENTS_NO} ${OUTPUT_NC} ${OFFSET} ${CALENDAR} ${NUM_TYPE}


exit 0
# -- last line
