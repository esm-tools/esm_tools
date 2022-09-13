#!/bin/bash
#
# Postprocessing for FOCI within ESM-Tools
# based on the Postprocessing from the old mkexp based runtime environment
# Sebastian Wahl 06/2021
#
# Modified for OpenIFS by Joakim Kjellsson
#

#################################################################################
# default settings for the variables that can be changed via the command line
# 
basedir=~/esm/esm-experiments/ # change via -p
EXP_ID="test_experiment"        # change via -r
startyear=1850                  # change via -s
endyear=1850                    # change via -e
envfile="$basedir/$EXP_ID/scripts/env.sh"  # change via -x
run_monitoring="no"

module load nco || module load NCO

ATM_CHECK_NETCDF4=false
# set to false to skip netcdf4 conversion, time consuming but reduces file size by at least 50%
ATM_CONVERT_NETCDF4=true 
ATM_FILE_TAGS="regular_sfc regular_pv regular_pl regular_ml reduced_sfc reduced_pv reduced_pl reduced_ml"

# Other settings
day="01"
max_jobs=12
#
###############################################################################
# END OF USER INTERFACE
###############################################################################
#
#
#------- DO NOT EDIT BELOW THIS LINE UNLESS YOU KNOW WHAT YOU ARE DOING ------#
#
# Read the command line arguments
OPTIND=1         # Reset in case getopts has been used previously in the shell.
while getopts "h?md:r:s:e:p:x" opt; do
    case "$opt" in
    h|\?)
        echo
        echo " Valid options are -h or -? for this help"
        echo "                   -d for more output (useful for debugging, not used at the moment)"
        echo "                   -p path to data                     (basedir,  default is $basedir)"
        echo "                   -r experiment / run id              (run,       default is $EXP_ID)"
        echo "                   -s startyear                        (startyear, default is $startyear)"
        echo "                   -e endyear                          (endyear,   default is $endyear)"
        echo "                   -x full path to env.sh file         (envfile,   default is $HOME/esm/esm-experiments/\$EXP_ID/scripts/env.sh)"
        echo "                   -m run oifs_monitoring.sh           "
        echo
        exit 0
        ;;
    d)  debug=1 # verbose mode
        ;;
    r)  EXP_ID=$OPTARG
        ;;
    s)  startyear=$OPTARG
        ;;
    e)  endyear=$OPTARG
        ;;
    p)  basedir=$OPTARG
        ;;
    x)  envfile=$OPTARG
        ;;
    m)  run_monitoring="yes"
        ;;
    esac
done

shift $((OPTIND-1))
[ "$1" = "--" ] && shift
envfile="$basedir/$EXP_ID/scripts/env.sh"
echo
echo "Doing postprocessing in $basedir for $EXP_ID from year $startyear to $endyear"
echo "Using an environment from $envfile"
echo
if [[ ! -r $envfile ]] ; then
   echo
	echo $envfile does not exist
	echo
	exit 1
else
  # module purge in envfile writes non-printable chars to log
   source $envfile | tee
fi
#
# the ncks option -a is deprecated since version 4.7.1 and replaced by --no-alphabetize
sortoption="--no-alphabetize"
[[ $(ncks --version 2>&1 | tail -1 | tr -dc '0-9') -lt 471 ]] && sortoption="-a"

# Support log style output
print () { echo "$(date +'%F %T'):" "$@"; }
print_re='^[0-9]+-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}'

# Bail out on error
trap 'print Error at line $BASH_LINENO >&2' ERR
set -e
#
###############################################################################
#
# some derived variables that directly depend on the command line arguments...
startdate=${startyear}0101              # 
nextdate=$((endyear + 1))0101
DATA_DIR=${basedir}/${EXP_ID}/outdata
RESTART_DIR=${basedir}/${EXP_ID}/restart
#inidate=18500101 # not needed anymore
debug=0

# Component directories/names
atmmod=oifs

#
# Default options for Unix commands
mkdir () { command mkdir -vp "$@"; }
rm () { command rm -vf "$@"; }
ln () { command ln -vf "$@"; }

cdo () { command cdo -O "$@"; }
after () { command cdo after "$@"; }

#
# Utilities
#

function get_file_names {
    typeset pattern="$1"
    shift
    echo $(printf " $pattern" "$@")
}

function time_merge {
    typeset out="$1"
    typeset tmp=$(dirname $out)/.$(basename $out)
    shift
    cat "$@" > $tmp && mv $tmp $out
}

#
# Job specification
#

mean_op='-monmean'; avg_op='-monavg'

sleep_time=2

# Definition of some time variables
#
# enddate:     last day of this run
enddate=$(date --date="$nextdate - 1 day" "+%Y%m%d")

#
# DATA PROCESSING
#

print "post-processing started for $startdate-$enddate"

# Check time range

if [[ $startdate != *01 ]]
then
    print "Oops: invalid start date; currently only first of month is supported"
    exit 1
fi

# Computation of expected input time stamps
startstamp=${startdate%??}
laststamp=
stamps=

currdate=$startdate
while [[ $currdate -le $enddate ]] ; do
    laststamp=${currdate%??}
    stamps="$stamps $laststamp"
    #currdate=$(calc_date plus -M 1 $currdate)
	 # 18930401 does not exist for the date function and leads to an error
	 [[ "$currdate" == "18930401" ]] && currdate="18930331"
	 currdate=$(date --date="$currdate + 1 month" "+%Y%m%d")
done

# Computation of expected years for concatenated output
#iniyear=${inidate%????}
startyear=${startdate%????}
endyear=${enddate%????}
#[[ $startyear == $iniyear && $inidate != *0101 ]] && ((++startyear)) 
[[ $enddate != *1231 ]] && ((--endyear))

# Temporary directory
id=$$
post_dir=$DATA_DIR/${id}_$startdate-$enddate
[[ -d $post_dir ]] &&
    print "Hey: previous job failed or still running; removing temp dir"
rm -r $post_dir
mkdir $post_dir

#
# Convert OpenIFS/XIOS netcdf3 output to netcdf4 using the chunking algorithm
# developed by Willi Rath, GEOMAR (convert_to_deflated_nc4classic_with_small_chunks.sh)
# Converts any OpenIFS netCDF file to deflated (level 1) netCDF4-classic with
# (1,1,100,100)=(t,plev,lat,lon) chunks.
# needs ncdump and a recent (4.3.7 or newer) nco
#
print 'OpenIFS netcdf4 conversion started'
outmod=${DATA_DIR}/${atmmod}

mkdir ${outmod}
cd ${outmod}
mkdir nc3

filetags="${ATM_FILE_TAGS}"

#steps="${EXP_ID}_1d ${EXP_ID}_5d ${EXP_ID}_1m ${EXP_ID}_1y 1_${EXP_ID}_1d 1_${EXP_ID}_5d 1_${EXP_ID}_1m 1_${EXP_ID}_1y"
steps="1ts 1h 3h 6h 1d 1m 1y"
tchunk=1; zchunk=1; ychunk=100; xchunk=100

echo 0 > $post_dir/status
if ${ATM_CONVERT_NETCDF4} ; then
	for ((year=startyear; year<=endyear; ++year))
	do
		for filetag in $filetags
		do
   		for s in $steps
      	        do
                        # Name of output from model (always ECE3...)
                        input_orig=ECE3_${s}_${year}0101_${filetag}.nc
                        # Rename to this
			input=${EXP_ID}_${s}_${year}0101_${year}1231_${filetag}.nc3
                        # Name of final compressed file
		    	output=${EXP_ID}_${s}_${year}0101_${year}1231_${filetag}.nc
                        			
      	  	        if [[ -f $input_orig ]] ; then
			    mv $input_orig $input
                            # If too many jobs run at the same time, wait
			    while (( $(jobs -p | wc -l) >=  max_jobs )); do sleep $sleep_time; done
			    (
			    trap 'echo $? > $post_dir/status' ERR
			    print "converting " $input " to " $output
			    if [[ ! -f $output ]]; then
			        ncks -7 $sortoption -L 1 \
				--cnk_dmn time,${tchunk} --cnk_dmn time_counter,${tchunk} \
				--cnk_dmn z,${zchunk} --cnk_dmn depthu,${zchunk} \
                                --cnk_dmn depthv,${zchunk} --cnk_dmn depthw,${zchunk} --cnk_dmn deptht,${zchunk} \
				--cnk_dmn x,${xchunk} --cnk_dmn y,${ychunk} \
				$input $output 
				if [[ $? -eq 0 ]]; then
				    print "Conversion of $input to $output OK, now checking file with cdo diff"
				    ${ATM_CHECK_NETCDF4} && cdo -s diff $input $output > ${output}.check
                                    if ${ATM_CHECK_NETCDF4} && [[ $? -eq 0 ]] && [[ $(wc -l ${output}.check | awk '{print$1}') -eq 0 ]]; then
                                        print "cdo diff $input $output OK"
					rm -f ${output}.check
					mv -v $input nc3/
					rm nc3/$input
                                     else
					if ${ATM_CHECK_NETCDF4} ; then
                                            echo "ERROR: $input and $output differ"
					    mv -v $input nc3/                    
					else
					    echo "cdo diff check switched off (ATM_CHECK_NETCDF4 = ${ATM_CHECK_NETCDF4})"
    	                                    mv -v $input nc3/
					    rm nc3/$input
					fi
				    fi
				else
				    echo "ERROR during conversion of $input to $output"
				    mv -v $input nc3/                    
				fi
			    else
				echo "ERROR: $output exists. This should not happen."
				mv -v $input nc3/                    
			    fi
			    ) &
			fi
		done #steps
		done #filetags
	done #years
fi 
wait
[[ $(<$post_dir/status) -eq 0 ]]

print 'OpenIFS netcdf4 conversion finished'

#
# Calculate yearly means from nemo output and place in ym/ subdirectory 
#

print 'OpenIFS ym calculation started'

# Use only daily and monthly output
steps="1d 1m"
cd ${outmod}

echo 0 > $post_dir/status
mkdir ym

if ${ATM_CONVERT_NETCDF4} ; then
    for ((year=startyear; year<=endyear; ++year))
    do
	for filetag in $filetags
	do
		for s in $steps
		do
			# !!! output files will have the same name as the old input file !!! 
			input=${EXP_ID}_${s}_${year}0101_${year}1231_${filetag}.nc
			output=${EXP_ID}_1y_${year}0101_${year}1231_${filetag}.nc

			if [[ -f $input ]] && [[ ! -f ym/$output ]] && [[ ! -f ym/${output}3 ]]; then

				touch ym/$output 
				# If too many jobs run at the same time, wait
				while (( $(jobs -p | wc -l) >=  max_jobs )); do sleep $sleep_time; done
				(	
					trap 'echo $? > $post_dir/status' ERR
					cdo yearmean $input ym/$output 
					cd ym
					mv $output ${output}3
					ncks -7 $sortoption -L 1 \
						--cnk_dmn time,${tchunk} --cnk_dmn time_counter,${tchunk} \
						--cnk_dmn z,${zchunk} --cnk_dmn depthu,${zchunk} --cnk_dmn depthv,${zchunk} --cnk_dmn depthw,${zchunk} --cnk_dmn deptht,${zchunk} \
						--cnk_dmn x,${xchunk} --cnk_dmn y,${ychunk} \
					${output}3 $output
					# only simple checking with ym as the files can be 
					# reproduced in case of an error
					[[ $? -eq 0 ]] && rm -v ${output}3 
					cd ..
				) &
				# file from cdo must be available for [[ ! -f ym/$output ]] in case of e.g. 5d and 1m output
				sleep 5
			fi
		done #steps
	done
   wait
done

wait
[[ $(<$post_dir/status) -eq 0 ]]

print 'OpenIFS ym calculation finished'

fi 

#
# put OpenIFS restart files from previous year in one tar file
#


#print 'OpenIFS restart postprocessing started'
#cd ${RESTART_DIR}/${atmmod}
#
#for ((year=startyear-1; year<endyear; ++year)) ; do
#	if ls ${year}1231 > /dev/null 2>&1 ; then
#		# If too many jobs run at the same time, wait
#		while (( $(jobs -p | wc -l) >=  max_jobs )); do sleep $sleep_time; done
#		(
#			trap 'echo $? > $post_dir/status' ERR
#			print "Processing year $year"
#			tar czf ${EXP_ID}_restart_${year}1231.tar.gz *${EXP_ID}_*_restart*_${year}1231_*.nc 
#			[[ $? -eq 0 ]] && rm *${EXP_ID}_*_restart*_${year}1231_*.nc 
#		) &
#	fi
#	wait
#done
#wait

#[[ $(<$post_dir/status) -eq 0 ]]
#print 'NEMO restart postprocessing finished'
#

# Make mask and cell size info 
# * If we have _fx_ output, then take lsm and other stuff
# * Use cdo gridarea to generate areacella 
cd ${DATA_DIR}/${atmmod}
for ((year=startyear-1; year<endyear; ++year)) ; do 
    if [[ -f "ECE3_fx_${year}0101_regular_sfc.nc" ]] ; then
        if [[ ! -f "areacella.nc" ]] ; then
            # Get grid cell area
            cdo -chname,cell_area,areacella -gridarea ECE3_fx_${year}0101_regular_sfc.nc areacella.nc
        fi 
        if [[ ! -f "lsm.nc" ]] ; then
            # Get lsm
            cdo -aexpr,'land_mask=lsm+cl' -select,name=lsm,cl,al,sz ECE3_fx_${year}0101_regular_sfc.nc lsm.nc 
        fi
    fi
    
    if [[ -f "ECE3_fx_${year}0101_reduced_sfc.nc" ]] ; then
        if [[ !-f "areacella_reduced.nc" ]] ; then
            # Get grid cell area
            cdo -chname,cell_area,areacella -gridarea ECE3_fx_${year}0101_reduced_sfc.nc areacella_reduced.nc
        fi
	if [[ !-f "lsm_reduced.nc" ]] ; then
            # Get lsm
	    cdo -aexpr,'land_mask=lsm+cl' -select,name=lsm,cl,al,sz ECE3_fx_${year}0101_reduced_sfc.nc lsm_reduced.nc
	fi
    fi
done

if [[ -f "areacella.nc" ]] && [[ -f "lsm.nc" ]] ; then
    rm *_fx_*_regular_sfc.nc 
fi
if [[ -f "areacella_reduced.nc"]] && [[ -f "lsm_reduced.nc" ]] ; then
    rm *_fx_*_reduced_sfc.nc
fi

#
# Epilogue
#

# Clean up

print 'removal of temporary and non-precious data files started'
rm -r $post_dir
print 'removal of temporary and non-precious data files finished'

print "post-processing finished for $startdate-$enddate"

#if [[ "$run_monitoring" == "yes" ]] ; then
#    print "will now run NEMO monitoring for $startdate-$enddate"
#   $(dirname $0)/nemo_monitoring.sh -r ${EXP_ID} 
#else
#   print "OpenIFS monitoring switched off, use -m to activate it"
#fi
