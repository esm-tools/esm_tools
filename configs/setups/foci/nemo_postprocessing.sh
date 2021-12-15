#!/bin/bash
#
# Postprocessing for FOCI within ESM-Tools
# based on the Postprocessing from the old mkexp based runtime environment
# Sebastian Wahl 06/2021
#

#################################################################################
# default settings for the variables that can be changed via the command line
# 
basedir=~/esm/esm-experiments/ # change via -p
EXP_ID="test_experiment"        # change via -r
startyear=1850                  # change via -s
endyear=1850                    # change via -e
envfile="$basedir/$EXP_ID/scripts/env.sh"  # change via -x

module load nco || module load NCO

OCEAN_CHECK_NETCDF4=false
# set to false to skip netcdf4 conversion, time consuming but reduces file size by at least 50%
OCEAN_CONVERT_NETCDF4=true 
OCEAN_FILE_TAGS="grid_T grid_U grid_V grid_W icemod ptrc_T"

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
while getopts "h?d:r:s:e:p:x:" opt; do
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
	source $envfile
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
ocemod=nemo

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
# Convert NEMO netcdf3 output to netcdf4 using the chunking algorithm
# developed by Willi Rath, GEOMAR (convert_to_deflated_nc4classic_with_small_chunks.sh)
# Converts any NEMO netCDF file to deflated (level 1) netCDF4-classic with
# (1,1,100,100)=(t,z,y,x) chunks.
# needs ncdump and a recent (4.3.7 or newer) nco
#
print 'NEMO netcdf4 conversion started'
outmod=${DATA_DIR}/${ocemod}

# reduce number of jobs to avoid memory issues
max_jobs=4

mkdir ${outmod}
cd ${outmod}
mkdir nc3

filetags="${OCEAN_FILE_TAGS}"
steps="${EXP_ID}_1d ${EXP_ID}_5d ${EXP_ID}_1m ${EXP_ID}_1y 1_${EXP_ID}_1d 1_${EXP_ID}_5d 1_${EXP_ID}_1m 1_${EXP_ID}_1y"
tchunk=1; zchunk=1; ychunk=100; xchunk=100

echo 0 > $post_dir/status
if ${OCEAN_CONVERT_NETCDF4} ; then
	for ((year=startyear; year<=endyear; ++year))
	do
		for filetag in $filetags
		do
   		for s in $steps
      	do
				input=${s}_${year}0101_${year}1231_${filetag}.nc3
		    	output=${s}_${year}0101_${year}1231_${filetag}.nc
				# !!! output files will have the same name as the old input file !!! 
      	  	if [[ -f $output ]] ; then
					mv $output $input
               
					# If too many jobs run at the same time, wait
					while (( $(jobs -p | wc -l) >=  max_jobs )); do sleep $sleep_time; done
					(
						trap 'echo $? > $post_dir/status' ERR
						print "converting " $input " to " $output
						if [[ ! -f $output ]]; then
							ncks -7 $sortoption -L 1 \
								--cnk_dmn time,${tchunk} --cnk_dmn time_counter,${tchunk} \
								--cnk_dmn z,${zchunk} --cnk_dmn depthu,${zchunk} --cnk_dmn depthv,${zchunk} --cnk_dmn depthw,${zchunk} --cnk_dmn deptht,${zchunk} \
								--cnk_dmn x,${xchunk} --cnk_dmn y,${ychunk} \
							$input $output 
							if [[ $? -eq 0 ]]; then
								print "Conversion of $input to $output OK, now checking file with cdo diff"
								${OCEAN_CHECK_NETCDF4} && cdo -s diff $input $output > ${output}.check
								if ${OCEAN_CHECK_NETCDF4} && [[ $? -eq 0 ]] && [[ $(wc -l ${output}.check | awk '{print$1}') -eq 0 ]]; then
									print "cdo diff $input $output OK"
									rm -f ${output}.check
									mv -v $input nc3/
									rm nc3/$input
								else
									if ${OCEAN_CHECK_NETCDF4} ; then
										echo "ERROR: $input and $output differ"
										mv -v $input nc3/                    
									else
										echo "cdo diff check switched off (OCEAN_CHECK_NETCDF4 = ${OCEAN_CHECK_NETCDF4})"
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

print 'NEMO netcdf4 conversion finished'

#
# Calculate yearly means from nemo output and place in ym/ subdirectory 
#

print 'NEMO ym calculation started'

steps="${EXP_ID}_1m ${EXP_ID}_5d ${EXP_ID}_1d 1_${EXP_ID}_1m 1_${EXP_ID}_5d 1_${EXP_ID}_1d"
cd ${outmod}

echo 0 > $post_dir/status
mkdir ym

for ((year=startyear; year<=endyear; ++year))
do
	for filetag in $filetags
	do
		for s in $steps
		do
			# !!! output files will have the same name as the old input file !!! 
			input=${s}_${year}0101_${year}1231_${filetag}.nc
			if [[ "$s" =~ ^1_.*$ ]] ; then
				output=1_${EXP_ID}_1y_${year}0101_${year}1231_${filetag}.nc
			else
				output=${EXP_ID}_1y_${year}0101_${year}1231_${filetag}.nc
			fi     
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

print 'NEMO ym calculation finished'
#
# put NEMO restart files from previous year in one tar file
#
print 'NEMO restart postprocessing started'
cd ${RESTART_DIR}/${ocemod}

for ((year=startyear-1; year<endyear; ++year)) ; do
	if ls *${EXP_ID}_*_restart*_${year}1231_*.nc > /dev/null 2>&1 ; then
		# If too many jobs run at the same time, wait
		while (( $(jobs -p | wc -l) >=  max_jobs )); do sleep $sleep_time; done
		(
			trap 'echo $? > $post_dir/status' ERR
			print "Processing year $year"
			tar czf ${EXP_ID}_restart_${year}1231.tar.gz *${EXP_ID}_*_restart*_${year}1231_*.nc 
			[[ $? -eq 0 ]] && rm *${EXP_ID}_*_restart*_${year}1231_*.nc 
		) &
	fi
	wait
done
wait

[[ $(<$post_dir/status) -eq 0 ]]
print 'NEMO restart postprocessing finished'
#
# Store land sea mask information separately
# merge mesh_mask.nc files 
# TODO: nocscombine needs to be compiled with NEMO 
cd ${DATA_DIR}/${ocemod}
if [[ $(type -P nocscombinev2.x) ]] && [[ ! -f mesh_mask.nc ]] && [[ -f mesh_mask_0000.nc ]]; then
	nocscombinev2.x -f mesh_mask_0000.nc 
	if [[ $? -eq 0 ]] && [[ -f mesh_mask.nc ]]; then
		print "nocscombinev2.x finished sucessfully"
		rm mesh_mask_????.nc
	   test -d $DATA_DIR/fx || mkdir -p $DATA_DIR/fx
		mv mesh_mask.nc $DATA_DIR/fx/
	fi
else
	print "nocscombinev2.x is missing or"
	print "mesh_mask.nc already available or"
	print "mesh_mask_0000.nc missing"
fi
print 'NEMO storing of static data finished'
#
# Epilogue
#

# Clean up

print 'removal of temporary and non-precious data files started'
rm -r $post_dir
print 'removal of temporary and non-precious data files finished'

print "post-processing finished for $startdate-$enddate"

