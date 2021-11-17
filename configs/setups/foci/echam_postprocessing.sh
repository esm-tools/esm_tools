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

LAND_FILE_TAGS='jsbach veg surf yasso nitro land'
ATM_FILE_TAGS='echam co2 accw'

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
        #echo "                   -t filetype (nc or grb)             (fileext,   default is $fileext)"
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
set -x
#
###############################################################################
# END OF USER INTERFACE
###############################################################################
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

###############################################################################
#
# Component directories/names
atmmod=echam6
srfmod=jsbach

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
	 currdate=$(date --date="$currdate + 1 month" "+%Y%m%d")
done

# Computation of expected years for concatenated output
#iniyear=${inidate%????}
startyear=${startdate%????}
endyear=${enddate%????}
#[[ $startyear == $iniyear && $inidate != *0101 ]] && ((++startyear)) 
[[ $enddate != *1231 ]] && ((--endyear))

# Temporary directory
id="post"
post_dir=$DATA_DIR/${id}_$startdate-$enddate
if [[ -d $post_dir ]] ; then
    print "Hey: previous job failed or still running; $post_dir exists. Will stop now"
	 exit 1
fi
mkdir -p $post_dir

function rename_files {

  prefix=$1
  stamps_in=$2
  filetags_in=$3

  for stamp in $stamps_in ; do

    suffix=${stamp}${fileext}

    for filetag in $filetags_in ; do

        input=${EXP_ID}_${stamp}.${day}_${filetag}
        output=${prefix}_${filetag}_${suffix}

        trap 'echo $input:$? >> status' ERR
        if [[ -r $input ]] ; then
	        mv -v $input $output
           if [[ ! -r ${basedir}/log/${prefix}_${filetag}.codes ]] ; then
              cp ${input}.codes ${basedir}/${EXP_ID}/log/echam/${prefix}_${filetag}.codes 
           fi
           rm ${input}.codes
        elif [[ -r ${input}.nc ]] ; then
           mv -v ${input}.nc ${output}
        else
           if ! [[ -f $output ]] ; then
              echo "$output not available."
			  else
              echo "$output already available."
           fi
        fi
    done
  done
}
#
#  Postprocessing of ECHAM
#
print 'ECHAM post-processing started'

outmod=${DATA_DIR}/${atmmod}
fileext=.grb
[[ -d $outmod ]] || ln -s -r ${DATA_DIR}/echam $outmod
#mkdir ${outmod}
cd ${outmod} || exit 1

prefix=${EXP_ID}_${atmmod}

# Lists of files

meantags='BOT ATM'
filetags=${ATM_FILE_TAGS}
#TODO
#filetags=""
#meantags='BOT'

# Remove list
atm_files_to_remove=

# Generate monthly means, possibly apply afterburner transformations
echo 0 > $post_dir/status
rename_files $prefix "$stamps" "$filetags"
wait

remove_list=
for stamp in $stamps ; do

    suffix=${stamp}${fileext}

    for filetag in $meantags $filetags ; do

        input=${prefix}_${filetag}_${suffix}
        output=${prefix}_${filetag}_mm_${suffix}

        # If too many jobs run at the same time, wait
        while (( $(jobs -pr | wc -l) >=  max_jobs )); do sleep $sleep_time; done

        case $filetag/$mean_op in

            BOT/*)
                input=${prefix}_echam_${suffix}
                BOT_1_file=${post_dir}/${prefix}_BOT_1_mm_${suffix}
                BOT_2_file=${post_dir}/${prefix}_BOT_2_mm_${suffix}
                BOT_3_file=${post_dir}/${prefix}_BOT_3_mm_${suffix}
                (
                    trap 'echo $? > $post_dir/status' ERR
                    after $input $BOT_1_file << EOF
code =                             64, 65, 66, 67,
                               83, 84, 85, 86, 87, 88,
                       91, 92, 93, 94, 95, 96, 97,
                  100,101,102,103,104,105,106,107,108,109,
                  110,111,112,113,114,115,116,117,    119,
                  120,121,122,123,124,
                                  134,        137,    139,
                  140,141,142,143,144,145,146,147,
                  150,151,
                                  164,165,166,167,168,169,
                      171,            175,176,177,178,179,
                  180,181,182,    184,185,186,187,188,
                      191,192,193,            197,
                              203,204,205,        208,209,
                  210,211,    213,214,    216,
                                                      229,
                  230,231,    233,    235,
                  260
type = 20
level = 1
format = 1 
mean = 1 
EOF
                    # Post-process accw stream
                    cdo -f nc2 setmisstoc,0.0 $mean_op ${prefix}_accw_${suffix} \
                        $post_dir/${prefix}_BOT_2_mm_${stamp}.nc
                    cdo -f grb copy $post_dir/${prefix}_BOT_2_mm_${stamp}.nc \
                        $BOT_2_file
                    # Assemble final BOT file
                    cdo merge $BOT_1_file $BOT_2_file $output
                ) &
                ;;

            ATM/*)
                input=${prefix}_echam_${suffix}
                ATM_1_file=${post_dir}/${prefix}_ATM_1_mm_${suffix}
                ATM_2_file=${post_dir}/${prefix}_ATM_2_mm_${suffix}
                (
                    trap 'echo $? > $post_dir/status' ERR
                    after $input $ATM_1_file << EOF
code = 130, 131, 132, 133, 135, 153, 154, 156, 157, 223
level = 100000, 92500, 85000, 77500, 70000, 60000, 50000, 40000, 30000, 25000,
    20000, 15000, 10000, 7000, 5000, 3000, 2000, 1000, 700, 500, 300, 200, 100,
    50, 20, 10 
type = 30
format = 1 
mean = 1 

EOF
                    after $input $ATM_2_file << EOF
code = 138, 148, 149, 155
level = 100000, 92500, 85000, 77500, 70000, 60000, 50000, 40000, 30000, 25000,
    20000, 15000, 10000, 7000, 5000, 3000, 2000, 1000, 700, 500, 300, 200, 100,
    50, 20, 10 
type = 70
format = 1 
mean = 1 
EOF
                    cdo merge $ATM_1_file $ATM_2_file $output
                ) &
                ;;

            co2/*)
                (
                    trap 'echo $? > $post_dir/status' ERR
                    cdo $avg_op $input $output
                ) &
                [[ " $meantags " != *' co2 '* ]] && meantags="$meantags co2"
                ;;

            */?*)
                # with averaging, by default, files are left as they are
                ;;

            *)
                # w/o averaging, by default, files are left as they are
                ;;

        esac
    done # filetags

done # stamp in $stamps

wait
[[ $(<$post_dir/status) -eq 0 ]]

atm_files_to_remove="$atm_files_to_remove $remove_list"

print 'ECHAM post-processing finished'


#
# Concatenate monthly to yearly files
#

print 'ECHAM concatenation started'

echo 0 > $post_dir/status

remove_list=
for ((year=startyear; year<=endyear; ++year))
do

    for filetag in $(echo $meantags | sed 's/\>/_mm/g') $filetags
    do
        input=${prefix}_${filetag}_%s$fileext
        output=${prefix}_${filetag}_$year$fileext
        inputs=$(get_file_names $input $(for month in $(seq -f %02.0f 1 12); do echo ${year}${month}; done))

        # If too many jobs run at the same time, wait
        while (( $(jobs -pr | wc -l) >=  max_jobs )); do sleep $sleep_time; done
        (
            trap 'echo $? > $post_dir/status' ERR

            time_merge $output $inputs
        ) &
        remove_list="$remove_list $inputs"  
    done
done

wait
# netcdf conversion
for ((year=startyear; year<=endyear; ++year))
do

    for filetag in $(echo $meantags | sed 's/\>/_mm/g') 
    do
        output=${prefix}_${filetag}_$year$fileext

        # If too many jobs run at the same time, wait
        while (( $(jobs -pr | wc -l) >=  max_jobs )); do sleep $sleep_time; done
        (
            trap 'echo $? > $post_dir/status' ERR
				# netcdf conversion for mean files, set axis units in days, otherwise
				# python does not recognize the axis properly.
				[[ "$fileext" == ".grb" ]] && cdo -t echam6 -r -f nc4c -z zip_1 -settunits,days -settaxis,${year}-01-15,00:00:00,1months $output $(basename $output .grb).nc
        ) &
		  [[ "$fileext" == ".grb" ]] && remove_list="$remove_list $output" 
    done
done


wait

for ((year=startyear; year<=endyear; ++year))
do
	# Add szip compression
	for filetag in $filetags
	do
		file=${prefix}_${filetag}_$year${fileext}
		while (( $(jobs -p | wc -l) >=  max_jobs )); do sleep $sleep_time; done
		(
			trap 'echo $? > $post_dir/status' ERR
			cdo -szip $file ${file}.szip
		) &
		remove_list="$remove_list $file"
	done
done

wait
[[ $(<$post_dir/status) -eq 0 ]]

atm_files_to_remove="$atm_files_to_remove $remove_list"

print 'ECHAM concatenation finished'


#
# Postprocessing of JSBACH
#

print 'JSBACH post-processing started'

outmod=${DATA_DIR}/${srfmod}
fileext=.grb

mkdir ${outmod}
cd ${outmod}

prefix=${EXP_ID}_${srfmod}

filetags=${LAND_FILE_TAGS}
function file_name {
    typeset tag=$1
    typeset stamp=$2
    echo ${prefix}_${tag}_$stamp$fileext
}

# Remove list

srf_files_to_remove=

echo 0 > $post_dir/status

rename_files $prefix "$stamps" "$filetags"
wait

remove_list=
for stamp in $stamps
do

    for filetag in $filetags
    do
        input=${prefix}_${filetag}_$stamp$fileext
        output=${prefix}_${filetag}_mm_$stamp$fileext

        # If too many jobs run at the same time, wait
        while (( $(jobs -p | wc -l) >=  max_jobs )); do sleep $sleep_time; done
        (
            trap 'echo $? > $post_dir/status' ERR

            case $filetag/$avg_op in

                land/*) 
                    # Generate monthly mean FaPAR from land stream (code 125)
                    cdo -aexpr,var125=var148/var149 $avg_op $input $output ;;

                jsbach/*)
                    # Generate monthly mean Albedo from jsbach stream (code 13)
                    # Generate monthly mean FaPAR from land stream (code 125)
                    input2=${prefix}_land_$stamp$fileext
                    cdo merge -aexpr,var13=var22/var21 $avg_op $input \
                              -expr,var125=var148/var149 $avg_op $input2 \
                              $output ;;
	             
                */?*)
                    cdo $avg_op $input $output ;;

                *)
                    ln $input $output ;;

            esac
        ) &
        remove_list="$remove_list $input"  
    done

done # stamps

wait
[[ $(<$post_dir/status) -eq 0 ]]

srf_files_to_remove="$srf_files_to_remove $remove_list"

print 'JSBACH post-processing finished'


#
# Concatenate monthly to yearly files
#

print 'JSBACH concatenation started'

echo 0 > $post_dir/status

remove_list=
for ((year=startyear; year<=endyear; ++year))
do

    for filetag in $filetags
    do
        input=${prefix}_${filetag}_mm_%s$fileext
        output=${prefix}_${filetag}_mm_$year$fileext
        inputs=$(get_file_names $input $(for month in $(seq -f %02.0f 1 12); do echo ${year}${month}; done))

        # If too many jobs run at the same time, wait
        while (( $(jobs -p | wc -l) >=  max_jobs )); do sleep $sleep_time; done
        (
            trap 'echo $? > $post_dir/status' ERR

            time_merge $output $inputs
        ) &
        remove_list="$remove_list $inputs"  
    done
done

wait

[[ $(<$post_dir/status) -eq 0 ]]

srf_files_to_remove="$srf_files_to_remove $remove_list"

print 'JSBACH concatenation finished'
#
# store some static data to fx/
#
if [[ ! -f $DATA_DIR/fx/${EXP_ID}_${atmmod}_fx.nc ]]; then

	test -d $DATA_DIR/fx || mkdir -p $DATA_DIR/fx
	cd $DATA_DIR/fx
	
	# select slm, slf from ECHAM6 output, calculated gridareas and add them
	cdo -r -f nc4c -t echam6 -gridweights $DATA_DIR/$atmmod/${EXP_ID}_${atmmod}_co2_${startyear}${fileext}.szip weight_$startyear.nc
	cdo -r -f nc4c -t echam6 -gridarea $DATA_DIR/$atmmod/${EXP_ID}_${atmmod}_co2_${startyear}${fileext}.szip area_$startyear.nc
	cdo -r -f nc4c -t echam6 -selname,slm,slf -seltimestep,1 $DATA_DIR/$atmmod/${EXP_ID}_${atmmod}_echam_${startyear}${fileext}.szip slm_slf_$startyear.nc
	cdo merge *_${startyear}.nc ${EXP_ID}_${atmmod}_fx.nc && rm *_${startyear}.nc
	print 'ECHAM6 storing of static data finished'
fi	

#
# Epilogue
#
# Clean up
print 'removal of temporary and non-precious data files started'
rm -r $post_dir
( cd $DATA_DIR/$atmmod && rm $atm_files_to_remove )
( cd $DATA_DIR/$srfmod && rm $srf_files_to_remove )
print 'removal of temporary and non-precious data files finished'
#
print "post-processing finished for $startdate-$enddate"

