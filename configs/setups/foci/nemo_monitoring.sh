#!/bin/bash
# TODO: need to add SLURM header
#
#################################################################################
# default settings for the variables that can be changed via the command line
#
basedir=~/esm/esm-experiments/  # change via -p
EXP_ID="test_experiment"        # change via -r
envfile="$basedir/$EXP_ID/scripts/env.sh"  # change via -x
ncpus=24
use_singularity=true
#
#------- DO NOT EDIT BELOW THIS LINE UNLESS YOU KNOW WHAT YOU ARE DOING ------#
#
# Support log style output
print () { echo "$(date +'%F %T'):" "$@"; }
# always use -O with cdo
cdo () { command cdo -O "$@"; }

# Read the command line arguments
OPTIND=1         # Reset in case getopts has been used previously in the shell.
while getopts "h?d:r:n:p:x:" opt; do
    case "$opt" in
    h|\?)
        echo
        echo " Valid options are -h or -? for this help"
        echo "                   -d for more output (useful for debugging, not used at the moment)"
        echo "                   -p path to data                     (basedir,  default is $basedir)"
        echo "                   -r experiment / run id              (run,      default is $EXP_ID)"
        echo "                   -n number of CPUs to use            (ncpus,    default is $ncpus)"
        echo "                   -x full path to env.sh file         (envfile,  default is $HOME/esm/esm-experiments/\$EXP_ID/scripts/env.sh)"
        echo
        exit 0
        ;;
    d)  debug=1 # verbose mode
        ;;
    r)  EXP_ID=$OPTARG
        ;;
    n)  ncpus=$OPTARG
        ;;
    p)  basedir=$OPTARG
        ;;
    x)  envfile=$OPTARG
        ;;
    esac
done
shift $((OPTIND-1))
[ "$1" = "--" ] && shift

# update vars with command line options if set
envfile="$basedir/$EXP_ID/scripts/env.sh" 
export PBS_NP=${ncpus}

echo
print "NEMO monitoring and derived data calculation in"
print "$basedir for $EXP_ID"
print "Using environment file $envfile"
echo
if [[ ! -r $envfile ]] ; then
   echo
   print $envfile does not exist
   echo
   exit 1
else
   source $envfile
fi

EXP_DIR=${basedir}/${EXP_ID}
if [ ! -d ${EXP_DIR} ] ; then
	print "$EXP_DIR not found."
	exit 1
fi

if [ ! -d ${EXP_DIR}/outdata/nemo/ym ] ; then
	print "$EXP_DIR/outdata/nemo/ym not found."
	print "You need to run nemo_postprocessing.sh first."
	exit 1
fi

# setup basic directory
MONITORING_PATH=${EXP_DIR}/_MON
test -d ${MONITORING_PATH} || mkdir ${MONITORING_PATH}
cd ${MONITORING_PATH}

# generate lock file, check if we're already locked and exit if necessary
lock_file=${MONITORING_PATH}/monitoring_lock_${EXP_ID}.lock
[ -e "${lock_file}" ] && { echo ${lock_file} exists; exit 1; }

# if we're still running, create a lock file containing a nanosecond time stamp
# to be used later on
lock_time_stamp=`date -Ins`
echo ${lock_time_stamp} > ${lock_file}

# setup directories (in interactive mode this is done by Monitoring/scripts/monitoring_basic_setup.py
for d in model_data derived_data plots galleries tmp ; do
	mkdir -p ${d}/${EXP_ID}
done
mkdir -p ini

sw_bind=""
if [[ "$(hostname)" =~ "nesh" ]] ; then
   echo "`date` NOTE: This code runs on $(hostname)"
   # need to do this as /gxfs_work1/gxfs_home_interim/sw is a soft link to
   # /gxfs_work1/gxfs_home_interim/sw which singularity does not like as the 
   # soft link can't be resolved in the container
   sw_bind="--bind /gxfs_home/sw:/gxfs_work1/gxfs_home_interim/sw"
	foci_input2="/gxfs_work1/geomar/smomw235/foci_input2"
	# only used if use_singularity=false
	MINICONDA_HOME=~smomw235/miniconda3 
	module load nco
elif [[ "$(hostname)" =~ blogin* ]] || [[ "$(hostname)" =~ glogin* ]] ; then
   echo "`date` NOTE: This code runs on $(hostname)"
   sw_bind="--bind /sw:/sw"
	# on HLRN4 cdftools are linked to e.g. libcurl.so.4 which are not 
	# available in the default path in the container, luckily all the 
	# required shared libs are installed in our conda environment
	export LD_LIBRARY_PATH=/opt/conda/envs/monitoring/lib:$LD_LIBRARY_PATH
	foci_input2="/scratch/usr/shkifmsw/foci_input2"
	# only used if use_singularity=false
	MINICONDA_HOME=~shkifmsw/miniconda3 
else
   echo $(hostname) is untested.
   exit 1
fi

cat > ini/monitoring_${EXP_ID}.ini << EOF
[monitoring_configuration]
monitoring_path = ${MONITORING_PATH} 
drakkar_config = ORCA05
experiment_name = ${EXP_ID}
original_model_data_path = ${EXP_DIR}/outdata/nemo/ym
original_mesh_data_path = ${foci_input2}/NEMO_ORCA05/input/ORCA05/masks 
repeat_existing_diagnostics = False
repeat_existing_plots = False
exclude_freq_from_diag_and_plots = 1d,5d,1m,730h,5y,10y,20y
simple_mode = True
EOF

if $use_singularity ; then
	module load singularity
	# run monitoring from the singularity container
	# TODO: currently the .sif files is expected in the cwd, this is not the best solution
	if [[ ! -f mkexp-monitoring.sif ]] ; then
   	print "mkexp-monitoring.sif not available in $(pwd)"
	   print "Need to fetch singularity image from https://cloud.geomar.de/s/K8wiQPaacQcJ5LL/download/mkexp-monitoring.sif"
   	print "This only needs to be done once per simulation"
   	curl -O https://cloud.geomar.de/s/K8wiQPaacQcJ5LL/download/mkexp-monitoring.sif
	fi

	SINGULARITYENV_LD_LIBRARY_PATH=$LD_LIBRARY_PATH \
	SINGULARITYENV_PYTHONPATH=/usr/local/Monitoring \
		singularity exec --bind $WORK:$WORK --bind $HOME:$HOME \
		$sw_bind --bind ${IO_LIB_ROOT}/bin:/usr/local/bin \
		mkexp-monitoring.sif python \
		/usr/local/Monitoring/scripts/monitoring_parallel.py \
		${MONITORING_PATH}/ini/monitoring_${EXP_ID}.ini
else
	if [[ ! -d Monitoring ]]; then
		git clone -b develop-swahl git@git.geomar.de:TM/Monitoring.git Monitoring
	fi
	# activate the python environment
	source ${MINICONDA_HOME}/bin/activate monitoring
	python \
		${MONITORING_PATH}/Monitoring/scripts/monitoring_parallel.py \
		${MONITORING_PATH}/ini/monitoring_${EXP_ID}.ini
fi
monerror=$?
if [[ $monerror -gt 0 ]] ; then
	echo
	echo "Monitoring finished with error code $?, Try to continue anyways"
	echo
fi

# start concatenation
# settings for concatenation
# varlist="DRAKE_transports FLORIDA_BAHAMAS_transports moc icediags amoc_max_25.000N amoc_max_36.000N amoc_max_45.000N"
# complete list of derived data from Willi's monitoring
varlist="BERING_transports FRAM_transports BAFFIN_transports
DENMARK_STRAIT_transports ICELAND_SCOTLAND_transports CUBA_FLORIDA_transports
FLORIDA_BAHAMAS_transports DRAKE_transports AUS_AA_transports
ITF_transports MOZAMBIQUE_CHANNEL_transports SOUTH_AFR_transports
KERGUELEN_transports CAMPBELL_transports AFR_AUSTR_transports
AUSTR_AM_transports AM_AFR_transports DAVIS_transports
icediags moc psi speed
section_23W section_ACT section_DAVIS
section_OSNAP section_STAtlOMZ section_WoceA1E
section_WoceA1W section_WoceA24N section_WoceS04A
amoc_max_25.000N amoc_max_36.000N amoc_max_45.000N"

frequency='1y'
datadir=${MONITORING_PATH}/derived_data/${EXP_ID}
run=${EXP_ID}
targetdir="${EXP_DIR}/derived/nemo/"
mkdir -p $targetdir

for f in $frequency ; do
	for var in $varlist ; do
		if ls -1 $datadir/${run}*${f}*${var}.nc > /dev/null 2>&1 ; then
			firstfile=$(ls -1 $datadir/${run}*${f}*${var}.nc | head -1)
			lastfile=$(ls -1 $datadir/${run}*${f}*${var}.nc | tail -1)
			if [[ "$(basename $firstfile)" =~ ${run}_${f}_([0-9]+)_([0-9]+)_(.*).nc ]]; then
				startdate=${BASH_REMATCH[1]}
			fi	
			if [[ "$(basename $lastfile)" =~ ${run}_${f}_([0-9]+)_([0-9]+)_(.*).nc ]]; then
				enddate=${BASH_REMATCH[2]}
			fi	
			echo " Running ncrcat / cp for ${run}*${f}*${var}.nc --> ${targetdir}/${run}_${f}_${startdate}_${enddate}_${var}.nc"
			while (( $(jobs -p | wc -l) >= PBS_NP )); do sleep 5; done
			(
				rm -f ${targetdir}/${run}_${f}_*_${var}.nc
				ofile=''
				if [[ "$firstfile" == "$lastfile" ]]; then
					cp -pv $firstfile $targetdir/
					ofile=$firstfile
				else
					rm -fv ${targetdir}/${run}_${f}_*_${var}.nc
					ofile=${targetdir}/${run}_${f}_${startdate}_${enddate}_${var}.nc
					ncrcat -7 -o ${ofile} $datadir/${run}*${f}*${var}.nc
				fi

				# rewrite time axis, had problems using the original time axis with 
				# python (xarray)
				year=$(echo $startdate | cut -b -4)
			   [[ "$f" == "1y" ]] && tax="${year}-07-01,00:00:00,1y"
			   [[ "$f" == "5y" ]] && tax="${year}-07-01,00:00:00,5y"
			   [[ "$f" == "10y" ]] && tax="${year}-01-01,00:00:00,10y"
			   [[ "$f" == "20y" ]] && tax="${year}-01-01,00:00:00,20y"
				mv ${ofile} ${ofile}.tmp.nc 
            cdo -L -r -f nc4c settunits,days -settaxis,$tax \
                ${ofile}.tmp.nc ${ofile} && rm ${ofile}.tmp.nc 
			) &
		else
			echo " No file matching $datadir/${run}*${f}*${var}.nc available"
		fi
	done
done
wait
# check if existing lock file is ours
[ -e "${lock_file}" ] && [[ "${lock_time_stamp}" == "$(cat ${lock_file})" ]]     && rm -fv ${lock_file}     || { echo "${lock_file} not ours. Won't delete."; exit 1; }

print 'monitoring3 finished'
