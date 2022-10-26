#!/bin/bash
# 
# Wrapper script to run notebook with command line arguments
# in the shell. As papermill can handle yaml arguments as well
# we may just read the finished config yaml at some point
# SebastianWahl 05/2022
# 
echo "`date`: ECHAM6 monitoring started"
#
basedir="$HOME/esm/esm-experiments/"
obsroot="$HOME/foci_input2/OBS_MONITORING/T63/"
expid="test"
iniyear=1850
condapath="$HOME/miniconda3/"
#
#------- DO NOT EDIT BELOW THIS LINE UNLESS YOU KNOW WHAT YOU ARE DOING ------#
#
# Read the command line arguments
OPTIND=1         # Reset in case getopts has been used previously in the shell.
while getopts "h?:r:i:p:c:o:" opt; do
    case "$opt" in
    h|\?)
        echo
        echo " Valid options are -h or -? for this help"
        echo "                   -p path to data             (basedir,      default is $basedir)"
        echo "                   -o path to obs data         (basedir,      default is $obsroot)"
        echo "                   -r experiment / run id      (run,          default is $expid)"
        echo "                   -i initial year             (initial year, default is $iniyear)"
        echo "                   -c root path to conda env   (condapath,    default is \$HOME/miniconda3/)"
        echo
        exit 0
        ;;
    r)  expid=$OPTARG
        ;;
    i)  iniyear=$OPTARG
        ;;
    p)  basedir=$OPTARG
        ;;
    o)  obsroot=$OPTARG
        ;;
    c)  condapath=$OPTARG
        ;;
    esac
done

shift $((OPTIND-1))
[ "$1" = "--" ] && shift
echo
echo "Doing ECHAM6 monitoring in $basedir for $expid from year $iniyear onwards"
echo "Using conda environment from $condapath"
echo
if ! source $condapath/bin/activate jupyter_mon ; then
   echo
	echo "source $condapath/bin/activate jupyter_mon failed" 
	echo "install with"
	echo "   conda env create -n jupyter_mon --file $(dirname $0)/jupyter_mon.yaml"
	echo "   source $condapath/bin/activate jupyter_mon" 
   echo '   python -m ipykernel install --user --name jupyter_mon --display-name "jupyter_mon"'
	echo
	exit 1
else
	source $condapath/bin/activate jupyter_mon 
fi

cd $(dirname $0)
papermill echam_monitoring.ipynb echam_monitoring_${expid}.ipynb -p expid $expid -p iniyear $iniyear -p exproot $basedir -p obsroot $obsroot
jupyter-nbconvert --no-input --to html echam_monitoring_${expid}.ipynb
mv -v *.html $basedir/$expid/mon/echam/

echo "`date`: ECHAM6 monitoring finished"
exit 0
