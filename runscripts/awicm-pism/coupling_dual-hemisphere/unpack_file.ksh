#!/bin/ksh
#------------------------------------------------------------------------------
# Script to unpack all variables of a jsbach restart file.
#
# Veronika Gayler, June  2013
#------------------------------------------------------------------------------
set -e

restart=$1                 # file with packed data
maskfile=$2                # file with corresponding land sea mask 
# maskfile=/pool/data/JSBACH/T63/jsbach_T63GR15_11tiles_1850.nc
maskname=slm               # variable name of the land sea mask in maskfile 

#------------------------------------------------------------------------------
. ${MODULESHOME}/init/ksh
module load nco || true
module unload cdo || true
if [[ $(uname -n | cut -c1-6) == mlogin || $(uname -n | cut -c1-9) == mistralpp || $(uname -n) == m????? ]]; then
    module load cdo/1.7.2-magicsxx-gcc48
    export LD_LIBRARY_PATH="/sw/rhel6-x64/hdf5/hdf5-1.8.14-threadsafe-gcc48/lib:/sw/rhel6-x64/netcdf/netcdf_c-4.4.0-gcc48/lib:/sw/rhel6-x64/sys/libaec-0.3.2-gcc48/lib"
else
    #module load cdo/1.7.2-gccsys
    module load cdo/1.7.2
fi
#------------------------------------------------------------------------------

if [[ ${restart} = "" ]]; then
  echo " ERROR: "
  echo " you need to define the name of the file with packed data"
  echo " and a file containing the corresponding land sea mask"
  exit 1
fi

restartfile=${restart##*/}
[[ ${restartfile} = ${restart} ]] && restart=./${restart}
mydir=$(pwd)
restartdir=${restart%/*}; cd ${restartdir}; restartdir=$(pwd)
cd ${mydir} 
scriptdir=${0%/*}; cd ${scriptdir}/..; scriptdir=$(pwd)
script=$(basename $0)

if [[ ! -f ./unpack ]]; then
 echo "  ERROR: "
 echo "   The program 'unpack' is missing."
 echo "   Compile <cosmos>/contrib/unpack.f90 and place the executable in this "
 echo "   directory. Instructions are given in the program header."
 exit 1
fi

# data processing
#-----------------

cd ${restartdir}
[[ -f 2d_${restartfile} ]] && rm 2d_${restartfile}
cdo -s -selvar,${maskname} ${maskfile} ${maskname}.nc 

variables=$(cdo -s showvar ${restartfile})
for var in ${variables}; do
    {
    echo "${script}: ${var}"
    ncks -O -v ${var} ${restartfile} ${var}.nc
    case ${var} in
	landseamask | disch | awfre )
	    echo "${script}:   copying ${var}: already on lat lon grid"
	    ln -fs ${var}.nc 2d_${var}.nc
	    ;;
	vct_a | vct_b )
	    echo "${script}:   copying ${var}: no landpoint dimension"
	    ln -fs ${var}.nc 2d_${var}.nc
	    ;;
	*)
	    ${scriptdir}/unpack <<EOF
                ${var}
                ${maskname}
EOF
	    ;;
    esac
    } &
done
echo "Done submitting jobs, waiting"
wait
echo "Done with the variable loop"

for var in ${variables}; do
    ncks -h -A -v ${var} 2d_${var}.nc 2d_${restartfile}
    rm ${var}.nc 2d_${var}.nc
done

# add global attributes
#ncks -M ${restartfile} | grep 'Global attribute ' > global_atts.$$
#nlines=$(cat global_atts.$$ | wc -l)
#for (( l = 1; l <= $nlines; l++ )); do
#  glatt=$(cat global_atts.$$ | head -$l | tail -1)
#  name=$(echo ${glatt} | cut -f4 -d' ' | cut -f 1 -d,)
#  type=$(echo ${glatt} | cut -f8 -d' ' | cut -c 4 | tr "A-Z" "a-z")
#  value=$(echo ${glatt} | cut -f3 -d= )
#  [[ ${value} = "" ]] && value=' '
#  ncatted -h -a ${name},global,a,${type},"${value}" 2d_${restartfile}
#done
#rm global_atts.$$
ncatted -h -a history,global,a,c," $(date): unpack_file.ksh ${restartfile}; " 2d_${restartfile}
echo "All done with ${script}: Output is $(pwd)/2d_${restartfile}"
