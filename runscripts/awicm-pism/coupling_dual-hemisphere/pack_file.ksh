#!/bin/ksh
#------------------------------------------------------------------------------
# Script to re-pack all variables of an unpacked jsbach restart file.
#    to be used in combination with unpack_file.ksh to e.g. modify the some 
#    arrays of the restart file
#
# Veronika Gayler, August  2014
#------------------------------------------------------------------------------
set -e

restart=$1                 # file with unpacked (lon/lat) data
maskfile=$2                # file with corresponding land sea mask 
#maskfile=/pool/data/JSBACH/T63/jsbach_T63GR15_11tiles_1850.nc
maskname=slm               # variable name of the land sea mask in maskfile 

#------------------------------------------------------------------------------
. ${MODULESHOME}/init/ksh
module load nco || true
module unload cdo
if [[ $(uname -n | cut -c1-6) == mlogin || $(uname -n | cut -c1-9) == mistralpp || $(uname -n) == m????? ]]; then
    module load cdo/1.7.2-magicsxx-gcc48
    export LD_LIBRARY_PATH="/sw/rhel6-x64/hdf5/hdf5-1.8.14-threadsafe-gcc48/lib:/sw/rhel6-x64/netcdf/netcdf_c-4.4.0-gcc48/lib:/sw/rhel6-x64/sys/libaec-0.3.2-gcc48/lib"
else
    #module load cdo/1.7.2-gccsys
    module load cdo/1.7.2
fi
cdo="cdo -b 64 -s"
#------------------------------------------------------------------------------

if [[ ${restart} = "" ]]; then
  echo " ERROR: "
  echo " you need to define the name of the file with unpacked data"
  echo " and a file containing the corresponding land sea mask"
  exit 1
fi

restartfile=${restart##*/}
[[ ${restartfile} = ${restart} ]] && restart=./${restart}
restartdir=${restart%/*}; cd ${restartdir}; restartdir=$(pwd) 
scriptdir=${0%/*}; cd ${scriptdir}/..; scriptdir=$(pwd)
script=$(basename $0)

if [[ ! -f ./pack ]]; then
 echo "  ERROR: "
 echo "   The program 'pack' is missing."
 echo "   Compile <cosmos>/contrib/pack.f90 and place the executable in this "
 echo "   directory. Instructions are given in the program header."
 exit 1
fi

# data processing
#-----------------

cd ${restartdir}
[[ -f 1d_${restartfile} ]] && rm 1d_${restartfile}
${cdo} -selvar,${maskname} ${maskfile} ${maskname}.nc
 
# start restart file with (2d) land sea mask; -h: suppress history

ncks -A -h --no_abc -v ${maskname} ${maskname}.nc 1d_${restartfile}
ncrename -h -v ${maskname},'landseamask' 1d_${restartfile}       1> /dev/null

# 'cdo showvar' does not work for variables with more than three dimensiones
variables=$(ncdump -h ${restartfile} | grep 'double ' | cut -f 2 -d ' ' | cut -f1 -d '(')

for var in ${variables}; do
    {
    # extract specific variable; -a: suppress alphabetical order of variables and dimensions
    ncks -O --no_abc -v ${var} ${restartfile} ${var}.nc
    case ${var} in
	lon | lat )
	    echo "${script}: ignoring ${var}: not needed in restart file"
	    ;;
	landseamask | disch | awfre )
	    echo "${script}: ${var} should not be packed"
	    cp ${var}.nc 1d_${var}.nc
	    ;;
	vct_a | vct_b )
	    echo "${script}: ${var} cannot be packed"
	    cp ${var}.nc 1d_${var}.nc
	    ;;
	* )
	    echo "${script}: ${var}"
	    ${scriptdir}/pack <<EOF
                ${var}
                ${maskname}
EOF
	    ;;
    esac
    } &
done
wait

echo "${script}: Finished variable loop"

# merge packed variables into one file; -h: suppress history
for var in ${variables}; do
    if [[ -f 1d_${var}.nc ]]; then
	ncks -A -h -v ${var} 1d_${var}.nc 1d_${restartfile}
	rm ${var}.nc 1d_${var}.nc
    fi
done

echo "${script}: Finished merging"

#set -x
# add global attributes
#ncks -M ${restartfile} | grep 'Global attribute ' > global_atts.$$
#nlines=$(cat global_atts.$$ | wc -l)
#for (( l = 1; l <= $nlines; l++ )); do
#  glatt=$(cat global_atts.$$ | head -$l | tail -1)
#  name=$(echo ${glatt} | cut -f4 -d' ' | cut -f 1 -d,)
#  type=$(echo ${glatt} | cut -f8 -d' ' | cut -c 4 | tr "A-Z" "a-z")
#  value=$(echo ${glatt} | cut -f3 -d= )
#  [[ ${value} = "" ]] && value=' '
# ncatted -h -a ${name},global,a,${type},"${value}" 1d_${restartfile}
#done
#rm global_atts.$$
#ncatted -h -a history,global,a,c," $(date): pack_file.ksh ${restartfile}; " 1d_${restartfile}

