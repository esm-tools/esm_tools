#!/bin/bash
# 

debug=$1
expid=$2 #"ECE3"
expout=$3 #"ECE3"
indate=$4 #"185001"
outdate=$5 #"185001"
indir=$6 #"./"
outdir=$7 #"./"
machine=$8 #"glogin"

#echo "$indir"

print () { echo "$(date +'%F %T'):" "$@"; }

# Read the command line arguments
#OPTIND=1         # Reset in case getopts has been used previously in the shell.
#while getopts "h:d:e:o:i:u:a:t:m:" opt; do
#while getopts ":hdeoiuatm" opt; do
#    echo "opt $opt"
#    case "$opt" in
#    h|\?)
#         echo " "
#         echo " This script can be used to produce restart conditions from an OpenIFS output file "
#         echo " "
#         echo " Usage: "
#         echo " ./oifs_grib_output_to_restart_new.sh "
#         echo " -h = Displays this message "
#         echo " -d = Activate extra info for debugging "
#         echo " -e = EXP ID (four characters) for input data "
#         echo " -o = EXP ID (four characters) for output data "
#         echo " -i = Input dir "
#         echo " -u = Output dir "
#         echo " -a = Date of input data "
#         echo " -t = Date of output date "
#         echo " -m = Machine you are using "
#         echo " "
#         echo " Author/Contact: Joakim Kjellsson, GEOMAR, jkjellsson@geomar.de "
#         echo " No support will be provided by the ESM-Tools development team w.r.t this script "
#         echo " Please contact the author Joakim Kjellsson if you need help using this tool "
#         echo " "
#         echo " How to create restart files from OpenIFS output "
#         echo " Step 1: Make sure all appropriate variables are written to the output "
#         echo " Step 2: Run this script " 
#         echo " Step 3: Provide the ICMGGINIT, ICMSHINIT, and ICMUAINIT files produced to the new run "
#         echo " Step 4: You can now use the produced ICM* files as initial conditions for "
#         echo "         OpenIFS to restart the model. "
#         echo " Note: This does not produce a true restart, but its pretty darn close... "
#         echo " "
#         exit 0
#        ;;
#    d)  debug=1 # verbose mode
#        echo "debug: $debug"
#        ;;
#    e)  expid=$OPTARG
#        ;;
#    o)  expout=$OPTARG
#        ;;
#    i)  indir="$OPTARG"
#        echo "optarg $OPTARG" 
#        echo "indir $indir"
#        ;;
#    u)  outdir=$OPTARG
#        ;;
#    a)  indate=$OPTARG
#        ;;
#    t)  outdate=$OPTARG
#        ;;
#    m)  machine=$OPTARG
#        ;;
#    esac
#done
#shift $((OPTIND-1))
#[ "$1" = "--" ] && shift

# update vars with command line options if set
#[[ -z $envfile ]] && envfile="$basedir/$EXP_ID/scripts/env.sh"
#export PBS_NP=${ncpus}

#
# This script needs ecCodes or grib_api installed
# This works on blogin, but not glogin
# On glogin you can find grib binaries in 
# /sw/dataformats/eccodes/2.25.0/skl/gcc.8.3.0/bin/

if [[ "x$machine" == "xglogin" ]] ;  then 
   grib_dir="/sw/dataformats/eccodes/2.25.0/skl/gcc.8.3.0/bin/"
   module load cdo
fi

if [[ "x$debug" == "x1" ]] ; then
   echo " Machine: $machine "
   echo " Grib_dir: $grib_dir "
   echo " Exp ID (in): $expid "
   echo " Exp ID (out): $expout "
   echo " In dir: $indir "
   echo " Out dir: $outdir "
   echo " In date: $indate "
   echo " Out date: $outdate "
fi
 
# Locate ecCodes binaries
grib_ls=$grib_dir/grib_ls
grib_copy=$grib_dir/grib_copy
grib_set=$grib_dir/grib_set
grib_filter=$grib_dir/grib_filter

# Where do you want the resulting files to end up
mkdir -vp $outdir/$outdate/

# These files will be used
# Note: The ICMUA file is new in 43r3. Did not exist in 40r1
icmgg_in="${indir}/ICMGG${expid}+${indate}"
icmsh_in="${indir}/ICMSH${expid}+${indate}"
icmua_in="${indir}/ICMUA${expid}+${indate}"

if [[ "x$debug" == "x1" ]] ; then
   echo " Will work on these files: "
   echo $icmgg_in
   echo $icmsh_in
   echo $icmua_in
fi

# Create a tmp dir
tmpdir="tmp"
if [[ "x$debug" == "x1" ]] ; then
   echo " Removing the old tmp dir "
   echo " and making a new one "
fi

rm -rf ${tmpdir}
mkdir -vp ${tmpdir}

# Copy input files
if [[ "x$debug" == "x1" ]] ; then
   echo " Copy input files to tmp dir "
fi
cp -v ${indir}/ICMGG${expid}INIT ${tmpdir}/.
cp -v ${indir}/ICMSH${expid}INIT ${tmpdir}/.

# Create a rules file for grib_filter
# This tells grib_filter to split the GRIB file
# into separate files for each variable (shortName)
# and level
cat > ${tmpdir}/gf1 <<EOF 
write "${tmpdir}/shinit.[shortName].[typeOfLevel].[level]";
EOF

cat > ${tmpdir}/gf2 <<EOF
write "${tmpdir}/gginit.[shortName]";
EOF

cat > ${tmpdir}/gf3 <<EOF
write "${tmpdir}/gginiua.[shortName].[typeOfLevel].[level]";
EOF

#
# Select one day from the output file 
# (in case there are several)
# 
if [[ "x$debug" == "x1" ]] ; then
    echo " Copy input data to tmp dir  "
fi
$grib_copy ${icmgg_in} ${tmpdir}/ICMGG_tmp
$grib_copy ${icmsh_in} ${tmpdir}/ICMSH_tmp
$grib_copy ${icmua_in} ${tmpdir}/ICMUA_tmp

#
# Run grib_filter 
# This will create files like shinit.vo.1 etc
# First we run it on ICMSHINIT and ICMGGINIT to get 
# time invariants such as surface geopotential
# and vegetation indices
# 
if [[ "x$debug" == "x1" ]] ; then
echo " Run grib_filter on ${tmpdir}/ICMSHECE3INIT "
fi
$grib_filter ${tmpdir}/gf1 ${tmpdir}/ICMSHECE3INIT

if [[ "x$debug" == "x1" ]] ; then
echo " Run grib_filter on ${tmpdir}/ICMGGECE3INIT "
fi
$grib_filter ${tmpdir}/gf2 ${tmpdir}/ICMGGECE3INIT

if [[ "x$debug" == "x1" ]] ; then
echo " Run grib_filter on ${tmpdir}/ICMSH_tmp "
fi
$grib_filter ${tmpdir}/gf1 ${tmpdir}/ICMSH_tmp 

if [[ "x$debug" == "x1" ]] ; then
echo " Run grib_filter on ${tmpdir}/ICMGG_tmp "
fi
$grib_filter ${tmpdir}/gf2 ${tmpdir}/ICMGG_tmp

if [[ "x$debug" == "x1" ]] ; then
echo " Run grib_filter on ${tmpdir}/ICMUA_tmp "
fi
$grib_filter ${tmpdir}/gf3 ${tmpdir}/ICMUA_tmp

# 
# Now we glue it all together
# (We need to do it in a very specific order)
#

if [[ "x$debug" == "x1" ]] ; then
echo " First put lnsp on first hybrid level "
fi
cp -f ${tmpdir}/shinit.lnsp.hybrid.1 ${tmpdir}/shinit.tmp

# Now vo, d, t
for lev in {1..91}
do
   for var in vo d t
   do
       if [[ "x$debug" == "x1" ]] ; then
          echo " Put $var on hybrid level $lev "
       fi
       cat ${tmpdir}/shinit.$var.hybrid.$lev >> ${tmpdir}/shinit.tmp
   done
done

# End with orography
if [[ "x$debug" == "x1" ]] ; then
echo " orography is last field in ${tmpdir}/shinit.tmp "
fi
# I think the last should be on surface, not hybrid
#cat ${tmpdir}/shinit.z.hybrid.1 >> ${tmpdir}/shinit.tmp 
cat ${tmpdir}/shinit.z.surface.0 >> ${tmpdir}/shinit.tmp

# Now surface physics (grid point)
for var in stl1 stl2 stl3 stl4 swvl1 swvl2 swvl3 swvl4 sd src skt ci \
           lmlt lmld lblt ltlt lshf lict licd tsn asn \
           rsn sst istl1 istl2 istl3 istl4 chnk lsm sr al aluvp alnip aluvd alnid \
           lai_lv lai_hv sdfor slt sdor isor anor slor lsrh cvh cvl tvh tvl cl dl
do
    if [[ "x$debug" == "x1" ]] ; then
       echo " Put $var in ${tmpdir}/gginit.tmp "
    fi
    cat ${tmpdir}/gginit.$var >> ${tmpdir}/gginit.tmp
done

# Now q and o3
for lev in {1..91}
do
    for var in q o3
    do
        if [[ "x$debug" == "x1" ]] ; then
            echo " Put $var on hybrid level $lev in ${tmpdir}/gginiua.tmp "
        fi
        cat ${tmpdir}/gginiua.$var.hybrid.$lev >> ${tmpdir}/gginiua.tmp 
    done
done

# Then cloud variables
for lev in {1..91}
do
    for var in crwc cswc clwc ciwc cc
    do
        if [[ "x$debug" == "x1" ]] ; then
            echo " Put $var on hybrid level $lev in ${tmpdir}/gginiua.tmp "
        fi
        cat ${tmpdir}/gginiua.$var.hybrid.$lev >> ${tmpdir}/gginiua.tmp 
    done
done

#
# Move files 
# 
if [[ "x$debug" == "x1" ]] ; then
    echo " Rename files and move them "
fi
#mv -v ${tmpdir}/gginit.tmp ${outdir}/${outdate}/ICMGG${expout}INIT
#mv -v ${tmpdir}/gginiua.tmp ${outdir}/${outdate}/ICMGG${expout}INIUA
#mv -v ${tmpdir}/shinit.tmp ${outdir}/${outdate}/ICMSH${expout}INIT
$grib_set -s dataDate=$outdate ${tmpdir}/gginit.tmp ${outdir}/${outdate}/ICMGG${expout}INIT
$grib_set -s dataDate=$outdate ${tmpdir}/gginiua.tmp ${outdir}/${outdate}/ICMGG${expout}INIUA 
$grib_set -s dataDate=$outdate ${tmpdir}/shinit.tmp ${outdir}/${outdate}/ICMSH${expout}INIT

if [[ "x$debug" == "x1" ]] ; then
   echo " Make netcdf files on regular grid of restarts "
fi
cdo -f nc -setgridtype,regular ${outdir}/${outdate}/ICMGG${expout}INIT ${outdir}/${outdate}/ICMGG${expout}INIT.nc
cdo -f nc -setgridtype,regular ${outdir}/${outdate}/ICMGG${expout}INIUA ${outdir}/${outdate}/ICMGG${expout}INIUA.nc
cdo -f nc -sp2gp,cubic ${outdir}/${outdate}/ICMSH${expout}INIT ${outdir}/${outdate}/ICMSH${expout}INIT.nc 


echo " ==== Can you feel that? We are done here... "


