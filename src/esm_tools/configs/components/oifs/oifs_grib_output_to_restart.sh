#!/bin/bash
# 
# This script can be used to produce restart conditions from an OpenIFS output file
#
# Author/Contact: Joakim Kjellsson, GEOMAR, jkjellsson@geomar.de
# No support will be provided by the ESM-Tools development team w.r.t this script
# Please contact the author Joakim Kjellsson if you need help using this tool
#
# How to create restart files from OpenIFS output
# Step 1: Restart the model but only run one day and set LXIOS=false in fort.4
# Step 2: Set indir to the workdir where the GRIB output now is
# Step 3: Set the correct EXPID etc below
# Step 4: Run this script
# Step 5: You can now use the produced ICM* files as initial conditions for 
#         OpenIFS to restart the model. 
# Note: This does not produce a true restart, but its pretty darn close...
#
# This script needs ecCodes or grib_api installed
# This works on blogin, but not glogin
# On glogin you can find grib binaries in 
# /sw/dataformats/eccodes/2.25.0/skl/gcc.8.3.0/bin/
module load eccodes

# Where is the output
indir="/scratch/projects/shk00018/focioifs_restarts/FOCI_GJK006/restart/oifs/4000010100/"
# What is the EXPID, i.e. ICMGG<EXPID>INIT 
expid="ECE3"
# this is the date in the file name, e.g. ICMGGECE3+197901 for Jan 1979
indate="400001"
# this is the date you take from the output file 
# to use as initial condition for the next run
# e.g. 19790102
# NOTE: There must only be one time step for this date
indate_cut="40000102"

# Where do you want the resulting files to end up
targetdir="/scratch/projects/shk00018/focioifs_restarts/FOCI_GJK006/restart/oifs/4000010100/"
mkdir -vp $targetdir
# What should be the EXPID of the resulting files, e.g. ICMGG<EXPID>INIT 
expid_tgt="ECE3"

# These files will be used
# Note: The ICMUA file is new in 43r3. Did not exist in 40r1
icmgg_in="${indir}/ICMGG${expid}+${indate}"
icmsh_in="${indir}/ICMSH${expid}+${indate}"
icmua_in="${indir}/ICMUA${expid}+${indate}"

# Create a tmp dir
tmpdir="tmp"
rm -rf ${tmpdir}
mkdir -vp ${tmpdir}

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
echo " Cut step from output "
grib_copy -w dataDate=${indate_cut} ${icmgg_in} ${tmpdir}/ICMGG_tmp
grib_copy -w dataDate=${indate_cut} ${icmsh_in} ${tmpdir}/ICMSH_tmp
grib_copy -w dataDate=${indate_cut} ${icmua_in} ${tmpdir}/ICMUA_tmp

#
# Run grib_filter 
# This will create files like shinit.vo.1 etc
# 
echo " Run grib_filter on SH "
grib_filter ${tmpdir}/gf1 ${tmpdir}/ICMSH_tmp 

echo " Run grib_filter on GG sfc "
grib_filter ${tmpdir}/gf2 ${tmpdir}/ICMGG_tmp

echo " Run grib_filter on GG ua "
grib_filter ${tmpdir}/gf3 ${tmpdir}/ICMUA_tmp

# 
# Now we glue it all together
# (We need to do it in a very specific order)
#

# First lnsp
cp -f ${tmpdir}/shinit.lnsp.hybrid.1 ${tmpdir}/shinit.tmp

# Now vo, d, t
for lev in {1..91}
do
   for var in vo d t
   do
       cat ${tmpdir}/shinit.$var.hybrid.$lev >> ${tmpdir}/shinit.tmp
   done
done

# End with orography
cat ${tmpdir}/shinit.z.hybrid.1 >> ${tmpdir}/shinit.tmp 

# Now surface physics (grid point)
for var in stl1 stl2 stl3 stl4 swvl1 swvl2 swvl3 swvl4 sd src skt ci \
           lmlt lmld lblt ltlt lshf lict licd tsn asn \
           rsn sst istl1 istl2 istl3 istl4 chnk lsm sr al aluvp alnip aluvd alnid \
           lai_lv lai_hv sdfor slt sdor isor anor slor lsrh cvh cvl tvh tvl cl dl
do
    cat ${tmpdir}/gginit.$var >> ${tmpdir}/gginit.tmp
done

# Now q and o3
for lev in {1..91}
do
    for var in q o3
    do
        cat ${tmpdir}/gginiua.$var.hybrid.$lev >> ${tmpdir}/gginiua.tmp 
    done
done

# Then cloud variables
for lev in {1..91}
do
    for var in crwc cswc clwc ciwc cc
    do
        cat ${tmpdir}/gginiua.$var.hybrid.$lev >> ${tmpdir}/gginiua.tmp 
    done
done

#
# Move files 
# 
mv ${tmpdir}/gginit.tmp ${targetdir}/ICMGG${expid_tgt}INIT
mv ${tmpdir}/gginiua.tmp ${targetdir}/ICMGG${expid_tgt}INIUA
mv ${tmpdir}/shinit.tmp ${targetdir}/ICMSH${expid_tgt}INIT
