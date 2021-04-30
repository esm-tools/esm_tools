#!/bin/bash

## 
## Usage: ./oifs_change_dataDate_initfiles.sh MACHINE INDIR EXPID YYYY-MM-DDTHH:MM:SS YYYY-MM-DDTHH:MM:SS OUTDIR
## where EXPID is the 4-character experiment ID, e.g. gu5a if the filename is ICMGGgu5aINIT etc.
## YYYYMMDD is the new date you want to set in the grib files
## DIR is the location of the original data. 
##

indir=$1
icmcl_file=$2
inexpid=$3
outexpid=$4
inidate=$5
startdate=$6
enddate=$7
outdir=$8
with_wam=$9
perturb=${10}
nx=${11}
ensemble_id=${12}

style=${13:-"jesus"}

if [[ "$(hostname -f)" =~ ollie ]] ; then
    module purge
    module load intel.compiler
    module load cdo netcdf/4.6.2_intel
    export PATH=/home/ollie/jstreffi/ecmwf/grib_api_intel_hdf5_1.10.2_gnu/bin:$PATH

elif [[ "$(hostname -f)" =~ dkrz.de ]] ; then
    export PATH=/sw/rhel6-x64/grib_api/grib_api-1.15.0-intel14/bin:$PATH
    module purge
    module load netcdf_c/4.3.2-gcc48
    module load cdo

elif [[ "$(hostname -f)" =~ hlrn.de ]] ; then
    #module load eccodes # eccodes only available on blogin :-(
	 #module load cdo
	 module load intel/19.0.5 impi/2019.5
	 export PATH=/home/shkifmsw/sw/HPC_libraries/intel2019.0.5_impi2019.5_20200811/bin:$PATH
elif [[ "$(hostname -f)" =~ juwels ]] ; then
	 
    # new Intel 2019 settings 
    # self compiled netcdf etc from Sebastian Wahl
    module --force purge
    module use $OTHERSTAGES
    module load Stages/Devel-2019a
    #module load Intel/2019.3.199-GCC-8.3.0
    #module load IntelMPI/2019.6.154
    module load Intel/2019.5.281-GCC-8.3.0
    module load ParaStationMPI/5.4.4-1-mt    
    module load Python/3.6.8
    module load imkl/2019.3.199
    #export IO_LIB_ROOT=/p/project/hirace/HPC_libraries/intel2019.3.199_impi2019.6.154_20200703/
    #export IO_LIB_ROOT=/p/project/hirace/HPC_libraries/intel2019.5.281_parastation_5.4.4-1-mt_20201113/
    export IO_LIB_ROOT=/p/project/chhb19/jstreffi/ecmwf/eccodes_intel2020_psi2020/
    export PATH=$IO_LIB_ROOT/bin:$PATH
    export LD_LIBRARY_PATH=$IO_LIB_ROOT/lib:$LD_LIBRARY_PATH
elif [[ "$(hostname)" =~ nesh ]] ; then
	 module load intel/19.0.4 intelmpi/19.0.4 
	 export PATH=/gxfs_home/geomar/smomw235/sw/HPC_libraries/intel2019.0.4_impi2019.0.4_20210122/bin:$PATH
else
     # get rid of all these ifs when using env.sh
     module load PrgEnv-cray/6.0.4
     module load alps pbs
     module load cray-mpich/7.7.3
     module load craype-x86-skylake
     module load cmake/3.14.0
     module load cray-hdf5-parallel/1.10.2.0
     module load cray-netcdf-hdf5parallel/4.6.1.3
     module load cdo/1.9.5
     module load fftw/2.1.5.9
     module load nco/4.9.4
     module load proj4/5.1.0
     module load python/3.9.1
     export PATH=$PATH:/home/awiiccp2/software/ecmwf/eccodes_cce_mpich/bin
fi 

echo " OpenIFS preprocessing "
echo " ===================== "
echo " "
echo " Machine: $(hostname) "
echo " grib_set: $(which grib_set) "
echo " cdo -V: "
cdo -V 
echo " "
echo " Input dir: $indir "
echo " Output dir: $outdir "
echo " InExp ID: $inexpid "
echo " OutExp ID: $outexpid "
echo " Start date: $startdate "
echo " End date: $enddate "
echo " Perturb: $perturb "
echo " Style: $style"

if [[ "x${style}" == "xjesus" ]] ; then
   ndate=$(date -u -d "${inidate}" +%Y%m%d)
else
   ndate=$(date -u -d "${startdate}" +%Y%m%d)
fi
initime=$(date -u -d "${inidate}" +%Y-%m-%dT%T)
starttime=$(date -u -d "${startdate}" +%Y-%m-%dT%T)
endtime=$(date -u -d "${enddate}" +%Y-%m-%dT%T)
echo " New date: $ndate "
echo " Initial time: $initime "
echo " Start time: $starttime "
echo " End time: $endtime "
echo " "

echo " * Change dataDate in files: "
echo " $files "

old=${indir}/ICMGG${inexpid}INIT
new=${outdir}/ICMGG${outexpid}INIT
newgginit=${new}
if [ -f $old ]; then                                                                                                   
    cp $old $old\_ori
    rm $old                                              
    grib_set -s dataDate=$ndate $old\_ori $new
    echo " Made new file: " $new " with date " $ndate                                                                                                                 
else                                                                                                                                                                 
    echo " Could not find file " $old                                                                                                                                 
    exit  
fi 

old=${indir}/ICMGG${inexpid}INIUA
new=${outdir}/ICMGG${outexpid}INIUA
if [ -f $old ]; then
    cp $old $old\_ori
    rm $old  
    grib_set -s dataDate=$ndate $old\_ori $new
    echo " Made new file: " $new " with date " $ndate
else
    echo " Could not find file " $old
    exit
fi

# Modify SKT (skin-temp) field to create ensemble members
if [[ "x${perturb}" == "x1" ]] ; then
    
    echo "          Modifying OpenIFS inital field SKT to generate ensemble member No ${ensemble_id}" 
    
    rm -rf tmp.grb tmp2.grb
    # Generate random numbers on a 1D array of same size as reduced grid
    #echo "cdo -remapnn,r${nx}x1 -divc,10 -subc,0.5 -random,r${nx}x1,${ensemble_id} ${outdir}/random_1d.grb"
    #cdo -O -remapnn,r${nx}x1 -divc,10 -subc,0.5 -random,r${nx}x1,${ensemble_id} ${outdir}/random_1d.grb
    echo "cdo -f nc -remapnn,r${nx}x1 -divc,10 -subc,0.5 -random,r${nx}x1,${ensemble_id} ${outdir}/random_1d.nc"
    cdo -O -f nc -remapnn,r${nx}x1 -divc,10 -subc,0.5 -random,r${nx}x1,${ensemble_id} ${outdir}/random_1d.nc
    # Now take grid description from ICMGG 
    #echo "cdo -setgrid,${newgginit} ${outdir}/random_1d.grb ${outdir}/random.grb "
    #cdo -O -setgrid,${newgginit} ${outdir}/random_1d.grb ${outdir}/random.grb    
    echo "cdo -setgrid,${newgginit} ${outdir}/random_1d.nc ${outdir}/random.nc "
    cdo -O -f grb -setgrid,${newgginit} ${outdir}/random_1d.nc ${outdir}/random.grb
    # Merge with ICMGG
    echo "cdo merge ${outdir}/ICMGG${outexpid}INIT ${outdir}/random.grb ${outdir}/tmp.grb"
    cdo -O merge ${outdir}/ICMGG${outexpid}INIT ${outdir}/random.grb ${outdir}/tmp.grb
    # Add to SKT (var 235)
    echo "cdo expr,'var235=var1+var235;' ${outdir}/tmp.grb ${outdir}/tmp2.grb "
    cdo -O expr,'var235=var1+var235;' ${outdir}/tmp.grb ${outdir}/tmp2.grb
    # Replace values
    echo "cdo replace ${outdir}/ICMGG${outexpid}INIT ${outdir}/tmp2.grb ${outdir}/ICMGG${outexpid}INIT_new"
    cdo -O replace ${outdir}/ICMGG${outexpid}INIT ${outdir}/tmp2.grb ${outdir}/ICMGG${outexpid}INIT_new
    # Control: Save diffs
    cdo -O sub ${outdir}/ICMGG${outexpid}INIT_new ${outdir}/ICMGG${outexpid}INIT ${outdir}/ICMGG${outexpid}INIT_sub
    # Replace file
    echo "mv ${outdir}/ICMGG${outexpid}INIT_new ${outdir}/ICMGG${outexpid}INIT"
    mv ${outdir}/ICMGG${outexpid}INIT_new ${outdir}/ICMGG${outexpid}INIT
    # Control: Make nc file    
    cdo -O -f nc -t ecmwf -setgridtype,regular ${outdir}/ICMGG${outexpid}INIT_sub ${outdir}/ICMGG${outexpid}INIT_sub.nc 
    # Clean up crew
    #rm -f ${outdir}/random_1d.grb ${outdir}/random.grb ${outdir}/tmp.grb ${outdir}/tmp2.grb ${outdir}/ICMGG${outexpid}INIT_sub 
    echo "          Modification of OpenIFS initial files for ensemble complete"
    
    
fi

old=${indir}/ICMSH${inexpid}INIT
new=${outdir}/ICMSH${outexpid}INIT
if [ -f $old ]; then
    cp $old $old\_ori
    rm $old  
    grib_set -s dataDate=$ndate $old\_ori $new
    echo " Made new file: " $new " with date " $ndate
else
    echo " Could not find file " $old
    exit
fi

if [[ "x${with_wam}" == "x1" ]] ; then
    files="cdwavein sfcwindin specwavein uwavein"
    for file in $files ; do

        ## old file
        old=${indir}/$file 
        
        ## new file
        new=${outdir}/$file

        if [ -f $old ]; then
            cp $old $old\_ori
            rm $old 
            ## use grib_set to make new files
            grib_set -s dataDate=$ndate $old\_ori $new
            echo " Made new file: " $new " with date " $ndate
        else
            echo " Could not find file " $old
            exit
        fi
    done
fi

if [[ "x$style" == "xjesus" ]] ; then
    
    echo " * Cut ICMCL file "
    cdo -select,startdate=${starttime},enddate=${endtime} ${icmcl_file} ${outdir}/ICMCL${outexpid}INIT

fi



