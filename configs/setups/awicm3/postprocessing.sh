#!/bin/bash

oifs_outdata_dir=$1
folder_time_stamp=$2
outoifs=$1/$2/

echo "============================"
echo "AWICM3 POSTPROCESSING SCRIPT"
echo "============================"
echo ""
echo "The environment is loaded, for example, here is the value of:"
echo "- NETCDF_CXX_INCLUDE_DIRECTORIES: $NETCDF_CXX_INCLUDE_DIRECTORIES"
echo "Current folder: $(pwd)"
echo ""
echo "This is the list of the OpenIFS files for the current run ($outoifs)"
ls $outoifs
