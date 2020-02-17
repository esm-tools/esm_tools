#!/usr/bin/env bash

cat << EOF > ~/.esmtoolsrc
FUNCTION_PATH=`pwd`/configs
NAMELIST_PATH=`pwd`/namelists
RUNSCRIPTS_DIR=`pwd`/runscripts
EOF


pip install --user -e . 
