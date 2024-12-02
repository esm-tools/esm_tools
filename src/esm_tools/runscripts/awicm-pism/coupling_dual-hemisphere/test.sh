#!/bin/bash

cd Fortran-v1.0

compiler=$(nc-config --fc)
include=$(nc-config --fflags)
libs=$(nc-config --flibs)

## For ollie, a bad hack: the library -lsz and -lcurl don't
## appear to be found on the compute nodes, we throw them out:
#bad_libs="-lsz -lcurl"
#for bad_lib in ${bad_libs}; do
#        libs="${libs//$bad_lib/}"
#done
echo -e "\t\t- INFO: compiler=$compiler"
echo -e "\t\t- INFO: include=$include"
echo -e "\t\t- INFO: libs=$libs"
echo -e "\t\t- INFO: the compile command will be:"
echo "$compiler $include $libs MOD_PRE.f90 MOD_DATA.f90 MOD_MAIN.f90 MOD_OUTPUT.f90 dEBMmain.f90 -o dEBMmain"
$compiler \
        $include \
        $libs \
        MOD_PRE.f90 \
        MOD_DATA.f90 \
        MOD_MAIN.f90 \
        MOD_OUTPUT.f90 \
        dEBMmain.f90 \
        -o dEBMmain \
        2>> _stdout_stderr
#make comp
#make run
cd ..
