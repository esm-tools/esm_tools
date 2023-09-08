#!/bin/ksh

thisdir=$(dirname $0)
thisdir=$(readlink -f $thisdir)
old_dir=$(pwd)

rm -rf ${thisdir}/calendar/_build/
mkdir -p ${thisdir}/calendar/_build/

gcc ${thisdir}/calendar/time_difference.c -o ${thisdir}/calendar/_build/time_difference
