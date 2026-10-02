#!/bin/bash -e
###############################################################################
# Combine LPJ-GUESS parallel run output files into single .out files
# for ec2cmor2 preprocessing.
#
# Called as an esm_tools subjob after tidy.
# Requires: pip package lpjg2nc2 (provides combine_runs.py)
#
# Feb 2026
# AWI Bremerhaven
###############################################################################

echo "################################################################################"
echo "  LPJ-GUESS combine_runs (ec2cmor2 prep)                                       "
echo "################################################################################"
echo ""
echo "$(date):: Starting LPJ-GUESS output combination"
echo ""

thisrun_work_dir=$1
outdata_dir=$2
run_datestamp=$3
njobs=$4

# Construct paths
input_path="${thisrun_work_dir}"
output_path="${outdata_dir}/${run_datestamp}" # e.g. /.../outdata/lpj_guess/19000101-19001231

echo "$(date):: Configuration:"
echo "$(date)::   input_path  = ${input_path}"
echo "$(date)::   output_path = ${output_path}"
echo "$(date)::   run_datestamp = ${run_datestamp}"
echo "$(date)::   njobs       = ${njobs}"

# Create output directory
mkdir -p "${output_path}"

# Run combine_runs.py with --no-split
echo "$(date):: Running combine_runs.py --no-split"
combine_runs=$(command -v combine_runs)
echo "${combine_runs} ${input_path} --no-split -o ${output_path} -j ${njobs}"
${combine_runs} "${input_path}" --no-split -o "${output_path}" -j "${njobs}"
rc=$?
if [ $rc -eq 0 ]; then
    echo "$(date):: Successfully combined LPJ-GUESS output"
    echo "$(date):: Output in: ${output_path}"
else
    echo "$(date):: ERROR: combine_runs failed with exit code ${rc}"
    exit $rc
fi

# Run lpjg2nc with --remap --nc_rm_ascii
echo "$(date):: Running lpjg2nc --remap --nc_rm_ascii ..."
lpjg2nc=$(command -v lpjg2nc)
echo "${lpjg2nc} -p ${output_path} --remap --nc_rm_ascii -o ${output_path} -j ${njobs}"
${lpjg2nc} -p "${output_path}" --remap --nc_rm_ascii -o "${output_path}" -j "${njobs}"
rc=$?
if [ $rc -eq 0 ]; then
    echo "$(date):: Successfully converted LPJ-GUESS output to nc"
    echo "$(date):: Output in: ${output_path}"
else
    echo "$(date):: ERROR: lpjg2nc failed with exit code ${rc}"
    exit $rc
fi

echo "$(date):: Done"
