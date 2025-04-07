#!/bin/bash -e
###############################################################################
# Automatic run of TriPyView as a post job of esm-runscripts
# Original code by: Simon Porrmann
# Cleanup and testing by: Paul Gierz
#
# March 2025
# AWI Bremerhaven
###############################################################################

echo "################################################################################"
echo "  _____    _ ___    __   ___                                                    "
echo " |_   _| _(_) _ \_  \ \ / (_)_____ __ __                                        "
echo "   | || '_| |  _/ || \ V /| / -_) V  V /                                        "
echo "   |_||_| |_|_|  \_, |\_/ |_\___|\_/\_/                                         "
echo "                 |__/                                                           "
echo "################################################################################"
echo ""
echo "$(date):: Starting Tripyview Job"
echo ""

outdata_path=$1
start_date=$2
next_date=$3
initial_date=$4
final_date=$5
mesh_dir=$6
base_dir=$7
expid=$8

# Change dates to years
start_year=$(date -d "$start_date" +%Y)
next_year=$(date -d "$next_date" +%Y)
initial_year=$(date -d "$initial_date" +%Y)
final_year=$(date -d "$final_date" +%Y)

output_file="tripyrun_${start_year}.yaml"
target_dir="${base_dir}/${expid}/analysis/Tripyview/config"

export mesh_dir
export outdata_path
export start_year
export next_year
export initial_year
export final_year
export base_dir
export expid
export output_file
export target_dir


echo "$(date):: Configuration Values:"
echo "$(date):: ---------------------"
echo "$(date):: outdata_path=$outdata_path"
echo "$(date):: start_date=$start_date"
echo "$(date):: next_date=$next_date"
echo "$(date):: initial_date=$initial_date"
echo "$(date):: final_date=$final_date"
echo "$(date):: mesh_dir=$mesh_dir"
echo "$(date):: base_dir=$base_dir"
echo "$(date):: expid=$expid"
echo "$(date):: output_file=$output_file"
echo "$(date):: target_dir=$target_dir"

function activate_env() {
    # https://www.shellcheck.net/wiki/SC1091
    # Disable Not following on the source line
    # shellcheck disable=SC1091
    source "$(conda info --base)/etc/profile.d/conda.sh"
    conda activate esm-tools_auto_tripyview
    echo "$(date):: Now in environment ${CONDA_DEFAULT_ENV} (under ${CONDA_PREFIX})"
    echo "$(date):: Prepending PATH (by default, conda activate appends...)"
    export PATH="${CONDA_PREFIX}/bin:${PATH}"
    echo "$(date):: $(which -a python)"
    echo "$(date):: PATH=${PATH}"
}

function check_conda_availability() {
    if ! command -v conda &> /dev/null; then
        echo "$(date):: Conda is not installed. Please install conda or miniconda"
        exit 1
    fi
}

function activate_or_install_tripyview_environment() {
    # https://www.shellcheck.net/wiki/SC1091
    # Disable Not following on the source line
    # shellcheck disable=SC1091
    # Überprüfen, ob das Environment "esm-tools_auto_tripyview" existiert
    if conda info --envs | grep -q "^esm-tools_auto_tripyview "; then
        echo "$(date):: esm-tools_auto_tripyview already exists. Activating now..."
        activate_env
    else
        echo "$(date):: esm-tools_auto_tripyview env DOES NOT exist. Installing..."
        source "$(conda info --base)/etc/profile.d/conda.sh"
        conda create -y --name esm-tools_auto_tripyview python=3.9
        activate_env
        mkdir -p "${base_dir}/${expid}/src"
        cd "${base_dir}/${expid}/src" || exit 1
        git clone https://github.com/fesom/tripyview
        conda install -c conda-forge libstdcxx-ng
        cd tripyview
        pip install -e .
    fi
}

function setup_target_directory() {
    # Create folder for Tripy configs and results in the experiment folder
    if [ ! -d "$target_dir" ]; then
        echo "$(date):: Directory $target_dir not existing. Creating..."
        mkdir -p "$target_dir"
    fi
}

function prepare_output_file() {
    # go to Directory
    # working with envsubst (comes from GNU-gettext bib. Installed on OS and most linux distributions). Creating the Tripyview YAML for the current run.
    cd "${base_dir}/${expid}/scripts/fesom" || exit 1
    envsubst < ESM_auto_tripy.yaml > "$output_file"
    mv -v "$output_file" "$target_dir"
    ls -l "${target_dir}/${output_file}"
    echo "$(date):: Prepared ${output_file} for tripyrun"
}

function execute_tripyrun() {
    # Run the new created Tripy config
    echo "$(date):: Starting tripyrun"
    cd "$target_dir" || exit 1
    # Sanity check:
    echo "$(date):: $(which python)"
    echo "$(date):: $(python -VV)"
    python -c "import tripyview; print(tripyview.__file__); print(tripyview.__version__)"
    # Go!
    tripyrun "$output_file"
}

function main() {
    check_conda_availability
    activate_or_install_tripyview_environment
    setup_target_directory
    prepare_output_file
    execute_tripyrun
}

main



