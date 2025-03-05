#!/bin/bash
# Function to check for py39 envirionmet. If necessary installing and activating.
#Function for activating py39
#NOTE: Installation of tripyview is not included yet
activate_env() {
    cd ~
    source ~/.bash_profile
    conda activate esm-tools_auto_tripyview
}
if ! command -v conda &> /dev/null; then
    echo "Conda is not installed. Please install conda or miniconda"
    exit 1
fi

# Überprüfen, ob das Environment "esm-tools_auto_tripyview" existiert
if conda info --envs | grep -q "^esm-tools_auto_tripyview "; then
    echo "esm-tools_auto_tripyview is exisiting already. Activating now..."
    activate_env
else
    echo "The esm-tools_auto_tripyview env is not exisiting, installing..."
    cd ~
    source ~/.bash_profile
    conda create -y --name esm-tools_auto_tripyview python=3.9
    activate_env
fi

#import necessary variables from main config
outdata_path=$1
start_date=$2
next_date=$3
initial_date=$4
final_date=$5
mesh_dir=$6
base_dir=$7
expid=$8

#Change dates to years
start_year=$(date -d "$start_date" +%Y)
next_year=$(date -d "$next_date" +%Y)
initial_year=$(date -d "$initial_date" +%Y)
final_year=$(date -d "$final_date" +%Y)
#export necessary variables
export mesh_dir
export outdata_path
export start_year
export next_year
export initial_year
export final_year
export base_dir
export expid

echo "$base_dir"
echo "$expid"

output_file="tripyrun_${start_year}.yaml"

export output_file

#Create folder for Tripy configs and results in the experiment folder

target_dir="${base_dir}/${expid}/analysis/Tripyview/config"
if [ ! -d "$target_dir" ]; then
    echo "Directory $target_dir not existing. Creating..."
    mkdir -p "$target_dir"
fi

# go to Directory
# working with envsubst (comes from GNU-gettext bib. Installed on OS and most linux distributions). Creating the Tripyview YAML for the current run.
cd ${base_dir}/${expid}/config/fesom
envsubst < ESM_auto_tripy.yaml > "$output_file"

mv "$output_file" "$target_dir"
#Run the new created Tripy config
cd "$target_dir" 
tripyrun "$output_file"



