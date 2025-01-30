#!/bin/bash
#Home directory and reset of variable
cd ~
source ~/.bash_profile

cd ~

# Function to check for py39 envirionmet. If necessary installing and activating.
#this is just to not trigger errors while activating py39
#module unload netcdf-c
#module unload netcdf-fortran
#Function for activating py39
activate_env() {
    cd ~
    source ~/.bash_profile
    conda activate py39
}

cd /work/ab0995/a270275/runconfig
# Check if conda ist installed
if ! command -v conda &> /dev/null; then
    echo "Conda is not installed. Plesa install conda or miniconda"
    exit 1
fi

# Überprüfen, ob das Environment "py39" existiert
if conda info --envs | grep -q "^py39 "; then
    echo "Py39 is exisiting already. Activating now..."
    activate_env
else
    echo "The Py39 env is not exisiting, installing..."
    cd~
    source ~/.bash_profile
    conda create -y --name py39 python=3.9
    activate_env
fi

#check if yq library is installed if not installing
cd ~
cd /work/ab0995/a270275/runconfig
if ! command -v yq &> /dev/null; then
    echo "Installing yq..."
    pip install yq
fi

#import necessary variables from main config
outdata_path=$1
start_date=$2
next_date=$3
initial_date=$4
final_date=$5
mesh_dir=$6

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


output_file="tripyrun_${start_year}.yaml"

export output_file

#Create folder for Tripy configs and results in the experiment folder

target_dir="$outdata_path/Tripyview/config"
if [ ! -d "$target_dir" ]; then
    echo "Directory $target_dir not existing. Creating..."
    mkdir -p "$target_dir"
fi

# go to Directory
# Verwende envsubst, um den Platzhalter in der YAML-Datei zu ersetzen
envsubst < ESM_auto_tripy.yaml > "$output_file"
mv "$output_file" "$target_dir"
#Run the new createt Tripy config
cd "$target_dir" 
tripyrun "$output_file"



