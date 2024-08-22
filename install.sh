#!/usr/bin/env bash

# boolean variable to exit the program on error
shall_exit=false


# prints the error message as the first argument and exits the program with non-zero status
function quit_install () {
    echo ""
    echo "$(tput setaf 1)ERROR: ${1} $(tput sgr 0)"
    echo "please set the LANG and LC_ALL variables in your shell startup script (eg. .bashrc, .bash_profile) to the following values: "
    echo "    export LC_ALL=en_US.UTF-8"
    echo "    export LANG=en_US.UTF-8"
    echo "and re-execute them so that the changes take place. Exiting the installation process"

    exit 1
}


# check if LANG environment is set to the correct value
if [[ -z ${LANG+x} ]]; then
    err_msg="LANG environment variable is not set"
    shall_exit=true
fi


# check if LC_ALL variable is set to the correct value
if [[ -z ${LC_ALL+x} ]]; then
    err_msg="LC_ALL variable is not set"
    shall_exit=true
fi

# we have an error, terminate the script
if [[ ${shall_exit} == true ]]; then
    quit_install "${err_msg}"
fi


# NOTE(PG): Breaks on Apple M2 because they call it "Apple Git"
# git version 2.39.2 (Apple Git-143)
#
# ....because of course they do :-(
#
# Now it does not do that anymore.
git_error_message="You need git version >= 2.13 to install the esm_tools (see README.rst)."
if hash git 2>/dev/null; then
    git_version_number=$(git --version | awk '{print $3}')
    major_git_version=$(echo "$git_version_number" | cut -d. -f1)
    minor_git_version=$(echo "$git_version_number" | cut -d. -f2)
	if test ${major_git_version} -lt "2"; then
		echo $git_error_message
		echo "git version found: ${git_version}"
	else
		if test ${minor_git_version} -lt "10"; then
			echo $git_error_message
			echo "git version found: ${git_version}"
		fi
	fi
else
	echo $git_error_message
	echo "No installed git version found."
fi

# See here: https://tinyurl.com/5b57knvx
if [ ! -z ${VIRTUAL_ENV+x} ]; then
    echo "Detected virtual environment $VIRTUAL_ENV"
    echo "Ensuring an up to date pip"
    python -m pip install --upgrade pip
    pip install -e .
elif [ ! -z ${CONDA_PREFIX+x} ]; then
    echo "======================="
    echo "Using CONDA environment"
    echo "======================="
    echo "WARNING: The use of a conda environment is currently not recommended. Use only for testing purposes!"
    echo "Ensuring an up to date pip"
    python -m pip install --upgrade pip
    ${CONDA_PREFIX}/bin/pip install -e .
else
    echo "Ensuring an up to date pip"
    python -m pip install --upgrade pip
    echo "Standard install to user directory (likely ${HOME}/.local)"
    pip install --user -e .
fi
