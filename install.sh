#!/usr/bin/env bash

git_error_message="You need git version >= 2.13 to install the esm_tools (see README.rst)."
if hash git 2>/dev/null; then
	git_version=`git --version | rev | cut -d' ' -f 1 | rev`
	major_git_version=`git --version | rev | cut -d' ' -f 1 | rev | cut -d'.' -f 1`
	minor_git_version=`git --version | rev | cut -d' ' -f 1 | rev | cut -d'.' -f 2`
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
    pip install -e .
elif [ ! -z ${CONDA_PREFIX+x} ]; then
    echo "======================="
    echo "Using CONDA environment"
    echo "======================="
    echo "WARNING: The use of a conda environment is currently not recommended. Use only for testing purposes!"
    ${CONDA_PREFIX}/bin/pip install -e .
else
    echo "Standard install to user directory (likely ${HOME}/.local)"
    pip install --user -e .
fi
