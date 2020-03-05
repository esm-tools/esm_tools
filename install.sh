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
		if test ${minor_git_version} -lt "13"; then
			echo $git_error_message
			echo "git version found: ${git_version}"
		fi
	fi
else
	echo $git_error_message
	echo "No installed git version found."
fi


pip install --user -e .
