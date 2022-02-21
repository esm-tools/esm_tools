#!/bin/bash -e
echo "Welcome to $(esm_tools --version) (containerized)"
case $1 in
	master)
		shift
		esm_master $@
		;;
	runscripts)
		shift
		esm_runscripts $@
		;;
	*)
		echo "UKNOWN ESM TOOLS COMMAND!"
		exit 1
		;;
esac
