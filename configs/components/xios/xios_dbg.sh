#!/bin/sh
# Wrapper for the XIOS servers: on SIGBUS or SIGSEGV, glibc's libSegFault writes the
# faulting address, a backtrace and the memory map to work/segfault_xios_<task>_<host>.txt.
# UCX's own handler is disabled because it prints one line and nothing else.
# Zero overhead until a signal is raised.
export UCX_HANDLE_ERRORS=none
export SEGFAULT_SIGNALS="bus segv"
export SEGFAULT_USE_ALTSTACK=1
export SEGFAULT_OUTPUT_NAME="segfault_xios_${SLURM_PROCID:-x}_$(hostname).txt"
export LD_PRELOAD=/lib64/libSegFault.so
exec ./xios.x
