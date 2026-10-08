# Ubuntu 24.04 with GCC, Open MPI and a single-node Slurm

A container for building and running coupled setups on a laptop or workstation. It goes with the
machine file `configs/machines/ubuntu_gcc_openmpi.yaml`. So far it has been used for AWI-ESM3
develop at TQ21 with the FESOM PI mesh (runscript
`runscripts/awiesm3/develop/awiesm3-develop-container-TQ21L19-pi_1d.yaml`).

## What is in the image

- Ubuntu 24.04, GCC and gfortran 13, Open MPI 4.1
- MPI-enabled HDF5 from the Ubuntu packages, linked into `/opt/hdf5`
- netCDF-C and netCDF-Fortran built with parallel I/O, in `/opt/netcdf`
- FFTW, OpenBLAS, libaec, the ecCodes tools, nco, cmake, Python 3.12, gdb
- cdo from conda-forge in `/opt/cdo` (Ubuntu 24.04 has no arm64 package)
- Slurm, started as a single node inside the container by `start-slurm.sh`

Model sources, this esm_tools checkout and the input data are not in the image. They are mounted
when the container starts.

## Build

```bash
docker build -t awiesm3-env:dev .
```

Tested on arm64 (Apple Silicon). The Dockerfile has no architecture-specific step, but an x86-64
build has not been tried.

## Use

```bash
./awiesm3-shell.sh                       # interactive shell, Slurm running
./awiesm3-run.sh 'squeue; esm_master'    # one command, non-interactive
```

Both scripts expect these folders next to the esm_tools checkout (or next to this folder, if you
copied it out of esm_tools), and create the last two if missing:

| Folder | In the container | Contents |
|---|---|---|
| the esm_tools checkout | `/home/esm/esm_tools` | installed editable on first start |
| `model_codes/` | `/home/esm/model_codes` | where `esm_master` puts the model source |
| `awiesm3_pool/` | `/pool` | input data, the machine file's `pool_dir` |
| `awiesm3_work/` | `/work` | experiments |

Set `AWIESM3_BASE`, `AWIESM3_POOL` or `AWIESM3_WORK` to use other locations.

The container is started with `--privileged`: Slurm needs a writable cgroup tree and there is no
systemd in the container to provide one. It gets the hostname `awiesm3`, which is how
`configs/machines/all_machines.yaml` maps it to the machine file.

## Things that are easy to trip over

- esm_tools is installed into a venv on the Docker volume `awiesm3-venv` by `setup-esm-tools.sh`.
  On Python 3.12 that needs three workarounds, all explained in the script.
- The Slurm node is sized from what Docker gives the container. Docker Desktop defaults to less
  memory than a full build needs; give it at least 8 GB.
- A job that ends as `COMPLETED` may still have failed: check the model logs.
