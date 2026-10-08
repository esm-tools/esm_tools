# AWI-ESM3 build and run environment (Linux arm64/amd64).
#
# Build:  docker build --platform linux/arm64 -t awiesm3-env:dev .
#
# The model source, esm_tools and the input pool are not part of the image;
# they are bind-mounted at run time.

FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8

# Compilers, MPI, parallel I/O libraries, numerical libraries, build tools,
# and Slurm (esm_runscripts needs a batch system; a single-node Slurm runs
# inside the container). cdo is not packaged for arm64 in Ubuntu 24.04.
RUN apt-get update && apt-get install -y --no-install-recommends \
        build-essential gfortran g++ \
        openmpi-bin libopenmpi-dev \
        libhdf5-openmpi-dev libnetcdf-mpi-dev libpnetcdf-dev \
        libfftw3-dev libopenblas-dev liblapack-dev \
        libaec-dev zlib1g-dev libxml2-dev libcurl4-openssl-dev \
        libeccodes-tools nco \
        cmake make pkg-config autoconf automake libtool m4 bison flex \
        git git-lfs perl python3 python3-venv python3-pip python3-dev \
        slurm-wlm munge \
        ca-certificates curl wget rsync openssh-client \
        ksh less vim-tiny locales sudo \
    && locale-gen en_US.UTF-8 \
    && rm -rf /var/lib/apt/lists/*

# Ubuntu spreads its MPI-enabled HDF5 over multiarch paths that model build
# systems do not expect. Give it one conventional prefix. Ubuntu also names the
# high-level Fortran library libhdf5hl_fortran; XIOS links -lhdf5_hl_fortran.
RUN ARCH=$(gcc -print-multiarch) \
    && mkdir -p /opt/hdf5/lib \
    && ln -s /usr/include/hdf5/openmpi /opt/hdf5/include \
    && ln -s /usr/lib/$ARCH/hdf5/openmpi/* /opt/hdf5/lib/ \
    && ln -s libhdf5hl_fortran.so /opt/hdf5/lib/libhdf5_hl_fortran.so \
    && ln -s libhdf5hl_fortran.a /opt/hdf5/lib/libhdf5_hl_fortran.a

# netCDF-C and netCDF-Fortran with parallel I/O, in one prefix with nc-config
# and nf-config. Ubuntu's netCDF-Fortran is built against the serial netCDF-C,
# and its MPI netCDF-C ships without nc-config, so both are built here against
# the HDF5 above.
ARG NETCDF_C_VERSION=4.9.2
ARG NETCDF_F_VERSION=4.6.1
RUN set -e; mkdir -p /tmp/nc && cd /tmp/nc \
    && curl -fsSL --retry 5 https://downloads.unidata.ucar.edu/netcdf-c/${NETCDF_C_VERSION}/netcdf-c-${NETCDF_C_VERSION}.tar.gz | tar xz \
    && cd netcdf-c-${NETCDF_C_VERSION} \
    && CC=mpicc CPPFLAGS="-I/opt/hdf5/include" LDFLAGS="-L/opt/hdf5/lib -Wl,-rpath,/opt/hdf5/lib" \
       ./configure --prefix=/opt/netcdf --enable-parallel4 \
           --disable-dap --disable-byterange --disable-nczarr --disable-libxml2 \
           --disable-testsets --disable-examples > /tmp/nc/netcdf-c.configure.log 2>&1 \
    && make -j"$(nproc)" > /tmp/nc/netcdf-c.make.log 2>&1 \
    && make install > /tmp/nc/netcdf-c.install.log 2>&1 \
    && cd /tmp/nc \
    && curl -fsSL --retry 5 https://downloads.unidata.ucar.edu/netcdf-fortran/${NETCDF_F_VERSION}/netcdf-fortran-${NETCDF_F_VERSION}.tar.gz | tar xz \
    && cd netcdf-fortran-${NETCDF_F_VERSION} \
    && CC=mpicc FC=mpifort F77=mpifort \
       CPPFLAGS="-I/opt/netcdf/include -I/opt/hdf5/include" \
       LDFLAGS="-L/opt/netcdf/lib -L/opt/hdf5/lib -Wl,-rpath,/opt/netcdf/lib -Wl,-rpath,/opt/hdf5/lib" \
       ./configure --prefix=/opt/netcdf --disable-fortran-type-check > /tmp/nc/netcdf-fortran.configure.log 2>&1 \
    && make -j"$(nproc)" > /tmp/nc/netcdf-fortran.make.log 2>&1 \
    && make install > /tmp/nc/netcdf-fortran.install.log 2>&1 \
    && /opt/netcdf/bin/nc-config --has-parallel4 | grep -q yes \
    && rm -rf /tmp/nc

ENV HDF5_ROOT=/opt/hdf5 \
    NETCDF_ROOT=/opt/netcdf \
    PATH=/opt/netcdf/bin:$PATH \
    LD_LIBRARY_PATH=/opt/netcdf/lib:/opt/hdf5/lib

# Later additions, kept in their own layer so the layers above stay cached.
#   liburi-perl: the FCM build tool shipped with XIOS needs URI.pm
RUN apt-get update && apt-get install -y --no-install-recommends \
        liburi-perl bc file unzip time \
    && rm -rf /var/lib/apt/lists/*

# cdo is not packaged for arm64 in Ubuntu 24.04. ocp-tool and the OpenIFS
# pre/postprocessing call it, so take the conda-forge build into its own prefix.
# The wrapper clears LD_LIBRARY_PATH so cdo uses its own netCDF/HDF5, not /opt.
RUN apt-get update && apt-get install -y --no-install-recommends bzip2 \
    && rm -rf /var/lib/apt/lists/* \
    && case "$(uname -m)" in aarch64) MM=linux-aarch64 ;; *) MM=linux-64 ;; esac \
    && curl -fsSL --retry 5 https://micro.mamba.pm/api/micromamba/$MM/latest | tar -xj -C /usr/local bin/micromamba \
    && for try in 1 2 3 4; do micromamba create -y -p /opt/cdo -c conda-forge cdo && break; sleep 20; done \
    && test -x /opt/cdo/bin/cdo \
    && micromamba clean -a -y \
    && printf '#!/bin/sh\nexec env -u LD_LIBRARY_PATH /opt/cdo/bin/cdo "$@"\n' > /usr/local/bin/cdo \
    && chmod +x /usr/local/bin/cdo \
    && cdo -V 2>&1 | head -1

# gdb, for attaching to a hung or crashing model process. In its own layer after
# cdo so that adding it did not invalidate the cdo layer.
RUN apt-get update && apt-get install -y --no-install-recommends gdb \
    && rm -rf /var/lib/apt/lists/*

# Single-node Slurm; start it with `start-slurm.sh` in a --privileged container.
COPY start-slurm.sh setup-esm-tools.sh /usr/local/bin/

# MPI and Slurm refuse to run as root, so work as a normal user.
RUN useradd -m -s /bin/bash -u 1001 esm \
    && echo "esm ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/esm \
    && mkdir -p /opt/venv && chown esm:esm /opt/venv \
    && git config --system --add safe.directory '*'

# esm_tools lives in a venv on a named volume; see setup-esm-tools.sh.
ENV PATH=/opt/venv/bin:$PATH \
    VIRTUAL_ENV=/opt/venv \
    USER=esm

USER esm
WORKDIR /home/esm
CMD ["/bin/bash"]
