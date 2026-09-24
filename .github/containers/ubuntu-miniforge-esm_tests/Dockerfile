# Built and pushed to ghcr.io/esm-tools/esm_tools by
# .github/workflows/build-container.yml.

# Use Ubuntu 25 as the base image
FROM ubuntu:25.04

# Set non-interactive mode for apt
ENV DEBIAN_FRONTEND=noninteractive

# Update the system and install essential tools
#
# tzdata: pyarrow's temporal COMPUTE kernels (pc.strftime, used by
# stac_geoparquet when serializing timestamps back to STAC JSON) resolve
# timezones through Arrow's own discover_tz_dir(), which searches a fixed
# list of standard OS paths (/usr/share/zoneinfo chief among them) and does
# NOT consult TZDIR or any conda-installed copy for this specific lookup --
# verified directly: pointing TZDIR at either conda location had zero effect,
# identical "Cannot locate timezone 'UTC': discover_tz_dir failed to find
# zoneinfo" either way, while array construction/cast/to_pylist (a different,
# more lenient Arrow code path) worked fine regardless. Only real system
# tzdata at one of those hardcoded paths satisfies it. See below for the
# conda-side TZDIR, which is unrelated and kept for other consumers.
RUN apt-get update && apt-get install -y --no-install-recommends \
    wget \
    curl \
    locales \
    libsqlite3-dev \
    ca-certificates \
    bzip2 \
    git \
    tzdata && \
    rm -rf /var/lib/apt/lists/*

RUN locale-gen en_US.UTF-8 && \
    update-locale LANG=en_US.UTF-8

# Set environment variables for Miniforge installation
ENV MINIFORGE_VERSION=22.11.1-4
ENV PATH=/opt/conda/bin:$PATH

# Download and install Miniforge
RUN curl -L -O "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-$(uname)-$(uname -m).sh" && \
    bash Miniforge3-$(uname)-$(uname -m).sh -b -p /opt/conda && \
    rm Miniforge3-$(uname)-$(uname -m).sh && \
    conda init bash
#RUN wget --quiet https://github.com/conda-forge/miniforge/releases/download/$MINIFORGE_VERSION/Miniforge3-Linux-x86_64.sh -O /tmp/miniforge.sh && \
#    bash /tmp/miniforge.sh -b -p /opt/conda && \
#    rm /tmp/miniforge.sh && \
#    conda init bash

# Ensure that the shell runs with Conda initialized
SHELL ["/bin/bash", "-c"]

# Optionally, create a new Conda environment (uncomment to use)
RUN mamba create -n esm_tools python=3.9 setuptools=65.5.1

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Set working directory inside the container
WORKDIR /esm

# catalog-grib needs the ecCodes C library at runtime; cfgrib/eccodes (the pip
# packages) only bind to it. conda-forge bundles the compiled library, which
# is far more reliable here than pip alone -- see setup.py's own comment on
# the extra. Everything else (catalog + esm_tools itself) is plain pip.
#
# conda-forge's tzdata (installed below) covers the plain array
# construction/cast/to_pylist code paths, which DO respect TZDIR -- but not
# the temporal compute kernels (pc.strftime and friends), which need the
# real system tzdata installed above instead. Kept for completeness/in case
# some other consumer in this env does read TZDIR; not load-bearing for the
# geoparquet push failure this was originally added for -- see the tzdata
# apt-get comment above for the actual fix and how that was verified.
COPY . .
RUN mamba install -n esm_tools -y -c conda-forge eccodes tzdata && \
    mamba run -n esm_tools pip install --no-cache-dir -e ".[catalog,catalog-grib]"
ENV TZDIR=/opt/conda/envs/esm_tools/share/zoneinfo

# Use the Conda environment by default (uncomment if using an environment)
RUN echo "conda activate esm_tools" >> ~/.bashrc

CMD ["/bin/bash"]
