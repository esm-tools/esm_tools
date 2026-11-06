# TODO (PG): Change to ghcr hosting at some point in the future.

# Use Ubuntu 24.04 (LTS) as the base image. 25.04 reached its end of life (January
# 2026), its apt repositories are gone and the image cannot be rebuilt
FROM ubuntu:24.04

# Set non-interactive mode for apt
ENV DEBIAN_FRONTEND=noninteractive

# Update the system and install essential tools
RUN apt-get update && apt-get install -y --no-install-recommends \
    wget \
    curl \
    locales \
    libsqlite3-dev \
    ca-certificates \
    bzip2 \
    git && \
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
# Same python and setuptools as ``environment.yaml``. setuptools < 66.1 has a
# ``pkg_resources`` that does not work with Python 3.12, and >= 81 no longer has
# ``pkg_resources`` (imported by esm_tools)
RUN mamba create -n esm_tools python=3.12 setuptools=67.6.1

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Set working directory inside the container
WORKDIR /esm

# Use the Conda environment by default (uncomment if using an environment)
RUN echo "conda activate esm_tools" >> ~/.bashrc

CMD ["/bin/bash"]
