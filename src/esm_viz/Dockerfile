# ESM-Viz: Climate Data Visualization Service
# Multi-stage build for smaller final image

FROM mambaorg/micromamba:1.5-jammy AS builder

USER root
WORKDIR /app

# Install system dependencies for cartopy/GEOS
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgeos-dev \
    libproj-dev \
    proj-data \
    && rm -rf /var/lib/apt/lists/*

# Copy only dependency specs first for better layer caching
COPY --chown=$MAMBA_USER:$MAMBA_USER environment.yml /app/

# Create environment with runtime dependencies
RUN micromamba create -n viz -f environment.yml && \
    micromamba clean --all --yes

FROM mambaorg/micromamba:1.5-jammy

USER root
WORKDIR /app

# Install runtime system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgeos-c1v5 \
    libproj22 \
    proj-data \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Copy environment from builder
COPY --from=builder /opt/conda/envs/viz /opt/conda/envs/viz

# Copy application code
COPY --chown=$MAMBA_USER:$MAMBA_USER . /app/esm_viz/

# Add conda env to PATH and esm_viz to PYTHONPATH
ENV PATH="/opt/conda/envs/viz/bin:$PATH"
ENV PYTHONPATH="/app"

# Create non-root user for runtime
RUN useradd --create-home --shell /bin/bash viz && \
    chown -R viz:viz /app /opt/conda/envs/viz

USER viz

# Default port (23xxx block = Data & Catalog services)
ENV ESM_VIZ_PORT=23001
ENV ESM_VIZ_HOST=0.0.0.0

EXPOSE 23001

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:${ESM_VIZ_PORT}/health || exit 1

# Usage: docker run -p 23001:23001 esm-viz
#        docker run -e ESM_VIZ_PORT=8001 -p 8001:8001 esm-viz
CMD python -m esm_viz.cli serve \
    --host ${ESM_VIZ_HOST} --port ${ESM_VIZ_PORT}
