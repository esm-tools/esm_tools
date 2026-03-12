# ESM-Catalog STAC API Server
# Multi-stage build for smaller final image

FROM mambaorg/micromamba:1.5-jammy AS builder

USER root
WORKDIR /app

# Copy only dependency specs first for better layer caching
COPY --chown=$MAMBA_USER:$MAMBA_USER environment.yml /app/

# Create environment with runtime dependencies
RUN micromamba create -n api -f environment.yml && \
    micromamba clean --all --yes

FROM mambaorg/micromamba:1.5-jammy

USER root
WORKDIR /app

# Copy environment from builder
COPY --from=builder /opt/conda/envs/api /opt/conda/envs/api

# Copy application code
COPY --chown=$MAMBA_USER:$MAMBA_USER . /app/esm_catalog/

# Add conda env to PATH and esm_catalog to PYTHONPATH
ENV PATH="/opt/conda/envs/api/bin:$PATH"
ENV PYTHONPATH="/app:$PYTHONPATH"

# Create non-root user for runtime
RUN useradd --create-home --shell /bin/bash catalog && \
    chown -R catalog:catalog /app /opt/conda/envs/api

USER catalog

# Default port (23xxx block = Data & Catalog services)
ENV ESM_CATALOG_PORT=23000
ENV ESM_CATALOG_HOST=0.0.0.0
EXPOSE 23000

# Health check uses the same port variable
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:${ESM_CATALOG_PORT}/health || exit 1

# Usage: docker run -p 23000:23000 esm-catalog-api
#        docker run -e ESM_CATALOG_PORT=9000 -p 9000:9000 esm-catalog-api
CMD uvicorn esm_catalog.api.app:app \
    --host ${ESM_CATALOG_HOST} --port ${ESM_CATALOG_PORT}
