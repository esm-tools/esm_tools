#!/bin/bash
# ESM-Tools Data Services - HPC Deployment Script
# Runs STAC API and Visualization services via Apptainer
#
# Usage:
#   ./hpc-deploy.sh start    # Start both services
#   ./hpc-deploy.sh stop     # Stop both services
#   ./hpc-deploy.sh status   # Check service status
#   ./hpc-deploy.sh logs     # Tail service logs
#   ./hpc-deploy.sh build    # Build container images

set -euo pipefail

# Configuration
STAC_API_PORT=${STAC_API_PORT:-23100}
VIZ_PORT=${VIZ_PORT:-23101}
BIND_MOUNTS="/albedo:/albedo:ro"
CATALOG_DB="${CATALOG_DB:-/albedo/work/pgierz/catalog.duckdb}"

# Directories
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMAGE_DIR="${SCRIPT_DIR}/containers"
LOG_DIR="${SCRIPT_DIR}/logs"
PID_DIR="${SCRIPT_DIR}/pids"

# Image names
STAC_API_SIF="${IMAGE_DIR}/esm-catalog.sif"
VIZ_SIF="${IMAGE_DIR}/esm-viz.sif"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

ensure_dirs() {
    mkdir -p "${IMAGE_DIR}" "${LOG_DIR}" "${PID_DIR}"
}

build_images() {
    ensure_dirs
    log_info "Building STAC API container..."
    apptainer build "${STAC_API_SIF}" "docker-daemon://esm-catalog:latest" 2>/dev/null || \
    apptainer build "${STAC_API_SIF}" "${SCRIPT_DIR}/esm_catalog/Dockerfile"

    log_info "Building Viz container..."
    apptainer build "${VIZ_SIF}" "docker-daemon://esm-viz:latest" 2>/dev/null || \
    apptainer build "${VIZ_SIF}" "${SCRIPT_DIR}/esm_viz/Dockerfile"

    log_info "Images built successfully"
}

pull_images() {
    ensure_dirs
    REGISTRY="ghcr.io/esm-tools/esm_tools"
    TAG="${1:-latest}"

    log_info "Pulling STAC API container from registry..."
    apptainer pull "${STAC_API_SIF}" "docker://${REGISTRY}/esm-catalog-api:${TAG}"

    log_info "Pulling Viz container from registry..."
    apptainer pull "${VIZ_SIF}" "docker://${REGISTRY}/esm-viz:${TAG}"

    log_info "Images pulled successfully"
}

start_services() {
    ensure_dirs

    # Check if images exist
    if [[ ! -f "${STAC_API_SIF}" ]]; then
        log_error "STAC API image not found: ${STAC_API_SIF}"
        log_info "Run './hpc-deploy.sh build' first or pull from registry"
        exit 1
    fi

    if [[ ! -f "${VIZ_SIF}" ]]; then
        log_error "Viz image not found: ${VIZ_SIF}"
        log_info "Run './hpc-deploy.sh build' first or pull from registry"
        exit 1
    fi

    # Start STAC API
    if [[ -f "${PID_DIR}/stac-api.pid" ]] && kill -0 "$(cat "${PID_DIR}/stac-api.pid")" 2>/dev/null; then
        log_warn "STAC API already running (PID $(cat "${PID_DIR}/stac-api.pid"))"
    else
        log_info "Starting STAC API on port ${STAC_API_PORT}..."
        apptainer instance start \
            --bind "${BIND_MOUNTS}" \
            --env ESM_CATALOG_PORT="${STAC_API_PORT}" \
            --env ESM_CATALOG_DB="${CATALOG_DB}" \
            "${STAC_API_SIF}" \
            esm-stac-api

        # Run the server inside the instance
        nohup apptainer exec instance://esm-stac-api \
            uvicorn esm_catalog.api.app:app \
            --host 0.0.0.0 --port "${STAC_API_PORT}" \
            > "${LOG_DIR}/stac-api.log" 2>&1 &
        echo $! > "${PID_DIR}/stac-api.pid"
        log_info "STAC API started (PID $!)"
    fi

    # Start Viz Service
    if [[ -f "${PID_DIR}/viz.pid" ]] && kill -0 "$(cat "${PID_DIR}/viz.pid")" 2>/dev/null; then
        log_warn "Viz service already running (PID $(cat "${PID_DIR}/viz.pid"))"
    else
        log_info "Starting Viz service on port ${VIZ_PORT}..."
        apptainer instance start \
            --bind "${BIND_MOUNTS}" \
            --env ESM_VIZ_PORT="${VIZ_PORT}" \
            --env STAC_API_URL="http://localhost:${STAC_API_PORT}" \
            "${VIZ_SIF}" \
            esm-viz

        # Run the server inside the instance
        nohup apptainer exec instance://esm-viz \
            python -m esm_viz.cli serve \
            --host 0.0.0.0 --port "${VIZ_PORT}" \
            > "${LOG_DIR}/viz.log" 2>&1 &
        echo $! > "${PID_DIR}/viz.pid"
        log_info "Viz service started (PID $!)"
    fi

    log_info "Services started:"
    log_info "  STAC API:  http://$(hostname):${STAC_API_PORT}"
    log_info "  Viz:       http://$(hostname):${VIZ_PORT}"
    log_info "  Logs:      ${LOG_DIR}/"
}

stop_services() {
    log_info "Stopping services..."

    # Stop STAC API
    if [[ -f "${PID_DIR}/stac-api.pid" ]]; then
        PID=$(cat "${PID_DIR}/stac-api.pid")
        if kill -0 "${PID}" 2>/dev/null; then
            kill "${PID}" && log_info "Stopped STAC API (PID ${PID})"
        fi
        rm -f "${PID_DIR}/stac-api.pid"
    fi
    apptainer instance stop esm-stac-api 2>/dev/null || true

    # Stop Viz
    if [[ -f "${PID_DIR}/viz.pid" ]]; then
        PID=$(cat "${PID_DIR}/viz.pid")
        if kill -0 "${PID}" 2>/dev/null; then
            kill "${PID}" && log_info "Stopped Viz service (PID ${PID})"
        fi
        rm -f "${PID_DIR}/viz.pid"
    fi
    apptainer instance stop esm-viz 2>/dev/null || true

    log_info "All services stopped"
}

show_status() {
    echo "=== ESM Data Services Status ==="
    echo ""

    # STAC API
    echo -n "STAC API (port ${STAC_API_PORT}): "
    if [[ -f "${PID_DIR}/stac-api.pid" ]] && kill -0 "$(cat "${PID_DIR}/stac-api.pid")" 2>/dev/null; then
        echo -e "${GREEN}RUNNING${NC} (PID $(cat "${PID_DIR}/stac-api.pid"))"
        curl -s "http://localhost:${STAC_API_PORT}/health" 2>/dev/null && echo "" || echo -e "  ${YELLOW}(health check failed)${NC}"
    else
        echo -e "${RED}STOPPED${NC}"
    fi

    # Viz
    echo -n "Viz service (port ${VIZ_PORT}): "
    if [[ -f "${PID_DIR}/viz.pid" ]] && kill -0 "$(cat "${PID_DIR}/viz.pid")" 2>/dev/null; then
        echo -e "${GREEN}RUNNING${NC} (PID $(cat "${PID_DIR}/viz.pid"))"
        curl -s "http://localhost:${VIZ_PORT}/health" 2>/dev/null && echo "" || echo -e "  ${YELLOW}(health check failed)${NC}"
    else
        echo -e "${RED}STOPPED${NC}"
    fi

    echo ""
    echo "Apptainer instances:"
    apptainer instance list 2>/dev/null || echo "  (none)"
}

show_logs() {
    SERVICE="${1:-all}"

    case "${SERVICE}" in
        stac|api)
            tail -f "${LOG_DIR}/stac-api.log"
            ;;
        viz)
            tail -f "${LOG_DIR}/viz.log"
            ;;
        all|*)
            tail -f "${LOG_DIR}"/*.log
            ;;
    esac
}

# Main
case "${1:-help}" in
    start)
        start_services
        ;;
    stop)
        stop_services
        ;;
    restart)
        stop_services
        sleep 2
        start_services
        ;;
    status)
        show_status
        ;;
    logs)
        show_logs "${2:-all}"
        ;;
    build)
        build_images
        ;;
    pull)
        pull_images "${2:-latest}"
        ;;
    help|*)
        echo "ESM-Tools Data Services - HPC Deployment"
        echo ""
        echo "Usage: $0 <command>"
        echo ""
        echo "Commands:"
        echo "  start    Start STAC API and Viz services"
        echo "  stop     Stop all services"
        echo "  restart  Restart all services"
        echo "  status   Show service status"
        echo "  logs     Tail service logs (logs [stac|viz|all])"
        echo "  build    Build Apptainer images from Dockerfiles"
        echo "  pull     Pull images from ghcr.io registry (pull [tag])"
        echo ""
        echo "Environment variables:"
        echo "  STAC_API_PORT  STAC API port (default: 23100)"
        echo "  VIZ_PORT       Viz service port (default: 23101)"
        echo "  CATALOG_DB     Path to DuckDB catalog file"
        echo ""
        echo "Current configuration:"
        echo "  STAC API:  http://localhost:${STAC_API_PORT}"
        echo "  Viz:       http://localhost:${VIZ_PORT}"
        echo "  Bind:      ${BIND_MOUNTS}"
        echo "  Catalog:   ${CATALOG_DB}"
        ;;
esac
