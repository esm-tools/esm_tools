#!/bin/bash
# ESM Services Deployment Script for ALBEDO
# Usage: ssh albedo1 'bash -s' < deploy-albedo.sh
#    or: ssh albedo1 'bash /path/to/deploy-albedo.sh'

set -e

# Configuration
CONTAINER_DIR="${ESM_SERVICES_DIR:-$HOME/esm-services/containers}"
LOG_DIR="${ESM_SERVICES_DIR:-$HOME/esm-services}"
API_PORT="${ESM_API_PORT:-23100}"
VIZ_PORT="${ESM_VIZ_PORT:-23101}"
REGISTRY="ghcr.io/esm-tools/esm_tools"
TAG="${ESM_TAG:-esm-tools-plus-simcat-pgierz-workbench}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Load modules
log_info "Loading apptainer module..."
source /etc/profile.d/modules.sh
module load apptainer

# Ensure directories exist
mkdir -p "$CONTAINER_DIR"
mkdir -p "$LOG_DIR"

# Function to stop existing services
stop_services() {
    log_info "Stopping existing services..."

    # Stop apptainer instances
    apptainer instance stop esm-api 2>/dev/null || true
    apptainer instance stop esm-viz 2>/dev/null || true

    # Kill any lingering processes
    pkill -f "uvicorn esm_catalog.api.app" 2>/dev/null || true
    pkill -f "esm_viz.cli serve" 2>/dev/null || true

    sleep 2
    log_info "Services stopped."
}

# Function to pull containers
pull_containers() {
    log_info "Pulling containers (this may take a while)..."

    cd "$CONTAINER_DIR"

    # Use srun for memory allocation on compute node
    log_info "Pulling esm-catalog-api..."
    srun --mem=32G apptainer pull --force esm-catalog-api.sif "docker://${REGISTRY}/esm-catalog-api:${TAG}"

    log_info "Pulling esm-viz..."
    srun --mem=32G apptainer pull --force esm-viz.sif "docker://${REGISTRY}/esm-viz:${TAG}"

    log_info "Containers pulled successfully."
}

# Function to start services
start_services() {
    log_info "Starting services..."

    cd "$CONTAINER_DIR"

    # Start API service
    log_info "Starting API on port $API_PORT..."
    APPTAINERENV_PYTHONNOUSERSITE=1 nohup apptainer exec \
        --cleanenv \
        --bind /tmp:/tmp \
        --bind /albedo:/albedo \
        esm-catalog-api.sif \
        uvicorn esm_catalog.api.app:app --host 0.0.0.0 --port "$API_PORT" \
        > "$LOG_DIR/api.log" 2>&1 &

    sleep 2

    # Start Viz service
    log_info "Starting Viz on port $VIZ_PORT..."
    APPTAINERENV_PYTHONNOUSERSITE=1 nohup apptainer exec \
        --cleanenv \
        --bind /tmp:/tmp \
        --bind /albedo:/albedo \
        esm-viz.sif \
        python -m esm_viz.cli serve --host 0.0.0.0 --port "$VIZ_PORT" \
        > "$LOG_DIR/viz.log" 2>&1 &

    sleep 3
    log_info "Services started."
}

# Function to check service health
check_health() {
    log_info "Checking service health..."

    local api_health=$(curl -s "http://localhost:$API_PORT/health" 2>/dev/null || echo "failed")
    local viz_health=$(curl -s "http://localhost:$VIZ_PORT/health" 2>/dev/null || echo "failed")

    if echo "$api_health" | grep -q "ok"; then
        log_info "API: ${GREEN}healthy${NC}"
    else
        log_error "API: ${RED}unhealthy${NC} - $api_health"
    fi

    if echo "$viz_health" | grep -q "healthy"; then
        log_info "Viz: ${GREEN}healthy${NC}"
    else
        log_error "Viz: ${RED}unhealthy${NC} - $viz_health"
    fi
}

# Function to show logs
show_logs() {
    log_info "Recent API logs:"
    tail -10 "$LOG_DIR/api.log" 2>/dev/null || echo "No API logs found"
    echo ""
    log_info "Recent Viz logs:"
    tail -10 "$LOG_DIR/viz.log" 2>/dev/null || echo "No Viz logs found"
}

# Function to register a catalog
register_catalog() {
    local catalog_path="$1"
    if [ -z "$catalog_path" ]; then
        log_error "Usage: $0 register <catalog_path>"
        return 1
    fi

    log_info "Registering catalog: $catalog_path"
    curl -s -X POST "http://localhost:$API_PORT/catalogs" \
        -H "Content-Type: application/json" \
        -d "{\"path\": \"$catalog_path\"}"
    echo ""
}

# Main command handler
case "${1:-deploy}" in
    deploy)
        stop_services
        start_services
        check_health
        ;;
    pull)
        pull_containers
        ;;
    start)
        start_services
        check_health
        ;;
    stop)
        stop_services
        ;;
    restart)
        stop_services
        start_services
        check_health
        ;;
    status)
        check_health
        ;;
    logs)
        show_logs
        ;;
    register)
        register_catalog "$2"
        ;;
    full)
        stop_services
        pull_containers
        start_services
        check_health
        ;;
    *)
        echo "ESM Services Deployment Script"
        echo ""
        echo "Usage: $0 [command]"
        echo ""
        echo "Commands:"
        echo "  deploy   - Stop existing and start services (default)"
        echo "  pull     - Pull latest container images"
        echo "  start    - Start services"
        echo "  stop     - Stop services"
        echo "  restart  - Restart services"
        echo "  status   - Check service health"
        echo "  logs     - Show recent logs"
        echo "  register - Register a catalog (e.g., $0 register /path/to/catalog.db)"
        echo "  full     - Full deployment: stop, pull, start"
        echo ""
        echo "Environment variables:"
        echo "  ESM_API_PORT     - API port (default: 23100)"
        echo "  ESM_VIZ_PORT     - Viz port (default: 23101)"
        echo "  ESM_TAG          - Container tag (default: esm-tools-plus-simcat-pgierz-workbench)"
        echo "  ESM_SERVICES_DIR - Services directory (default: ~/esm-services)"
        ;;
esac
