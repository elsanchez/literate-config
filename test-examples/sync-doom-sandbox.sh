#!/bin/bash
# Sync Doom sandbox packages

set -euo pipefail

SANDBOX_DIR="$HOME/.config/doom-sandbox"

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

log() {
    echo -e "${BLUE}[$(date +'%H:%M:%S')]${NC} $1"
}

success() {
    echo -e "${GREEN}✓${NC} $1"
}

warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

error() {
    echo -e "${RED}✗${NC} $1"
}

log "🔄 Syncing Doom sandbox packages..."

# Check if sandbox exists
if [ ! -d "$SANDBOX_DIR" ]; then
    error "Sandbox directory not found: $SANDBOX_DIR"
    echo "Run create-doom-sandbox.sh first"
    exit 1
fi

# Check if doom command exists
if ! command -v doom &> /dev/null; then
    error "Doom command not found. Please install Doom Emacs first."
    exit 1
fi

log "Setting DOOMDIR to sandbox: $SANDBOX_DIR"
export DOOMDIR="$SANDBOX_DIR"

log "Running doom sync..."
if doom sync; then
    success "Doom sandbox packages synced successfully!"
    echo ""
    echo "🚀 Ready to launch:"
    echo "   ./launch-doom-sandbox.sh"
else
    error "Failed to sync Doom packages"
    echo ""
    echo "💡 Try:"
    echo "   doom doctor    # Check for issues"
    echo "   doom upgrade   # Update Doom"
    exit 1
fi
